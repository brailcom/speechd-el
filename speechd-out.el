;;; speechd-out.el --- Alternative output interface  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2004, 2005, 2006, 2008 Brailcom, o.p.s.

;; Author: Milan Zamazal <pdm@brailcom.org>

;; COPYRIGHT NOTICE
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


(require 'cl-lib)
(require 'eieio)

(require 'speechd-common)


(defvar speechd-out--drivers '())

(defcustom speechd-out-active-drivers '(ssip brltty)
  "List of names of the drivers to send output to."
  :type '(repeat symbol)
  :group 'speechd-el)

(defvar speechd-out--event-mapping
  '((empty . empty-text)
    (whitespace . whitespace)
    (beginning-of-line . beginning-of-line)
    (end-of-line . end-of-line)
    (start . start)
    (finish . finish)
    (minibuffer . prompt)
    (message . message)))


;;; Internal infrastructure


(defmacro speechd-out--loop-drivers (var &rest body)
  (let ((var* (car var))
        ($speechd-out-errors (gensym))
        ($error (gensym))
        ($error-data (gensym))
        ($err (gensym)))
    `(let ((,$speechd-out-errors '()))
       (dolist (,var* speechd-out--drivers)
         (when (memq (speechd-driver.name ,var*) speechd-out-active-drivers)
           (condition-case ,$err (progn ,@body)
             (error
              (push ,$err ,$speechd-out-errors)))))
       (when ,$speechd-out-errors
         ;; How to signal all the errors?
         (cl-destructuring-bind
             (,$error . ,$error-data) (cl-first ,$speechd-out-errors)
           (setq ,$speechd-out-errors (cdr ,$speechd-out-errors))
           (while (and ,$speechd-out-errors
                       (eq (caar ,$speechd-out-errors) ,$error))
             (setq ,$error-data (append ,$error-data
                                        (cdar ,$speechd-out-errors)))
             (setq ,$speechd-out-errors (cdr ,$speechd-out-errors)))
           (signal ,$error ,$error-data))))))

(defun speechd-out--loop-drivers-op (operation &rest args)
  (speechd-out--loop-drivers (driver)
    (apply operation driver args)))

(defun speechd-out--icon-name (icon)
  (let ((mapping (cdr (assq icon speechd-out--event-mapping))))
    (when mapping
      (symbol-name mapping))))


;;; Driver API


(defclass speechd-driver ()
  ((name :initarg :name
         :reader speechd-driver.name)))

(cl-defmethod initialize-instance :after
    ((this speechd-driver) _slots)
  (if (not (slot-boundp this 'name))
    (error "Driver name not given")))

(defun speechd-out-register-driver (driver)
  (let ((class (eieio-object-class driver)))
    (cl-labels ((replace (list)
               (cond
                ((null list)
                 (list driver))
                ((eq (eieio-object-class (car list)) class)
                 (cons driver (cdr list)))
                (t
                 (cons (car list) (replace (cdr list)))))))
      (setq speechd-out--drivers (replace speechd-out--drivers)))))

(cl-defmethod speechd.cancel ((_driver speechd-driver) _all))

(cl-defmethod speechd.stop ((_driver speechd-driver) _all))

(cl-defmethod speechd.pause ((_driver speechd-driver) _all))

(cl-defmethod speechd.resume ((_driver speechd-driver) _all))

(cl-defmethod speechd.repeat ((_driver speechd-driver)))

(cl-defmethod speechd.block ((_driver speechd-driver) _function))

(cl-defmethod speechd.text ((_driver speechd-driver) _text _cursor _markers))

(cl-defmethod speechd.icon ((_driver speechd-driver) _icon))

(cl-defmethod speechd.char ((_driver speechd-driver) _char))

(cl-defmethod speechd.key ((_driver speechd-driver) _key))

(cl-defmethod speechd.set ((_driver speechd-driver) _parameter _value))

(cl-defmethod speechd.shutdown ((_driver speechd-driver)))

(defvar speechd-update nil)


;;; Interface functions and variables


(cl-defstruct speechd-out-update
  text
  cursor
  group)

(defmacro speechd-out-with-updated-text (spec &rest body)
  `(let ((speechd-update ,spec))
     ,@body))

(defun speechd-out-cancel (&optional all)
  (interactive "P")
  (speechd-out--loop-drivers-op #'speechd.cancel all))

(defun speechd-out-stop (&optional all)
  (interactive "P")
  (speechd-out--loop-drivers-op #'speechd.stop all))

(defun speechd-out-pause (&optional all)
  (interactive "P")
  (speechd-out--loop-drivers-op #'speechd.pause all))

(defun speechd-out-resume (&optional all)
  (interactive "P")
  (speechd-out--loop-drivers-op #'speechd.resume all))

(defun speechd-out-repeat ()
  (interactive)
  (speechd-out--loop-drivers-op #'speechd.repeat))

(cl-defun speechd-out-icon (icon &key (priority speechd-default-sound-priority))
  (let ((icon-name (speechd-out--icon-name icon)))
    (when icon-name
      (speechd-out--loop-drivers (driver)
        (speechd.set driver 'priority priority)
        (speechd.icon driver icon-name)))))

(cl-defun speechd-out-char (char &key (priority speechd-default-char-priority) icon)
  (let ((icon-name (speechd-out--icon-name icon)))
    (speechd-out--loop-drivers (driver)
      (speechd.set driver 'priority priority)
      (let ((icon-name% icon-name)
            (driver% driver)
            (char% char))
        (speechd.block driver (lambda ()
                                (when icon-name%
                                  (speechd.icon driver% icon-name%))
                                (speechd.char driver% char%)))))))

(cl-defun speechd-out-keys (keys &key (priority speechd-default-key-priority) text)
  (speechd-out--loop-drivers (driver)
    (speechd.set driver 'priority priority)
    (let ((driver% driver)
          (keys% keys)
          (text% text))
      (speechd.block driver (lambda ()
                              (dolist (k keys%)
                                (speechd.key driver% k))
                              (when text%
                                (speechd.text driver% text% nil nil)))))))

(cl-defun speechd-out-text (text &key (priority speechd-default-text-priority) icon cursor markers)
  (let ((icon-name (speechd-out--icon-name icon)))
    (speechd-out--loop-drivers (driver)
      (speechd.set driver 'priority priority)
      (let ((icon-name% icon-name)
            (driver% driver)
            (text% text)
            (cursor% cursor)
            (markers% markers))
        (speechd.block driver (lambda ()
                                (when icon-name%
                                  (speechd.icon driver% icon-name%))
                                (speechd.text driver% text% cursor% markers%)))))))

(defun speechd-out-set (parameter value)
  (speechd-out--loop-drivers (driver)
    (speechd.set driver parameter value)))

(defun speechd-out-shutdown (&optional inactive-only)
  (dolist (driver speechd-out--drivers)
    (when (or (not inactive-only)
              (not (memq (speechd-driver.name driver)
                         speechd-out-active-drivers)))
      (speechd.shutdown driver))))

(defun speechd-out-enable-driver (driver)
  "Enable given driver."
  (interactive
   (list (intern
          (completing-read "Enable driver: "
                           (mapcar 'list
                                   (mapcar 'symbol-name
                                           (cl-set-difference
                                            (mapcar 'speechd-driver.name
                                                    speechd-out--drivers)
                                            speechd-out-active-drivers)))
                           nil t))))
  (unless (memq driver speechd-out-active-drivers)
    (push driver speechd-out-active-drivers)))

(defun speechd-out-disable-driver (driver)
  "Disable given driver and disconnect from its output device."
  (interactive
   (list (intern
          (completing-read "Disable driver: "
                           (mapcar 'list
                                   (mapcar 'symbol-name
                                           speechd-out-active-drivers))
                           nil t))))
  (setq speechd-out-active-drivers (remove driver speechd-out-active-drivers))
  (speechd-out-shutdown t))


;;; Announce

(provide 'speechd-out)


;;; speechd-out.el ends here
