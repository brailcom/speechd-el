;;; speechd-out.el --- Alternative output interface

;; Copyright (C) 2004, 2005, 2006 Brailcom, o.p.s.

;; Author: Milan Zamazal <pdm@brailcom.org>

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;;; Code:


(eval-when-compile (require 'cl))
(require 'eieio)

(require 'speechd-common)


(defvar speechd-out--drivers '())

(defvar speechd-out-active-drivers '(ssip)
  "List of names of the drivers to send output to.")

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
  (let ((var* (car var)))
    `(dolist (,var* speechd-out--drivers)
       (when (memq (speechd-driver.name ,var*) speechd-out-active-drivers)
         ,@body))))

(defmacro speechd-out--block (driver &rest body)
  `(progn
     (speechd.block-begin ,driver)
     (unwind-protect (progn ,@body)
       (speechd.block-end ,driver))))

(defun speechd-out--loop-drivers-op (operation &rest args)
  (speechd-out--loop-drivers (driver)
    (apply operation driver args)))

(defun speechd-out--icon-name (icon)
  (let ((mapping (cdr (assq icon speechd-out--event-mapping))))
    (when mapping
      (symbol-name mapping))))


;;; Driver API


(defclass speechd-driver ()
  ((name :initform (error "Driver name not given") :initarg :name
         :reader speechd-driver.name)))

(defun speechd-out-register-driver (driver)
  (let ((class (class-of driver)))
    (labels ((replace (list)
               (cond
                ((null list)
                 (list driver))
                ((eq (class-of (car list)) class)
                 (cons driver (cdr list)))
                (t
                 (cons (car list) (replace (cdr list)))))))
      (setq speechd-out--drivers (replace speechd-out--drivers)))))

(defmethod speechd.cancel ((driver speechd-driver)) all)

(defmethod speechd.stop ((driver speechd-driver)) all)

(defmethod speechd.pause ((driver speechd-driver)) all)

(defmethod speechd.resume ((driver speechd-driver)) all)

(defmethod speechd.repeat ((driver speechd-driver)))

(defmethod speechd.block-begin ((driver speechd-driver)))

(defmethod speechd.block-end ((driver speechd-driver)))

(defmethod speechd.text ((driver speechd-driver) text cursor))

(defmethod speechd.icon ((driver speechd-driver) icon))

(defmethod speechd.char ((driver speechd-driver) char))

(defmethod speechd.key ((driver speechd-driver) key))

(defmethod speechd.set ((driver speechd-driver) parameter value))

(defmethod speechd.shutdown ((driver speechd-driver)))

(defvar speechd.update nil)


;;; Interface functions and variables


(defstruct speechd-out-update
  text
  cursor
  group)

(defmacro speechd-out-with-updated-text (spec &rest body)
  `(let ((speechd.update ,spec))
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

(defun* speechd-out-icon (icon &key (priority speechd-default-sound-priority))
  (let ((icon-name (speechd-out--icon-name icon)))
    (when icon-name
      (speechd-out--loop-drivers (driver)
        (speechd.set driver 'priority priority)
        (speechd.icon driver icon-name)))))

(defun* speechd-out-char (char &key (priority speechd-default-char-priority)
                               icon)
  (let ((icon-name (speechd-out--icon-name icon)))
    (speechd-out--loop-drivers (driver)
      (speechd.set driver 'priority priority)
      (speechd-out--block driver
        (when icon-name
          (speechd.icon driver icon-name))
        (speechd.char driver char)))))

(defun* speechd-out-keys (keys &key (priority speechd-default-key-priority)
                               text)
  (speechd-out--loop-drivers (driver)
    (speechd.set driver 'priority priority)
    (speechd-out--block driver
      (dolist (k keys)
        (speechd.key driver k))
      (when text
        (speechd.text driver text nil)))))

(defun* speechd-out-text (text &key (priority speechd-default-text-priority)
                               icon cursor)
  (let ((icon-name (speechd-out--icon-name icon)))
    (speechd-out--loop-drivers (driver)
      (speechd.set driver 'priority priority)
      (speechd-out--block driver
        (when icon-name
          (speechd.icon driver icon-name))
        (speechd.text driver text cursor)))))

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
                                           (set-difference
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
