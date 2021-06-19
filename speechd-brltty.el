;;; speechd-brltty.el --- BRLTTY output driver  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2004-2008 Brailcom, o.p.s.

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

;;; Commentary:

;;; Code:


(require 'cl-lib)

(require 'brltty)
(require 'mmanager)
(require 'speechd-braille)


;;; User configuration


(defcustom speechd-braille-key-functions
  '((1 . speechd-brltty-previous-message)
    ((0 32 1) . speechd-brltty-previous-message)
    (2 . speechd-brltty-next-message)
    ((0 32 2) . speechd-brltty-next-message)
    (11 . speechd-brltty-last-message)
    ((0 32 25) . speechd-brltty-last-message)
    (12 . speechd-brltty-first-message)
    ((0 32 26) . speechd-brltty-first-message)
    (23 . speechd-brltty-scroll-left)
    ((0 32 23) . speechd-brltty-scroll-left)
    (24 . speechd-brltty-scroll-right)
    ((0 32 24) . speechd-brltty-scroll-right)
    (25 . speechd-brltty-scroll-to-bol)
    ((0 32 27) . speechd-brltty-scroll-to-bol)
    (26 . speechd-brltty-scroll-to-eol)
    ((0 32 28) . speechd-brltty-scroll-to-bol)
    (29 . speechd-brltty-scroll-to-cursor)
    ((0 32 29) . speechd-brltty-scroll-to-cursor)
    (8204 . speechd-brltty-finish-message)
    ((0 32 72) . speechd-brltty-finish-message)
    (8205 . speechd-brltty-cancel)
    ((0 32 71) . speechd-brltty-cancel))
  "Alist of Braille display key codes and corresponding Emacs functions.
If the given key is pressed, the corresponding function is called with a
`speechd-brltty-driver' instance as its single argument.
Please note the functions may be called asynchronously any time.  So they
shouldn't modify current environment in any inappropriate way.  Especially, it
is not recommended to assign or call user commands here."
  :type '(alist :key-type (sexp :tag "Key code") :value-type function)
  :group 'speechd-braille)

(defcustom speechd-braille-show-unknown-keys t
  "If non-nil, show Braille keys not assigned in `speechd-braille-key-functions'."
  :type 'boolean
  :group 'speechd-braille)


;;; Driver utilities


(defun speechd-brltty--create-manager ()
  (let ((manager (speechd-braille--create-manager #'speechd-brltty--display)))
    (mmanager-put manager 'braille-display #'brltty-write)
    manager))

(defvar speechd-brltty--retry-time 1.0)

(defun speechd-brltty--connection (driver &optional dont-open)
  (let ((connection (slot-value driver 'brltty-connection))
        (connection-error nil))
    (when (or (eq connection 'uninitialized)
              (and (not connection)
                   (> (- (float-time) (slot-value driver 'brltty-last-try-time))
                      speechd-brltty--retry-time)))
      (let ((first-time (eq connection 'uninitialized)))
        (if dont-open
            (setq connection nil)
          (let ((driver driver))
            (setq connection (condition-case err
                                 (brltty-open
                                  nil nil
                                  (lambda (key)
                                    (speechd-brltty--handle-key driver key)))
                               (brltty-connection-error
                                (setq connection-error err)
                                nil))))
          (setf (slot-value driver 'brltty-connection) connection)
          (when (and connection-error first-time)
            (signal (car connection-error) (cdr connection-error)))
          (speechd-brltty--ignore-most-keys connection))))
    connection))

(defun speechd-brltty--display (manager message &optional scroll)
  (cl-multiple-value-bind (connection text cursor) message
    (let ((display-width (car (brltty-display-size connection))))
      (when display-width
        (when (and cursor (>= cursor display-width) (not scroll))
          (mmanager-put manager 'scrolling
                        (* (/ cursor display-width) display-width))
          (setq scroll t))
        (if scroll
            (let ((scrolling (mmanager-get manager 'scrolling)))
              (setq text (substring text scrolling))
              (when cursor
                (setq cursor (- cursor scrolling))
                (when (or (< cursor 0) (> cursor display-width))
                  (setq cursor nil))))
          (mmanager-put manager 'scrolling 0)))
      (speechd-braille--display manager (list connection text cursor)))))


;;; Braille key handling


(defun speechd-brltty--handle-key (driver key)
  (let ((function (cdr (assoc key speechd-braille-key-functions))))
    (cond
     (function
      (funcall function driver))
     (speechd-braille-show-unknown-keys
      (message "Braille key pressed: %s" key)))))

(defun speechd-brltty-finish-message (driver)
  "Stop displaying the current message and display the next one."
  (let ((manager (slot-value driver 'manager)))
    (speechd-braille--stop manager)
    (mmanager-next manager)))

(defun speechd-brltty-scroll-left (driver &optional bolp)
  "Scroll towards the beginning of the currently displayed message."
  (let* ((manager (slot-value driver 'manager))
         (scrolling (mmanager-get manager 'scrolling)))
    (when (and scrolling (> scrolling 0))
      (speechd-braille--stop manager)
      (mmanager-put manager 'scrolling
                    (if bolp
                        0
                      (let ((display-size (car (brltty-display-size
                                                (speechd-brltty--connection driver)))))
                        (max (- scrolling (or display-size 0))
                             0))))
      (speechd-brltty--display manager (mmanager-history manager 'current) t))))

(defun speechd-brltty-scroll-to-bol (driver)
  "Scroll to the beginning of the currently displayed message."
  (speechd-brltty-scroll-left driver t))

(defun speechd-brltty-scroll-right (driver &optional eolp)
  "Scroll towards the end of the currently displayed message."
  (let* ((manager (slot-value driver 'manager))
         (scrolling (mmanager-get manager 'scrolling))
         (message (mmanager-history manager 'current)))
    (when scrolling
      (speechd-braille--stop manager)
      (cl-destructuring-bind (connection text _cursor) message
        (let ((display-width (or (car (brltty-display-size connection)) 0)))
          (when display-width
            (setq scrolling (if eolp
                                (max (- (length text) display-width) 0)
                              (+ scrolling display-width))))
          (when (< scrolling (length text))
            (mmanager-put manager 'scrolling scrolling))))
      (speechd-brltty--display manager message t))))

(defun speechd-brltty-scroll-to-eol (driver)
  "Scroll to the end of the currently displayed message."
  (speechd-brltty-scroll-right driver t))

(defmacro speechd-brltty--message-from-history (which)
  `(let* ((manager (slot-value driver 'manager))
          (message (mmanager-history manager ,which)))
     (when message
       (speechd-brltty--display manager message))))

(defun speechd-brltty-scroll-to-cursor (driver)
  "Scroll to the cursor position (if any) in the displayed message."
  (speechd-brltty--message-from-history 'current))

(defun speechd-brltty-previous-message (driver)
  "Display the previous message from the history."
  (speechd-brltty--message-from-history 'previous))

(defun speechd-brltty-next-message (driver)
  "Display the next message from the history."
  (speechd-brltty--message-from-history 'next))

(defun speechd-brltty-first-message (driver)
  "Display the first message in the history."
  (speechd-brltty--message-from-history 'first))

(defun speechd-brltty-last-message (driver)
  "Display the last message in the history."
  (speechd-brltty--message-from-history 'last))

(defun speechd-brltty-cancel (driver)
  "Discard all messages from the display queue."
  (speechd.cancel driver 'all))

(defun speechd-brltty-command-key (_driver key)
  "Put given key to the command queue."
  (setq unread-command-events (append unread-command-events (list key))))

(defmacro speechd-brltty-command-key-function (key)
  "Insert BRLTTY function handling general character KEY event."
  `(lambda (driver) (speechd-brltty-command-key driver ,key)))

(defun speechd-brltty--ignore-most-keys (connection)
  (brltty-ignore-keys connection)
  (when speechd-braille-key-functions
    (brltty-accept-keys connection
                        (mapcar (lambda (key-spec)
                                  (let ((key (car key-spec)))
                                    (when (numberp key)
                                      (setq key (list 0 0 key)))
                                    key))
                                speechd-braille-key-functions))))


;;; Driver definition, methods and registration


(defclass speechd-brltty-driver (speechd-braille-emu-driver)
  ((name :initform 'brltty)
   (manager)
   (brltty-connection :initform 'uninitialized)
   (brltty-last-try-time :initform 0)))

(cl-defmethod initialize-instance :after
    ((this speechd-brltty-driver) _slots)
  (oset this manager (speechd-brltty--create-manager)))

(cl-defmethod speechd-braille--make-message
    ((driver speechd-brltty-driver) text message)
  (list (speechd-brltty--connection driver) text message))

(cl-defmethod speechd.set ((driver speechd-brltty-driver) parameter value)
  (cond
   ((eq parameter 'brltty-accept-keys)
    (let ((connection (speechd-brltty--connection driver)))
      (when connection
        (funcall (if value 'brltty-accept-keys 'speechd-brltty--ignore-most-keys) connection))))
   (t
    (cl-call-next-method))))

(cl-defmethod speechd.shutdown ((driver speechd-brltty-driver))
  (mmanager-cancel (slot-value driver 'manager) nil)
  (brltty-close (speechd-brltty--connection driver t))
  (setf (slot-value driver 'brltty-connection) 'uninitialized))


(speechd-out-register-driver (make-instance 'speechd-brltty-driver))


;;; Announce


(provide 'speechd-brltty)


;;; speechd-brltty.el ends here
