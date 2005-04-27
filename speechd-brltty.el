;;; speechd-brltty.el --- BrlTTY output driver

;; Copyright (C) 2004, 2005 Brailcom, o.p.s.

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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'cl))

(require 'brltty)
(require 'speechd-braille)


(defgroup speechd-brltty ()
  "speechd-el BrlTTY output driver."
  :group 'speechd-el)

(defcustom speechd-brltty-key-functions '((1 . (lambda (_) (message "Hello!"))))
  "Alist of Braille display key codes and corresponding Emacs functions.
If the given key is pressed, the corresponding function is called with a
`speechd-brltty-driver' instance as its single argument.
Please note the functions may be called asynchronously any time.  So they
shouldn't modify current environment in any inappropriate way.  Especially, it
is not recommended to assign or call user commands here."
  :type '(alist :key-type (integer :tag "Key code") :value-type function)
  :group 'speechd-brltty)

(defcustom speechd-brltty-show-unknown-keys t
  "If non-nil, show Braille keys not assigned in `speechd-brltty-key-functions'."
  :type 'boolean
  :group 'speechd-brltty)


(defun speechd-brltty--create-manager ()
  (let ((manager (speechd-braille--create-manager)))
    (mmanager-put manager 'braille-display #'brltty-write)
    manager))

(defun speechd-brltty--handle-key (driver key)
  (let ((function (cdr (assoc key speechd-brltty-key-functions))))
    (cond
     (function
      (funcall function driver))
     (speechd-brltty-show-unknown-keys
      (message "Braille key pressed: %d" key)))))

(defun speechd-brltty--connection (driver)
  (let ((connection (slot-value driver 'brltty-connection)))
    (when (eq connection 'uninitialized)
      (lexical-let ((driver driver))
        (setq connection (brltty-open
                          nil nil
                          (lambda (key)
                            (speechd-brltty--handle-key driver key)))))
      (setf (slot-value driver 'brltty-connection) connection))
    connection))


(defclass speechd-brltty-driver (speechd-braille-emu-driver)
  ((name :initform 'brltty)
   (manager :initform (lambda () (speechd-brltty--create-manager)))
   (brltty-connection :initform 'uninitialized)))

(defmethod speechd-braille--make-message
    ((driver speechd-braille-emu-driver) text message)
  (list (speechd-brltty--connection driver) text message))

(defmethod speechd.shutdown ((driver speechd-brltty-driver))
  (brltty-close (speechd-brltty--connection driver)))


(speechd-out-register-driver (make-instance 'speechd-brltty-driver))


;;; Announce

(provide 'speechd-brltty)


;;; speechd-brltty.el ends here
