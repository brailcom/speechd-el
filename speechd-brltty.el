;;; speechd-brltty.el --- BrlTTY output driver

;; Copyright (C) 2004 Brailcom, o.p.s.

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


(defun speechd-brltty--create-manager ()
  (let ((manager (speechd-braille--create-manager)))
    (mmanager-put manager 'braille-display #'brltty-write)
    manager))


(defclass speechd-brltty-driver (speechd-braille-emu-driver)
  ((name :initform 'brltty)
   (manager :initform (lambda () (speechd-brltty--create-manager)))
   (brltty-connection :initform (lambda () (brltty-open)))))

(defmethod speechd-braille--make-message
    ((driver speechd-braille-emu-driver) text message)
  (list (slot-value driver 'brltty-connection) text message))

(defmethod speechd.shutdown ((driver speechd-brltty-driver))
  (brltty-close (slot-value driver 'brltty-connection)))


(speechd-out-register-driver (make-instance 'speechd-brltty-driver))


;;; Announce

(provide 'speechd-brltty)


;;; speechd-brltty.el ends here
