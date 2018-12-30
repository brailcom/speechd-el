;;; speechd-ssip.el --- SSIP driver

;; Copyright (C) 2004, 2006, 2008 Brailcom, o.p.s.

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


(require 'eieio)

(require 'speechd)
(require 'speechd-out)


(defclass speechd-ssip-driver (speechd-driver)
  ((name :initform 'ssip)
   (host :initform speechd-host :initarg :host)
   (port :initform speechd-port :initarg :port)))
  
(cl-defmethod speechd.cancel ((driver speechd-ssip-driver) all)
  (speechd-cancel all))

(cl-defmethod speechd.stop ((driver speechd-ssip-driver) all)
  (speechd-stop all))

(cl-defmethod speechd.pause ((driver speechd-ssip-driver) all)
  (speechd-pause all))

(cl-defmethod speechd.resume ((driver speechd-ssip-driver) all)
  (speechd-resume all))

(cl-defmethod speechd.repeat ((driver speechd-ssip-driver))
  (speechd-repeat))

(cl-defmethod speechd.block ((driver speechd-ssip-driver) function)
  (speechd-block function))

(cl-defmethod speechd.text ((driver speechd-ssip-driver) text cursor)
  (speechd-say-text text))

(cl-defmethod speechd.icon ((driver speechd-ssip-driver) icon)
  (speechd-say-sound icon))

(cl-defmethod speechd.char ((driver speechd-ssip-driver) char)
  (speechd-say-char char))

(cl-defmethod speechd.key ((driver speechd-ssip-driver) key)
  (speechd-say-key key))

(defconst speechd-ssip--parameter-names (mapcar 'car speechd--parameter-names))
(cl-defmethod speechd.set ((driver speechd-ssip-driver) parameter value)
  (when (eq parameter 'priority)
    (setq parameter 'message-priority))
  (when (memq parameter speechd-ssip--parameter-names)
    (speechd--set-parameter parameter value)))

(cl-defmethod speechd.shutdown ((driver speechd-ssip-driver))
  (speechd-close-all))


(speechd-out-register-driver (make-instance 'speechd-ssip-driver))


;;; Announce

(provide 'speechd-ssip)


;;; speechd-ssip.el ends here
