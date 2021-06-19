;;; speechd-ssip.el --- SSIP driver  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2004-2018 Brailcom, o.p.s.

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

(cl-defmethod speechd.cancel ((_driver speechd-ssip-driver) all)
  (speechd-cancel all))

(cl-defmethod speechd.stop ((_driver speechd-ssip-driver) all)
  (speechd-stop all))

(cl-defmethod speechd.pause ((_driver speechd-ssip-driver) all)
  (speechd-pause all))

(cl-defmethod speechd.resume ((_driver speechd-ssip-driver) all)
  (speechd-resume all))

(cl-defmethod speechd.repeat ((_driver speechd-ssip-driver))
  (speechd-repeat))

(cl-defmethod speechd.block ((_driver speechd-ssip-driver) function)
  (speechd-block function))

(cl-defmethod speechd.text ((_driver speechd-ssip-driver) text _cursor markers)
  (speechd-say-text text :markers markers))

(cl-defmethod speechd.icon ((_driver speechd-ssip-driver) icon)
  (speechd-say-sound icon))

(cl-defmethod speechd.char ((_driver speechd-ssip-driver) char)
  (speechd-say-char char))

(cl-defmethod speechd.key ((_driver speechd-ssip-driver) key)
  (speechd-say-key key))

(defconst speechd-ssip--parameter-names (mapcar 'car speechd--parameter-names))
(cl-defmethod speechd.set ((_driver speechd-ssip-driver) parameter value)
  (when (eq parameter 'priority)
    (setq parameter 'message-priority))
  (when (memq parameter speechd-ssip--parameter-names)
    (speechd--set-parameter parameter value)))

(cl-defmethod speechd.shutdown ((_driver speechd-ssip-driver))
  (speechd-close-all))


(speechd-out-register-driver (make-instance 'speechd-ssip-driver))


;;; Announce

(provide 'speechd-ssip)


;;; speechd-ssip.el ends here
