;;; speechd-emacspeak.el --- Emacspeak backend for Speech Daemon

;; Copyright (C) 2003 Brailcom, o.p.s.

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You can find the GNU General Public License at
;; http://www.gnu.org/copyleft/gpl.html
;; or you can write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a replacement for the file dtk-interp.el from the Emacspeak
;; distribution.  Unfortunately, Emacspeak's Lisp backend is non-modular, so
;; you have to replace its file dtk-interp.el by this file and recompile the
;; whole Emacspeak.

;;; Code:

(require 'speechd)


(defvar speechd-emacspeak-no-stop t)


(defun dtk-interp-silence (duration force)
  ;; unsupported in speechd
  )

(defun dtk-interp-tone (pitch duration &optional force)
  ;; unsupported in speechd
  )

(defun dtk-interp-notes-initialize ()
  ;; unsupported in speechd
  )

(defun dtk-interp-notes-shutdown()
  ;; unsupported in speechd
  )

(defun dtk-interp-note (instrument pitch duration &optional target step force)
  ;; unsupported in speechd
  )

(defun dtk-interp-queue (text)
  (speechd-say text :finish nil))

(defun dtk-interp-queue-set-rate (rate)
  ;; unsupported in speechd
  )

(defun dtk-interp-speak ()
  (speechd-send-data-end))

(defun dtk-interp-say (string)
  (speechd-say string))
(defalias 'dtk-interp-dispatch 'dtk-interp-say)

(defun dtk-interp-stop ()
  (if speechd-emacspeak-no-stop
      (setq speechd-emacspeak-no-stop nil)
    (speechd-stop)))

(defun dtk-interp-sync ()
  (dtk-interp-set-punctuations dtk-punctuation-mode)
  (dtk-interp-toggle-capitalization dtk-capitalize)
  (dtk-interp-toggle-allcaps-beep dtk-allcaps-beep)
  (dtk-interp-toggle-split-caps dtk-split-caps)
  (dtk-interp-set-rate dtk-speech-rate))

(defun dtk-interp-letter (letter)
  (speechd-say-char (string-to-char letter))
  (setq speechd-emacspeak-no-stop t))

(defun dtk-interp-set-rate (rate)
  (speechd-set-rate (if (numberp rate) rate (string-to-number rate))))

(defun dtk-interp-set-character-scale (factor)
  ;; unsupported in speechd
  )

(defun dtk-interp-toggle-split-caps (dtk-split-caps)
  ;; unsupported in speechd
  )

(defun dtk-interp-toggle-capitalization (dtk-capitalize)
  (speechd-set-capitalization (if dtk-capitalize :on :off)))

(defun dtk-interp-toggle-allcaps-beep (dtk-allcaps-beep)
  ;; unsupported in speechd
  )

(defun dtk-interp-set-punctuations (mode)
  (speechd-set-punctuation (cond
			    ((string= mode "none") :none)
			    ((string= mode "some") :some)
			    ((string= mode "all")  :all))))

(defun dtk-interp-reset-state ()
  (ignore-errors
    (speechd-close))
  (speechd-open))

(defun dtk-interp-pause ()
  (speechd-pause))

(defun dtk-interp-resume ()
  (speechd-resume))


;;; speechd.el customizations


(defadvice speechd-send-command (before speechd-emacspeak-send-command-advice
					activate)
  (setq speechd-emacspeak-no-stop nil))


;;; Announce

(provide 'dtk-interp)

;;; dtk-interp.el ends here
