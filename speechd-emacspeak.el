;;; speechd-emacspeak.el --- Make Emacspeak communicate with Speech Daemon

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

;; Hopefully, in the future, most code here will be wiped away by customizing
;; Emacspeak itself.

;;; Code:

(require 'speechd)

;;; Ensure dtk-* variables are set properly, otherwise Emacspeak will complain

(defadvice speechd-open (after speechd-open-advice activate protect)
  (when speechd-connection
    (process-kill-without-query speechd-connection))
  (setq dtk-speaker-process speechd-connection
	dtk-speak-server-initialized t)
  (add-hook 'kill-emacs-hook 'speechd-close))
(defadvice speechd-close (after speechd-close-advice activate protect)
  (setq dtk-speaker-process speechd-connection))

;;; Emacspeak may not start before `dtk-initialize' gets redefined

(defvar speechd-emacspeak-start-allowed-p nil)

(defadvice emacspeak (around emacspeak-speechd-advice preactivate activate)
  (when speechd-emacspeak-start-allowed-p
    ad-do-it))

;;; Load Emacspeak

(setq emacspeak-directory
      (expand-file-name "../" (file-name-directory load-file-name)))
(load "emacspeak-setup.el")

;;; Redefine some Emacspeak functions

(defun dtk-initialize ()
  (speechd-open))

;; Avoid transformation of characters (unfortunately, `dtk-char-to-speech' is
;; an inline function).
(defun emacspeak-speak-char (&optional prefix)
  "Speak character under point.
Pronounces character phonetically unless called with a PREFIX arg."
  (interactive "P")
  (let ((dtk-stop-immediately t)
        (char (following-char)))
    (when char
      (emacspeak-handle-action-at-point)
      (when prefix
	nil)			    ; speechd support for spelling modes needed
      (dtk-letter (char-to-string char))
      (when prefix
	nil)))) 			; ...
(defun emacspeak-speak-this-char (char)
  "Speak this CHAR."
  (let ((dtk-stop-immediately t))
    (when char
      (emacspeak-handle-action-at-point)
      (dtk-letter (char-to-string char)))))

;; Ensure Emacspeak won't throw errors if the process is not running
(defmacro speechd-protect-against-dtk-process-errors (name)
  `(defadvice ,name (around ,(intern (format "%s-speechd-advice" name))
			    preactivate activate)
     (when speechd-connection
       ad-do-it)))
(speechd-protect-against-dtk-process-errors dtk-speak)
(speechd-protect-against-dtk-process-errors dtk-say)
(speechd-protect-against-dtk-process-errors dtk-interp-silence)
(speechd-protect-against-dtk-process-errors dtk-interp-tone)
(speechd-protect-against-dtk-process-errors dtk-interp-notes-initialize)
(speechd-protect-against-dtk-process-errors dtk-interp-notes-shutdown)
(speechd-protect-against-dtk-process-errors dtk-interp-note)
(speechd-protect-against-dtk-process-errors dtk-interp-queue)
(speechd-protect-against-dtk-process-errors dtk-interp-queue-set-rate)
(speechd-protect-against-dtk-process-errors dtk-interp-speak)
(speechd-protect-against-dtk-process-errors dtk-interp-say)
(speechd-protect-against-dtk-process-errors dtk-interp-stop)
(speechd-protect-against-dtk-process-errors dtk-interp-sync)
(speechd-protect-against-dtk-process-errors dtk-interp-letter)
(speechd-protect-against-dtk-process-errors dtk-interp-set-rate)
(speechd-protect-against-dtk-process-errors dtk-interp-set-character-scale)
(speechd-protect-against-dtk-process-errors dtk-interp-toggle-split-caps)
(speechd-protect-against-dtk-process-errors dtk-interp-toggle-capitalization)
(speechd-protect-against-dtk-process-errors dtk-interp-toggle-allcaps-beep)
(speechd-protect-against-dtk-process-errors dtk-interp-set-punctuations)
(speechd-protect-against-dtk-process-errors dtk-interp-pause)
(speechd-protect-against-dtk-process-errors dtk-interp-resume)

;;; Start Emacspeak now

(setq speechd-emacspeak-start-allowed-p t)
(emacspeak)

;;; Announce

(provide 'speechd-emacspeak)

;;; speechd-emacspeak.el ends here
