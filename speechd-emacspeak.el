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

;;; Handle auditory icons

(defvar speechd-emacspeak-auditory-icon-mapping
  '((alarm . "bell")
    (alert-user . "bell")
    (ask-question . "bell")
    (ask-short-question . "bell")
    (button . "bell")
    (center . "bell")
    (close-object . "bell")
    (delete-object . "bell")
    (deselect-object . "bell")
    (ellipses . "bell")
    (fill-object . "bell")
    (full . "bell")
    (help . "bell")
    (item . "bell")
    (large-movement . "bell")
    (left . "bell")
    (mark-object . "bell")
    (modified-object . "bell")
    (n-answer . "bell")
    (new-mail . "bell")
    (news . "bell")
    (no-answer . "bell")
    (off . "bell")
    (on . "bell")
    (open-object . "bell")
    (paragraph . "bell")
    (progress . "bell")
    (quit . "bell")
    (right . "bell")
    (save-object . "bell")
    (scroll . "bell")
    (search-hit . "bell")
    (search-miss . "bell")
    (section . "bell")
    (select-object . "bell")
    (shutdown . "bell")
    (task-done . "bell")
    (unmodified-object . "bell")
    (warn-user . "warning")
    (window-resize . "bell")
    (y-answer . "bell")
    (yank-object . "bell")
    (yes-answer . "bell"))
  "Mapping of Emacspeak auditory icons to speechd auditory icons.
Alist with elements of the form (EMACSPEAK-NAME . SPEECHD-STRING).")

(defvar speechd-emacspeak-default-auditory-icon "click"
  "Name of the speechd auditory icon to play by default.
The icon is played when no corresponding entry is found in
`speechd-emacspeak-auditory-icon-mapping' for a requested icon.")

(defun speechd-emacspeak-auditory-icon (icon)
  (speechd-say-sound (or (cdr (assoc icon
				     speechd-emacspeak-auditory-icon-mapping))
			 speechd-emacspeak-default-auditory-icon)))
(setq emacspeak-auditory-icon-function 'speechd-emacspeak-auditory-icon)
(setq emacspeak-use-auditory-icons t)

;;; Ensure dtk-* variables are set properly, otherwise Emacspeak will complain

(setq dtk-speak-server-initialized t)

;;; Emacspeak may not start before `dtk-initialize' gets redefined

(defvar speechd-emacspeak-start-allowed-p nil)

(defadvice emacspeak (around emacspeak-speechd-advice preactivate activate)
  (when speechd-emacspeak-start-allowed-p
    ad-do-it))

;;; Load Emacspeak

(setq emacspeak-directory
      (expand-file-name "../" (file-name-directory load-file-name)))
(setq emacspeak-unibyte nil)
(setq dtk-speech-rate-base -99
      dtk-speech-rate-step 22
      dtk-speech-rate 0)
(load "emacspeak-setup.el")

;;; Redefine some Emacspeak functions

(defun dtk-initialize ()
  nil)

(defun tts-restart ()
  "Use this to nuke the currently running TTS server and restart it."
  (interactive)
  (speechd-reopen))

(defadvice comint-output-filter (around emacspeak activate)
  "Make comint speak its output."
  (save-excursion
    (speechd-protect
     (set-buffer (process-buffer (ad-get-arg 0)))
     (let ((prior (point))
	   (monitor emacspeak-comint-output-monitor)
	   (dtk-stop-immediately nil))
       ad-do-it
       (when (and (boundp 'comint-last-prompt-overlay)
		  comint-last-prompt-overlay)
	 (put-text-property (overlay-start comint-last-prompt-overlay)
			    (overlay-end comint-last-prompt-overlay)
			    'personality emacspeak-comint-prompt-personality))
       (when (and emacspeak-comint-autospeak
		  (or monitor 
		      (eq (selected-window)
			  (get-buffer-window
			   (process-buffer (ad-get-arg 0))))))
	 (when emacspeak-comint-split-speech-on-newline
	   (modify-syntax-entry 10 ">"))
	 (condition-case nil
	     (emacspeak-speak-region prior (point ))
	   (error (emacspeak-auditory-icon 'scroll)
		  (dtk-stop ))))
       ad-return-value))))

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
     (when (speechd-running-p)
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

(define-key emacspeak-keymap "s" 'speechd-stop)	; stop, not cancel
(define-key emacspeak-keymap "p" 'speechd-pause) ; not dtk-pause
(define-key emacspeak-keymap " " 'speechd-resume) ; not dtk-resume

;;; Disable voice handling (can't work the Emacspeak way; has to be solved in a
;;; better way in future)

(clrhash dtk-voice-table)
(setq dtk-family-table nil
      dtk-default-voice-string ""
      tts-voice-reset-code "")
;; TODO: Just a hack, fix it!
(voice-lock-mode -1)
(defun voice-lock-mode (&optional arg)
  (setq voice-lock-mode nil))

;;; Disable other Emacspeak features

(require 'emacspeak-advice)
(defadvice message (around emacspeak pre act)
  ad-do-it
  (setq emacspeak-last-message ad-return-value)
  (when (and emacspeak-speak-messages
	     ad-return-value)
    (tts-with-punctuations "all"
      (speechd-say-text ad-return-value :priority :progress))))

;;; Start Emacspeak now

(setq speechd-emacspeak-start-allowed-p t)
(emacspeak)

;;; Announce

(provide 'speechd-emacspeak)

;;; speechd-emacspeak.el ends here
