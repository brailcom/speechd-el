;;; speechd-bug.el --- reporting of speechd-el and speechd bugs

;; Copyright (C) 2003 Brailcom, o.p.s.

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

;;

;;; Code:


(eval-when-compile (require 'cl))
(require 'speechd-speak)


;;; Utility functions


(defun speechd-bug--insert (&rest args)
  (goto-char (point-max))
  (apply #'insert args)
  (goto-char (point-max))
  (insert "\n")
  (goto-char (point-max)))

(defun speechd-bug--look-for-file (file directories)
  (let ((found nil))
    (while (and (not found) directories)
      (let ((file-name (concat (car directories) "/" file)))
        (if (file-readable-p file-name)
            (setq found file-name)
          (setq directories (cdr directories)))))
    found))

  
;;; General information insertion


(defun speechd-bug--insert-program-version (program)
  (speechd-bug--insert "Version of `%s':" program)
  (shell-command (format "%s --version | head -1" program) t)
  (speechd-bug--insert "\n"))

(defun speechd-bug--insert-config-file (file directories)
  (speechd-bug--insert "===" file ":begin===")
  (let ((file-name (speechd-bug--look-for-file file directories)))
    (if file-name
        (insert-file-contents file-name)
      (speechd-bug--insert "---not-found---")))
  (speechd-bug--insert "===" file ":end===")
  (speechd-bug--insert "\n"))

(defun speechd-bug--insert-general-info ()
  (dolist (p '("speechd" "festival"))
    (speechd-bug--insert-program-version p))
  (speechd-bug--insert-config-file
   "speechd.conf" '("/etc/speechd" "/etc/speech-dispatcher"))
  (speechd-bug--insert-config-file
   "festival.conf" '("/etc/speechd/modules" "/etc/speech-dispatcher/modules"))
  (speechd-bug--insert-config-file "festival.scm" '("/etc")))


;;; Log insertion


(defun speechd-bug--insert-log-file (file-name)
  (speechd-bug--insert "===" file-name ":logbegin===")
  (shell-command
   (format "sed -n '1,/%s/d ; 0,/%s/p' %s"
           speechd-bug--repro-id speechd-bug--repro-id file-name) t)
  (speechd-bug--insert "===" file-name ":logend===")
  (speechd-bug--insert "\n"))

(defun speechd-bug--insert-logs ()
  ;; speechd
  (let ((file-name (speechd-bug--look-for-file
                    "speechd.conf"
                    '("/etc/speechd" "/etc/speech-dispatcher")))
        (festival-server "localhost")
        (festival-port 1314))
    (when file-name
      (let ((log-files ()))
        (save-excursion
          (find-file file-name)
          (save-match-data
            (goto-char (point-min))
            (when (re-search-forward "^[ \t]*LogFile[ \t]+\"\\(.*\\)\"" nil t)
              (push (match-string 1) log-files))
            (goto-char (point-min))
            (when (re-search-forward
                   "^[ \t]*CustomLogFile[ \t]+\"protocol\"[ \t]+\"\\(.*\\)\""
                   nil t)
              (push (match-string 1) log-files))))
        (dolist (f log-files)
          (speechd-bug--insert-log-file f))))
    ;; Festival
    ;; TODO:
    ))


;;; Reproducing bug


(defvar speechd-bug--finish-repro-key ".")

(defvar speechd-bug--repro-id nil)

(defvar speechd-bug--marker nil)

(defun speechd-bug--generate-repro-id ()
  (let ((time (current-time)))
    (format "speechd-el-%d-%d-%d" (first time) (second time) (third time))))
  
(defun speechd-bug--start-repro ()
  (setq speechd-bug--marker (point-marker))
  (setq speechd-bug--repro-id (speechd-bug--generate-repro-id))
  (speechd-say-sound (concat "_debug_on" speechd-bug--repro-id))
  (define-key speechd-speak-mode-map speechd-bug--finish-repro-key
    'speechd-bug--finish-repro))

(defun speechd-bug--finish-repro ()
  "Finish reproducing a bug."
  (interactive)
  (speechd-say-sound (concat "_debug_off" speechd-bug--repro-id))
  (define-key speechd-speak-mode-map speechd-bug--finish-repro-key 'undefined)
  (speechd-bug--insert-logs)
  (switch-to-buffer (marker-buffer speechd-bug--marker))
  (goto-char (marker-position speechd-bug--marker))
  (setq speechd-bug--marker nil)
  (setq speechd-bug--repro-id nil)
  (message "OK, now describe the bug and send the mail with C-c C-c."))


;;; The command


(defun speechd-bug ()
  "Send a bug report on speechd-el or Speech Dispatcher."
  (interactive)
  (require 'reporter)
  (let ((package (completing-read "Package: " '(("speechd-el" "speechd"))))
        (reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     (format "%s@bugs.freebsoft.org" package)
     package
     '(speechd-speak-version
       speechd--el-version
       speechd-speak--debug
       speechd-connection-parameters
       speechd-face-voices
       speechd-speak-deleted-char
       speechd-speak-buffer-name
       speechd-speak-on-minibuffer-exit
       speechd-speak-auto-speak-buffers
       speechd-speak-force-auto-speak-buffers
       speechd-speak-buffer-insertions
       speechd-speak-insertions-in-buffers
       speechd-speak-priority-insertions-in-buffers
       speechd-speak-align-buffer-insertions
       speechd-speak-movement-on-insertions
       speechd-speak-read-command-keys
       speechd-speak-read-command-name
       speechd-speak-by-properties-on-movement
       speechd-speak-by-properties-always
       speechd-speak-by-properties-never
       speechd-speak-faces
       speechd-speak-whole-line
       speechd-speak-connections
       speechd-speak-signal-events))
    (ignore-errors
      (save-excursion
        (speechd-bug--insert-general-info)))
    (if (y-or-n-p "Can you reproduce the bug now? ")
        (progn
          (message "Reproduce the bug now and finish it with `%s .'"
                   speechd-speak-prefix)
          (speechd-bug--start-repro))
      (message "Please describe the bug as precisely as possible."))))


;;; Announce


(provide 'speechd-bug)


;;; speechd-bug.el ends here
