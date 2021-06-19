;;; speechd-bug.el --- reporting speechd-el and speechd bugs  -*- lexical-binding: t -*-

;; Copyright (C) 2012, 2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2003, 2004, 2005 Brailcom, o.p.s.

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


(require 'cl-lib)
(require 'reporter)
(require 'speechd-speak)


(defvar speechd-bug--log-extractor "speechd-log-extractor")

(defvar speechd-bug--finish-repro-key "\C-f")

(defvar speechd-bug-packages
  '(("festival-czech") ("festival-freebsoft-utils") ("sbts")
    ("speech-dispatcher") ("speechd-el") ("unknown")))


(defvar speechd-bug--repro-id nil)

(defvar speechd-bug--marker nil)

(defvar speechd-bug--dribble-file nil)


;;; Utility functions


(defun speechd-bug--ensure-empty-line ()
  (goto-char (point-min))
  (while (not (re-search-forward "\n\n\\'" nil t))
    (goto-char (point-max))
    (insert "\n")
    (goto-char (point-min))))

(defun speechd-bug--insert (&rest args)
  (goto-char (point-max))
  (unless (looking-at "^")
    (insert "\n")
    (goto-char (point-max)))
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
  (speechd-bug--ensure-empty-line)
  (speechd-bug--insert (format "Version of `%s':" program))
  (shell-command (format "%s --version | head -1" program) t))

(defun speechd-bug--insert-config-file (file directories comment-prefix)
  (speechd-bug--ensure-empty-line)
  (speechd-bug--insert "===" file ":begin===")
  (let ((file-name
         (or (speechd-bug--look-for-file file directories)
             (condition-case _c
                 (read-file-name
                  (format "Configuration file `%s' not found, please type its location manually: " file))
               (quit)))))
    (if file-name
        (let ((point (point)))
          (insert-file-contents file-name)
          (flush-lines (format "^[ \t]*\\(%s.*\\)?$" comment-prefix)
                       point (point-max-marker)))
      (speechd-bug--insert "---not-found---")))
  (speechd-bug--insert "===" file ":end==="))

(defun speechd-bug--insert-general-info ()
  (dolist (p '("speech-dispatcher" "festival"))
    (speechd-bug--insert-program-version p))
  (speechd-bug--insert-config-file
   "speechd.conf" '("/etc/speechd" "/etc/speech-dispatcher") "#")
  (speechd-bug--insert-config-file
   "festival.conf" '("/etc/speechd/modules" "/etc/speech-dispatcher/modules")
   "#")
  (speechd-bug--insert-config-file "festival.scm" '("/etc") ";"))

(defun speechd-bug--insert-dribble-file ()
  (speechd-bug--ensure-empty-line)
  (speechd-bug--insert "===dribble:begin===")
  (insert-file-contents speechd-bug--dribble-file)
  (speechd-bug--insert "===dribble:end===")
  (delete-file speechd-bug--dribble-file)
  (setq speechd-bug--dribble-file nil))


;;; Log insertion


(defun speechd-bug--insert-log-file (file-name)
  (speechd-bug--ensure-empty-line)
  (speechd-bug--insert "===" file-name ":logbegin===")
  (shell-command
   (format "%s on%s off%s --compress < %s | uuencode %s.compressed"
           speechd-bug--log-extractor speechd-bug--repro-id
           speechd-bug--repro-id file-name (file-name-nondirectory file-name))
   t)
  (speechd-bug--insert "===" file-name ":logend==="))

(defun speechd-bug--dotconf-option (file-name option)
  (with-current-buffer (find-file-noselect file-name)
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward (concat "^[ \t]*" option "[ \t]+\"\\(.*\\)\"")
                               nil t)
        (match-string 1)))))

(defun speechd-bug--insert-logs ()
  ;; speechd
  (let ((file-name (speechd-bug--look-for-file
                    "speechd.conf"
                    '("/etc/speechd" "/etc/speech-dispatcher"
                      "/usr/local/etc/speechd"))))
    (when file-name
      (dolist (option '("LogFile" "CustomLogFile[ \t]+\"protocol\""))
        (let ((f (speechd-bug--dotconf-option file-name option)))
          (when f
            (save-match-data
              (when (string-match "^/" f)
                (speechd-bug--insert-log-file f)))))))
    ;; Festival
    (let* ((file-name (speechd-bug--look-for-file
                       "festival.conf"
                       '("/etc/speechd/modules"
                         "/etc/speech-dispatcher/modules"
                         "/usr/local/etc/speechd/modules")))
           (festival-server (or (speechd-bug--dotconf-option
                                 file-name "FestivalServerHost")
                                "localhost"))
           (festival-port (string-to-number
                           (or (speechd-bug--dotconf-option
                                file-name "FestivalServerPort")
                               "1314")))
           (festival-module-log (speechd-bug--dotconf-option
                                 file-name "DebugFile")))
      (when festival-module-log
        (speechd-bug--insert-log-file festival-module-log))
      (let ((process (open-network-stream "speechd-festival" nil
                                          festival-server festival-port))
            (output "")
            (log-file nil))
        (when process
          (unwind-protect
              (progn
                (set-process-filter process
                                    #'(lambda (_p str)
                                        (setq output (concat output str))))
                (process-send-string process "server_log_file\n")
                (while output
                  (if (accept-process-output nil 1)
                      (save-match-data
                        (when (string-match "^LP\r?\n\\(.*\\)\n" output)
                          (setq log-file (match-string 1 output))
                          (setq output nil)))
                    (setq output nil))))
            (delete-process process))
          (save-match-data
            (when (and log-file
                       (string-match "\"\\(/.*\\)\"" log-file))
              (speechd-bug--insert-log-file
               (concat (match-string 1 log-file) "-e")))))))))


;;; Reproducing bug


(defun speechd-bug--generate-repro-id ()
  (let ((time (current-time)))
    (format "speechd-el-%d-%d-%d" (cl-first time) (cl-second time) (cl-third time))))

(defun speechd-bug-reproduce ()
  "Start reproducing a speechd-el or Speech Dispatcher bug.
All user and speechd actions are watched from this moment.

Bug reproduction is finished by pressing the `C-e C-f' keys.  After the bug
reproduction is finished, information about it is inserted into the buffer
where the `speechd-bug-reproduce' command was invoked.

This command is useful when you want to provide information about a bug without
generating new bug report."
  (interactive)
  (setq speechd-bug--marker (point-marker))
  (setq speechd-bug--repro-id (speechd-bug--generate-repro-id))
  (speechd-say-sound (concat "_debug_on" speechd-bug--repro-id)
                     :priority 'important)
  (setq speechd-bug--dribble-file (make-temp-file "speechd-bug"))
  (open-dribble-file speechd-bug--dribble-file)
  (define-key speechd-speak-mode-map speechd-bug--finish-repro-key
    'speechd-bug--finish-repro))

(defun speechd-bug--finish-repro ()
  "Finish reproducing a bug."
  (interactive)
  (speechd-say-sound (concat "_debug_off" speechd-bug--repro-id)
                     :priority 'important)
  (define-key speechd-speak-mode-map speechd-bug--finish-repro-key 'undefined)
  (sit-for 1)                           ; wait a little for flushing the logs
  (switch-to-buffer (marker-buffer speechd-bug--marker))
  (let ((speechd-speak-mode nil))
    (speechd-bug--insert-dribble-file)
    (speechd-bug--insert-logs))
  (goto-char (marker-position speechd-bug--marker))
  (setq speechd-bug--marker nil)
  (setq speechd-bug--repro-id nil)
  (message "OK, now describe the bug and send the mail with C-c C-c."))


;;; The command


;;;###autoload
(defun speechd-bug ()
  "Send a bug report on speechd-el or Speech Dispatcher."
  (interactive)
  (let ((package (completing-read "Package: " speechd-bug-packages))
        (reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     "speechd-discuss@nongnu.org"
     package
     (append '(speechd-speak--debug)
             (apropos-internal "^speechd\\(-[a-z]+\\)+$" 'boundp)))
    (ignore-errors
      (save-excursion
        (speechd-bug--insert-general-info)))
    (if (y-or-n-p "Can you reproduce the bug now? ")
        (progn
          (message "Reproduce the bug now and finish it with `%s %s'"
                   (key-description speechd-speak-prefix)
                   (key-description speechd-bug--finish-repro-key))
          (speechd-bug-reproduce))
      (save-excursion
        (speechd-bug--ensure-empty-line)
        (speechd-bug--insert "The bug was not reproduced."))
      (message "Please describe the bug as precisely as possible."))))


;;; Announce


(provide 'speechd-bug)


;;; speechd-bug.el ends here
