;;; speechd.el --- Library for accessing Speech Daemon

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

;; This library allows you to communicate with Speech Daemon.
;; Usually, the communication goes like this:
;; 
;;   (speechd-open)
;;   ...
;;   (speechd-say "Hello, world!")
;;   ...
;;   (speechd-close)

;;; Code:


(eval-when-compile (require 'cl))
(require 'queue-f)


;;; User variables


(defgroup speechd ()
  "Speech Daemon interface.")

(defcustom speechd-host "localhost"
  "*Name of the default host running speechd to connect to."
  :type 'string
  :group 'speechd)

(defcustom speechd-port 9876
  "*Default port of speechd."
  :type 'integer
  :group 'speechd)

(defcustom speechd-timeout 3
  "*Number of seconds to wait for speechd response."
  :type 'integer
  :group 'speechd)

(defconst speechd-priority-tags
  '(radio (const :tag "High"   :value :high)
	  (const :tag "Medium" :value :medium)
	  (const :tag "Low"    :value :low)))

(defcustom speechd-default-text-priority :medium
  "*Default Speech Daemon priority of sent texts."
  :type speechd-priority-tags
  :group 'speechd)

(defcustom speechd-default-sound-priority :medium
  "*Default Speech Daemon priority of sent sound icons."
  :type speechd-priority-tags
  :group 'speechd)

(defcustom speechd-default-char-priority :low
  "*Default Speech Daemon priority of sent single letters."
  :type speechd-priority-tags
  :group 'speechd)

(defcustom speechd-default-key-priority :low
  "*Default Speech Daemon priority of sent symbols of keys."
  :type speechd-priority-tags
  :group 'speechd)

(defcustom speechd-debug nil
  "*If non-nil, be verbose about communication with speechd."
  :type 'boolean
  :group 'speechd)


;;; External variables


(defvar speechd-client-name "default"
  "String defining current client name.")


;;; Internal constants and configuration variables


(defconst speechd--el-version "speechd-el $Id: speechd.el,v 1.11 2003-05-15 17:51:39 pdm Exp $"
  "Version stamp of the source file.
Useful only for diagnosing problems.")

(defconst speechd--buffer " *speechd*"
  "Name of the buffer associated with speechd connections.")

(defconst speechd--application-name "Emacs"
  "Name of the client as set to speechd.")

(defconst speechd--coding-system 'utf-8-dos)

(defconst speechd--parameter-names
  '((:client-name . "CLIENT_NAME")
    (:language . "LANGUAGE")
    (:message-priority . "PRIORITY")
    (:punctuation-mode . "PUNCTUATION")
    (:important-punctuation . "IMPORTANT_PUNCTUATION")
    (:punctuation-table . "PUNCTUATION_TABLE")
    (:spelling-table . "SPELLING_TABLE")
    (:text-table . "TEXT_TABLE")
    (:character-table . "CHARACTER_TABLE")
    (:key-table . "KEY_TABLE")
    (:sound-table . "SOUND_TABLE")
    (:voice . "VOICE")
    (:rate . "RATE")
    (:pitch . "PITCH")))

(defconst speechd--list-parameter-names
  '((:spelling-tables . "SPELLING_TABLES")
    (:punctuation-tables . "PUNCTUATION_TABLES")
    (:text-tables . "TEXT_TABLES")
    (:sound-tables . "SOUND_TABLES")
    (:character-tables . "CHARACTER_TABLES")
    (:key-tables . "KEY_TABLES")
    (:voices . "VOICES")))

(defconst speechd--parameter-value-mappings
  '((:message-priority
     (:high .   "1")
     (:medium . "2")
     (:low .    "3"))
    (:punctuation-mode
     (:none . "none")
     (:some . "some")
     (:all .  "all"))))


;;; Internal variables


(defstruct speechd--connection
  name
  host
  port
  (failure-p nil)
  process
  (process-output nil)
  (command-queue (queue-create))
  (paused-p nil)
  (sending-data-p nil)
  (reading-answer-p nil)
  (parameters ()))

(defvar speechd--connections (make-hash-table :test 'equal)
  "Hash table mapping client names to `speechd-connection' instances.")

(defvar speechd--debug-info '())


;;; Utilities


(defmacro* speechd--with-current-connection (&body body)
  `(let ((connection (speechd--connection)))
     ,@body))

(defconst speechd--maximum-queue-length 10)
(defun speechd--queue-too-long-p (queue)
  (>= (queue-length queue) speechd--maximum-queue-length))


;;; Process handling functions


(defun* speechd--connection (&optional (name speechd-client-name)
				       (create-if-needed t))
  (or (gethash name speechd--connections)
      (and create-if-needed
	   (let ((speechd-client-name name))
	     (speechd-open)))))

(defun speechd--process-name ()
  (process-name (speechd--connection-process (speechd--connection))))

(defun speechd--connection-filter (process string)
  (when speechd-debug
    (with-current-buffer (process-buffer process)
      (let* ((marker (process-mark process))
	     (marker-position (or (marker-position marker) 1))
	     (moving (= (point) marker-position)))
	(save-excursion
	  (goto-char marker-position)
	  (insert string)
	  (set-marker marker (point)))
	(when moving
	  (goto-char marker-position)))))
  (speechd--with-current-connection
    (setf (speechd--connection-process-output connection)
	  (concat (speechd--connection-process-output connection) string))))

;;;###autoload
(defun* speechd-open (&optional (host speechd-host) (port speechd-port))
  "Open connection to speechd running on the given host and port.
The optional arguments HOST and PORT identify the speechd location differing
from the values of `speechd-host' and `speechd-port', see `open-network-stream'
for closer description of those arguments."
  (interactive)
  (speechd-close speechd-client-name)
  (let* ((name speechd-client-name)
	 (process (condition-case nil
		      (open-network-stream "speechd-proc"
					   (concat speechd--buffer "-" name)
					   host port)
		    (error nil))))
    (if process
	(progn
	  (set-process-coding-system
	   process speechd--coding-system speechd--coding-system)
	  (set-process-filter process 'speechd--connection-filter)
	  (process-kill-without-query process))
      (message "Connection to Speech Daemon failed"))
    (let ((connection (make-speechd--connection
		       :name name :host host :port port
		       :process process :failure-p (not process))))
      (puthash name connection speechd--connections)
      (speechd--set-connection-name name)
      connection)))

(defun* speechd-close (&optional (name speechd-client-name))
  "Close connection named NAME to speechd."
  (interactive)
  (let ((connection (speechd--connection name nil)))
    (when connection
      (let ((process (speechd--connection-process connection)))
	(ignore-errors
	  (delete-process process))
	(ignore-errors
	  (kill-buffer (process-buffer process)))
	(setf (speechd--connection-process connection) nil)))))

(defun speechd-reopen ()
  "Close and open again the connection to speechd."
  (interactive)
  (speechd-close)
  (speechd-open))

(defun speechd-running-p ()
  (let* ((connection (speechd--connection))
	 (process (and connection (speechd--connection-process connection))))
    (and process (eq (process-status process) 'open))))


;;; Process communication functions


(defconst speechd--eol "\n")
(defconst speechd--end-regexp (format "^[0-9][0-9][0-9] .*%s" speechd--eol))
(defconst speechd--result-regexp
  (format "\\`[0-9][0-9][0-9]-\\(.*\\)%s" speechd--eol))
(defconst speechd--success-regexp
  (format "^[1-2][0-9][0-9] .*%s" speechd--eol))

(defvar pokus ())
(defun speechd--send-string (string &optional expect-answer)
  (speechd--with-current-connection
    (if (speechd--connection-reading-answer-p connection)
	(let ((queue (speechd--connection-command-queue connection)))
	  (when (or (speechd--connection-sending-data-p connection)
		    (not (speechd--queue-too-long-p queue)))
	    (queue-enqueue queue (cons string expect-answer))))
      (let ((process (speechd--connection-process connection)))
	(when process
	  (unwind-protect
	      (progn
		(process-send-string process string)
		(push (cons 'w string) pokus))
	    (unless (speechd-running-p)
	      (setf (speechd--connection-process connection) nil))))))))

(defun speechd--command-answer ()
  ;; Why this function exists and the answer is not handled within
  ;; `speech-send-command' immediately:
  ;;
  ;; When a user walks through a buffer with autorepeated C-p or so, he wants
  ;; to listen to the very short sounds indicating beginnings of lines.  Thus
  ;; the reaction time must be very short, preferably less than 10 ms.
  ;; However, it seems that if `accept-process-output' is called and process
  ;; output is not available *immediately*, Emacs is unable to read the awaited
  ;; input anytime sooner than after several tens of miliseconds.  So we delay
  ;; reading the process output until `speechd-command-answer' is called,
  ;; either explicitly or automatically before sending next command.
  (speechd--with-current-connection
    (flet ((read-command-answer ()
             (while (let ((output (speechd--connection-process-output
				   connection)))
		      (and output
			   (not (string-match speechd--end-regexp output))))
	       (let ((output (speechd--connection-process-output connection)))
		 ;; We have to be very careful with accept-process-output -- it
		 ;; can invoke another speechd-el functions in various places.
		 (let ((reading (speechd--connection-reading-answer-p
				 connection)))
		   (setf (speechd--connection-reading-answer-p connection) t)
		   (unwind-protect
		       (accept-process-output (speechd--connection-process
					       connection)
					      speechd-timeout)
		     (setf (speechd--connection-reading-answer-p connection)
			   reading)))
		 (when (string= (speechd--connection-process-output connection)
				output)
		   (push "Timeout" speechd--debug-info)
		   (speechd-close)
		   (error "Timeout during communication with speechd."))))
	     ;; Process the answer
	     (let ((answer (speechd--connection-process-output connection))
		   (data '())
		   success
		   result)
	       (while (and answer (string-match speechd--result-regexp answer))
		 (push (match-string 1 answer) data)
		 (setq answer (substring answer (match-end 0))))
	       (setq success (and answer
				  (string-match speechd--success-regexp
						answer)))
	       (unless success
		 (setq data nil))
	       (setf (speechd--connection-process-output connection) nil)
	       (setq result (list data
				  success
				  (and answer (substring answer 0 3))
				  (and answer (substring answer 4))))
	       (push (cons 'r answer) pokus)
	       answer)))
      ;; If we're within another speechd--command-answer, i.e. the command to
      ;; be answered waits in the queue and the answer isn't available.  So we
      ;; simply return nil.
      (unless (speechd--connection-reading-answer-p connection)
	(prog1
	    (read-command-answer)
	  ;; Free the commands blocked by this speechd--command-answer
	  (let ((queue (speechd--connection-command-queue connection)))
	    (while (not (queue-empty queue))
	      (destructuring-bind
		  (request . expect-answer) (queue-dequeue queue)
		(speechd--send-string request)
		(when expect-answer
		  (setf (speechd--connection-process-output connection) "")
		  (read-command-answer))))))))))

(defun speechd--send-command (command &optional delay-answer)
  (speechd--with-current-connection
    ;; Finish unfinished
    (when (speechd--connection-sending-data-p connection)
      (speechd--send-data-end))
    (when (and (speechd--connection-process-output connection)
	       (not (speechd--connection-reading-answer-p connection)))
      (speechd--command-answer))
    ;; Send command
    (unless (listp command)
      (setq command (list command)))
    (when (or (speechd--connection-process connection)
	      (and (not (speechd--connection-failure-p connection))
		   (speechd-open (speechd--connection-host connection)
				 (speechd--connection-port connection))))
      (let ((string-to-send (concat (mapconcat #'identity command " ")
				    speechd--eol)))
	(when speechd-debug
	  (display-buffer (process-buffer
			   (speechd--connection-process connection))))
	(funcall (if speechd-debug #'message #'ignore)
		 (speechd--send-string string-to-send t))
	(unless (speechd--connection-reading-answer-p connection)
	  (setf (speechd--connection-process-output connection) ""))))
    ;; Read answer
    (unless (or (speechd--connection-reading-answer-p connection)
		delay-answer)
      (speechd--command-answer))))

(defun speechd--send-data (text)
  (speechd--with-current-connection
    (unless (speechd--connection-sending-data-p connection)
      (speechd-resume t)
      (speechd--send-command "SPEAK")
      (setf (speechd--connection-sending-data-p connection) t))
    (flet ((send (string)
	     (when speechd-debug
	       (push "Sending data" speechd--debug-info))
	     (speechd--send-string string)
	     (when speechd-debug
	       (push "Data sent" speechd--debug-info))))
      (while (and (> (length text) 0)
		  (string-match "\\(`\\|.*\n\\)\\(\\.\\)\\(.*\\)\\(\n\\|\\'\\)"
				text))
	(replace-match ".." nil nil text 2)
	(let ((end (1+ (match-end 0))))
	  (send (substring text 0 end))
	  (setq text (substring text end))))
      (send text)
      (unless (or (string= text "")
		  (eql (aref text (1- (length text))) ?\n))
	(speechd--send-string speechd--eol)))))

(defun speechd--send-data-end ()
  (speechd--with-current-connection
    (when (speechd--connection-sending-data-p connection)
      (setf (speechd--connection-sending-data-p connection) nil)
      (speechd--send-command "." t))))


;;; Value retrieval functions


(defun speechd--list (parameter)
  (first (speechd--send-command
	  (list "LIST"
		(cdr (assoc parameter speechd--list-parameter-names))))))


;;; Parameter setting functions


(defun speechd--convert-numeric (number)
  (cond ((< number -100) -100)
	((> number 100) 100)
	(t number)))

(defun speechd--transform-parameter-value (parameter value)
  (cond
   ((stringp value)
    value)
   ((integerp value)
    (number-to-string (speechd--convert-numeric value)))
   ((symbolp value)
    (cdr (assoc value
		(cdr (assoc parameter speechd--parameter-value-mappings)))))))

(defun speechd--set-parameter (parameter value)
  (speechd--with-current-connection
    (unless (equal (plist-get (speechd--connection-parameters connection)
			      parameter)
		   value)
      (setf (speechd--connection-parameters connection)
	    (plist-put (speechd--connection-parameters connection)
		       parameter value))
      (speechd--send-command
       (list "SET" "self"
	     (cdr (assoc parameter speechd--parameter-names))
	     (speechd--transform-parameter-value parameter value))))))

(defun speechd--set-connection-name (name)
  (speechd--set-parameter
   :client-name
   (format "%s:%s:%s" (user-login-name) speechd--application-name name)))

(defun speechd-set-language (language)
  (interactive (list (read-string "Language: ")))
  (speechd--set-parameter :language language))

(defconst speechd--set-punctuation-mode-table '(("none" . :none)
						("some" . :some)
						("all" .  :all)))
(defun speechd-set-punctuation-mode (mode)
  (interactive (list
		(cdr
		 (rassoc (completing-read "Punctuation mode: "
					  speechd--set-punctuation-mode-table
					  nil t)
			 speechd--set-punctuation-mode-table))))
  (speechd--set-parameter :punctuation-mode mode))

(defmacro speechd--generate-set-command (parameter prompt argdesc)
  (let ((prompt (concat prompt ": ")))
    `(defun ,(intern (concat "speechd-set-"
			     (substring (symbol-name parameter) 1)))
            (value)
       (interactive
	,(cond
	  ((integerp argdesc)
	   (concat "n" prompt))
	  (t
	   `(completing-read ,prompt
			     (mapcar #'list (speechd--list ,argdesc))))))
       (speechd--set-parameter ,parameter value))))
(speechd--generate-set-command :punctuation-table "Punctuation table"
			       :punctuation-tables)
(speechd--generate-set-command :spelling-table "Spelling table"
			       :spelling-tables)
(speechd--generate-set-command :text-table "Text table" :text-tables)
(speechd--generate-set-command :sound-table "Sound table" :sound-tables)
(speechd--generate-set-command :character-table "Character table"
			       :character-tables)
(speechd--generate-set-command :key-table "Key table" :key-tables)
(speechd--generate-set-command :pitch "Pitch" 0)
(speechd--generate-set-command :rate "Rate" 0)
(speechd--generate-set-command :voice "Voice" :voices)


;;; Speaking functions


;;;###autoload
(defun* speechd-say-text (text &key (priority speechd-default-text-priority)
			            (finish t))
  (interactive "sText: ")
  (speechd--set-parameter :message-priority priority)
  (speechd--send-data text)
  (when finish
    (speechd--send-data-end)))

(defun* speechd-say-sound (name &key (priority speechd-default-sound-priority))
  (speechd--set-parameter :message-priority priority)
  (speechd--send-command (list "SOUND_ICON" name)))

(defun* speechd-say-char (char &key (priority speechd-default-char-priority))
  (speechd--set-parameter :message-priority priority)
  (speechd--send-command
   (list "CHAR" (format "\"%s\"" (char-to-string char)))))

(defun* speechd-say-key (key &key (priority speechd-default-key-priority))
  (speechd--set-parameter :message-priority priority)
  ;; TODO: Implement real key handling
  (speechd--send-command (list "KEY" (format "\"%s\"" key))))


;;; Control functions


;;;###autoload
(defun speechd-cancel ()
  (interactive)
  (speechd--send-command '("CANCEL" "self")))

;;;###autoload
(defun speechd-stop ()
  (interactive)
  (speechd--send-command '("STOP" "self")))

;;;###autoload
(defun speechd-pause ()
  (interactive)
  (setf (speechd--connection-paused-p (speechd--connection)) t)
  (speechd--send-command '("PAUSE" "self")))

;;;###autoload
(defun speechd-resume (&optional softp)
  (interactive)
  (speechd--with-current-connection
    (when (or (speechd--connection-paused-p connection) (not softp))
      (speechd--send-command '("RESUME" "self"))
      (setf (speechd--connection-paused-p connection) nil))))

(defun speechd-repeat ()
  "Repeat the last message sent to speechd."
  (let ((id (first (first (speechd--send-command '("HISTORY" "GET" "LAST"))))))
    (when id
      (speechd--send-command (list "HISTORY" "SAY" id)))))


;;; Other functions


(defconst speechd-maintainer-address "pdm@brailcom.org")

;;;###autoload
(defun speechd-submit-bug-report ()
  (interactive)
  (require 'reporter)
  (if speechd-debug
      (if speechd--debug-info
	  (let ((reporter-prompt-for-summary-p t))
	    (reporter-submit-bug-report speechd-maintainer-address
					speechd--el-version
					'(speechd--debug-info)))
	(y-or-n-p "No debugging info available, really send bug report? "))
    (error
     "Please set speechd-debug to T before invoking the bug.")))


;;; Announce

(provide 'speechd)


;;; speechd.el ends here
