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


(defconst speechd--el-version "speechd-el $Id: speechd.el,v 1.14 2003-05-21 13:04:21 pdm Exp $"
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
  (request-queue (queue-create))
  (paused-p nil)
  (transaction-state 'nil)
  (new-state nil)
  (reading-answer-p nil)
  (parameters ()))

(defstruct speechd--request
  string
  (answer-type 'required)
  (transaction-state '(nil nil)))

(defvar speechd--connections (make-hash-table :test 'equal)
  "Hash table mapping client names to `speechd-connection' instances.")

(defvar speechd--debug-info '())


;;; Utilities


(defmacro* speechd--with-current-connection (&body body)
  `(let ((connection (speechd--connection)))
     ,@body))

(defmacro* speechd--with-connection-setting ((var value) &body body)
  (let ((accessor (intern (concat "speechd--connection-" (symbol-name var))))
	(orig-value (gensym)))
    `(let ((,orig-value (,accessor connection)))
       (setf (,accessor connection) ,value)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (,accessor connection) ,orig-value)))))

(defconst speechd--maximum-queue-length 10)
(defun speechd--queue-too-long-p (queue)
  (>= (queue-length queue) speechd--maximum-queue-length))


;;; Process management functions


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

(defun speechd--close-process (connection)
  (let ((process (speechd--connection-process connection)))
    (ignore-errors
      (kill-buffer (process-buffer process)))
    (ignore-errors
      (delete-process process))
    (setf (speechd--connection-process connection) nil)))

;;;###autoload
(defun* speechd-open (&optional host port &key quiet)
  "Open connection to speechd running on the given host and port.
If the connection corresponding to the current `speechd-client-name' value
already exists, close it and reopen again, with the same connection parameters.

The optional arguments HOST and PORT identify the speechd server location
differing from the values of `speechd-host' and `speechd-port', see
`open-network-stream' for closer description of those arguments.

Return the opened connection on success, nil otherwise."
  (interactive)
  (let ((connection (gethash speechd-client-name speechd--connections)))
    (let ((host (or host speechd-host))
	  (port (or port speechd-port)))
      (when connection
	(speechd--close-process connection)
	(setq host (speechd--connection-host connection)
	      port (speechd--connection-port connection)))
      (let* ((name speechd-client-name)
	     (parameters (and connection
			      (speechd--connection-parameters connection)))
	     (process (condition-case nil
			  (open-network-stream
			   "speechd-proc"
			   (concat speechd--buffer "-" name)
			   host port)
			(error nil))))
	(if process
	    (progn
	      (set-process-coding-system
	       process speechd--coding-system speechd--coding-system)
	      (set-process-filter process 'speechd--connection-filter)
	      (process-kill-without-query process))
	  (unless quiet
	    (message "Connection to Speech Daemon failed")))
	(setq connection (make-speechd--connection
			  :name name :host host :port port
			  :process process :failure-p (not process)))
	(puthash name connection speechd--connections)
	(speechd--set-connection-name name)
	(while parameters
	  (destructuring-bind (parameter value . next) parameters
	    (when (not (eq parameter :client-name))
	      (speechd--set-parameter parameter value))
	    (setq parameters next)))))
    connection))

(defun* speechd-close (&optional (name speechd-client-name))
  "Close connection named NAME to speechd."
  (interactive)
  (let ((connection (speechd--connection name nil)))
    (when connection
      (speechd--close-process connection)
      (remhash name speechd--connections))))

(defun speechd-reopen ()
  "Close and open again all the connections to speechd."
  (interactive)
  (let ((number-of-connections 0)
	(number-of-failures 0))
    (maphash #'(lambda (name _)
		 (let ((speechd-client-name name))
		   (if (speechd-open :quiet t)
		       (incf number-of-connections)
		     (incf number-of-failures))))
	     speechd--connections)
    (message (format "%d connections successfully reopened, %d failures"
		     number-of-connections number-of-failures))))

(defun speechd-running-p ()
  "Return non-nil, if the current speechd client name process is running."
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

(defun speechd--send-string (string)
  (speechd--with-current-connection
    (let ((process (speechd--connection-process connection)))
      (when process
	(unwind-protect
	    (process-send-string process string)
	  (when (and (not (speechd-running-p))
		     (not (speechd--connection-failure-p connection)))
	    (let* ((connection (speechd-open))
		   (process (and connection
				 (speechd--connection-process connection))))
	      (if process
		  (process-send-string (speechd--connection-process connection)
				       string)
		(setf (speechd--connection-process connection) nil
		      (speechd--connection-failure-p connection) t)))))))))

(defvar speechd--in-recursion nil)
(defun speechd--process-request (request)
  (speechd--with-current-connection
    (flet ((read-command-answer ()
	     (while (let ((output (speechd--connection-process-output
				   connection)))
		      (and output
			   (not (string-match speechd--end-regexp output))))
	       (let ((output (speechd--connection-process-output connection)))
		 ;; We have to be very careful with accept-process-output -- it
		 ;; can invoke another speechd-el functions in various places.
		 (speechd--with-connection-setting (reading-answer-p t)
		    (accept-process-output
		     (speechd--connection-process connection)
		     speechd-timeout))
		 (when (string= (speechd--connection-process-output connection)
				output)
		   (speechd-close)
		   (error "Timeout during communication with speechd."))))
	     ;; Process the answer
	     (let ((answer (speechd--connection-process-output connection))
		   (data '())
		   success
		   result)
	       (while (and answer (string-match speechd--result-regexp answer))
		 (setq answer (substring answer (match-end 0))))
	       (setq success (and answer
				  (string-match speechd--success-regexp
						answer)))
	       (unless success
		 (setq data nil))
	       (setf (speechd--connection-process-output connection) nil)
	       (when success
		 (setf (speechd--connection-transaction-state connection)
		       (speechd--connection-new-state connection)))
	       (setq result (list success
				  data
				  (and answer (substring answer 0 3))
				  (and answer (substring answer 4))))
	       result)))
      ;; Read pending answer
      (when (speechd--connection-process-output connection)
	(read-command-answer))
      ;; Ensure proper transaction state
      (let* ((state-spec (speechd--request-transaction-state request))
	     (required-state (first state-spec))
	     (new-state (second state-spec)))
	(flet ((check-state ()
		 (when (not (eq (speechd--connection-transaction-state
				 connection)
				required-state))
		   (let ((speechd--in-recursion t))
		     (cond
		      ((and (eq required-state 'in-data)
			    (not (eq new-state nil)))
		       (speechd--send-data-begin))
		      ((eq required-state nil)
		       (speechd--send-data-end)))))
		 (eq (speechd--connection-transaction-state connection)
		     required-state)))
	  (unless (check-state)
	    (speechd-open)
	    (setq connection (speechd--connection)))
	  ;; Continue only if the state can be set properly after reopen,
	  ;; otherwise give up and ignore the request completely.
	  ;; This also works for the "." command when in non-data state.
	  (prog1
	      (when (check-state)
		(speechd--send-string (speechd--request-string request))
		(let ((answer-type (speechd--request-answer-type request)))
		  (when answer-type
		    (setf (speechd--connection-process-output connection) ""
			  (speechd--connection-new-state connection) new-state)
		    (when (or (eq answer-type 'required)
			      speechd--in-recursion
			      (not (queue-empty
				    (speechd--connection-request-queue
				     connection))))
		      (read-command-answer)))))
	    ;; Free the commands blocked by this speechd--command-answer
	    (unless speechd--in-recursion
	      (let ((speechd--in-recursion t)
		    (queue (speechd--connection-request-queue connection)))
		(while (not (queue-empty queue))
		  (let* ((request (queue-dequeue queue))
			 (answer-type (speechd--request-answer-type request)))
		    (speechd--process-request request)))))))))))

(defun speechd--send-request (request)
  (speechd--with-current-connection
    (if (speechd--connection-reading-answer-p connection)
	(let ((queue (speechd--connection-request-queue connection)))
	  (when (or (not (speechd--queue-too-long-p queue))
		    (first (speechd--request-transaction-state request)))
	    (queue-enqueue queue request))
	  nil)
      (speechd--process-request request))))

(defun* speechd--send-command (command &optional delay-answer
				       (transaction-state '(nil nil)))
  (unless (listp command)
    (setq command (list command)))
  (speechd--send-request
   (make-speechd--request :string (concat (mapconcat #'identity command " ")
					  speechd--eol)
			  :answer-type (if delay-answer 'delayed 'required)
			  :transaction-state transaction-state)))

(defun speechd--send-data-begin ()
  (speechd--send-command "SPEAK" nil '(nil in-data)))

(defun speechd--send-data (text)
  (flet ((send (string)
           (speechd--send-request (make-speechd--request
				   :string string :answer-type nil
				   :transaction-state '(in-data in-data)))))
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
      (send speechd--eol))))

(defun speechd--send-data-end ()
  (speechd--send-command "." t '(in-data nil)))


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
    (let* ((plist (speechd--connection-parameters connection))
	   (orig-value (if (plist-member plist parameter)
			   (plist-get plist parameter)
			 'unknown)))
      (unless (equal orig-value value)
	(let ((answer
	       (speechd--send-command
		(list "SET" "self"
		      (cdr (assoc parameter speechd--parameter-names))
		      (speechd--transform-parameter-value parameter value)))))
	  (setq connection (speechd--connection))
	  (setf (speechd--connection-parameters connection)
		(plist-put (speechd--connection-parameters connection)
			   parameter
			   (cond
			    ((not answer) 'unknown)
			    ((first answer) value)
			    (t orig-value)))))))))

(defun speechd--set-connection-name (name)
  (speechd--set-parameter
   :client-name
   (format "%s:%s:%s" (user-login-name) speechd--application-name name)))

(defun speechd-set-language (language)
  "Set language of the current client connection to LANGUAGE.
Language must be an RFC 1766 language code, as a string."
  (interactive (list (read-string "Language: ")))
  (speechd--set-parameter :language language))

(defconst speechd--set-punctuation-mode-table '(("none" . :none)
						("some" . :some)
						("all" .  :all)))
(defun speechd-set-punctuation-mode (mode)
  "Set punctuation mode to MODE.
Mode must be one of the symbols `:none' (don't read any punctuation), `:some'
(read only some punctuation), and `:all' (read all the punctuation)."
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
  "Speak the given TEXT, represented by a string.
The key argument `priority' defines the priority of the message and must be one
of the symbols `:high', `:medium', and `:low'.
If the key argument `finish' is t, TEXT completes the message -- the next
invocation of this function will start a new text message to speechd.
Otherwise the message leaves open and the next invocation this function will
append the next text to it.  Regardless of the FINISH value, the function
initiates sending text data to speechd immediately."
  (interactive "sText: ")
  (speechd--set-parameter :message-priority priority)
  (when (speechd--connection-paused-p (speechd--connection))
    (speechd-resume))
  (speechd--send-data text)
  (when finish
    (speechd--send-data-end)))

(defun* speechd-say-sound (name &key (priority speechd-default-sound-priority))
  "Ask speechd to play an auditory icon.
NAME is the name of the icon, any string acceptable by speechd.
The key argument `priority' defines the priority of the message and must be one
of the symbols `:high', `:medium', and `:low'."
  (speechd--set-parameter :message-priority priority)
  (speechd--send-command (list "SOUND_ICON" name)))

(defun* speechd-say-char (char &key (priority speechd-default-char-priority))
  "Speak the given CHAR, any UTF-8 character.
The key argument `priority' defines the priority of the message and must be one
of the symbols `:high', `:medium', and `:low'."
  (speechd--set-parameter :message-priority priority)
  (speechd--send-command
   (list "CHAR" (format "\"%s\"" (char-to-string char)))))

(defun* speechd-say-key (key &key (priority speechd-default-key-priority))
  "Speak the given KEY.
The exact value and meaning of KEY is undefined now.
The key argument `priority' defines the priority of the message and must be one
of the symbols `:high', `:medium', and `:low'."
  (speechd--set-parameter :message-priority priority)
  ;; TODO: Implement real key handling
  (speechd--send-command (list "KEY" (format "\"%s\"" key))))


;;; Control functions


(defun speechd--control-command (command all)
  (speechd--send-command (list command (if all "all" "self"))))

;;;###autoload
(defun speechd-cancel (&optional all)
  "Stop speaking all the messages sent through the current client so far.
If the optional argument ALL is non-nil, stop speaking messages of all
clients."
  (interactive "P")
  (speechd--control-command "CANCEL" all))

;;;###autoload
(defun speechd-stop (&optional all)
  "Stop speaking the currently spoken message (if any) of this client.
If the optional argument ALL is non-nil, stop speaking the currently spoken
messages of all clients."
  (interactive "P")
  (speechd--control-command "STOP" all))

;;;###autoload
(defun speechd-pause (&optional all)
  "Pause speaking in the current client.
If the optional argument ALL is non-nil, pause speaking in all clients."
  (interactive "P")
  (setf (speechd--connection-paused-p (speechd--connection)) t)
  (speechd--control-command "PAUSE" all))

;;;###autoload
(defun speechd-resume (&optional all)
  "Resume previously stopped speaking in the current client.
If the optional argument ALL is non-nil, resume speaking messages of all
clients."
  (interactive "P")
  (speechd--with-current-connection
    (when (speechd--connection-paused-p connection)
      (speechd--control-command "RESUME" all)
      (setq connection (speechd--connection))
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
  "Submit a bug report about speechd-el."
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
