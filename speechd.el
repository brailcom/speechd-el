;;; speechd.el --- Library for accessing Speech Dispatcher

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

;; This library allows you to communicate with Speech Dispatcher.
;; Usually, the communication goes like this:
;; 
;;   (speechd-open)
;;   ...
;;   (speechd-say-text "Hello, world!")
;;   ...
;;   (speechd-close)
;;
;; Functions and variables starting with the prefix `speechd--' are considered
;; internal and you shouldn't use them outside this file.
;;
;; *Note*: If you call any of the speechd-el functions asynchronously
;; (typically within a process filter function), you must wrap the whole
;; asynchronous code by the `speechd-protect' macro.
;;
;; See docstrings and the Texinfo manual for full details.

;;; Code:


(eval-when-compile (require 'cl))
(require 'queue-f)


;;; User variables


(defgroup speechd ()
  "Speech Dispatcher interface.")

(defcustom speechd-host "localhost"
  "Name of the default host running speechd to connect to."
  :type 'string
  :group 'speechd)

(defcustom speechd-port
  (or (condition-case _
          (car (read-from-string (getenv "SPEECHD_PORT")))
        (error))
      6560)
  "Default port of speechd."
  :type 'integer
  :group 'speechd)

(defcustom speechd-timeout 3
  "Number of seconds to wait for speechd response."
  :type 'integer
  :group 'speechd)

(define-widget 'speechd-priority-tag 'radio
  "Radio group for selecting a speechd priority tag."
  :args '((const :tag "Important"    :value important)
	  (const :tag "Message"      :value message)
	  (const :tag "Text"         :value text)
	  (const :tag "Notification" :value notification)
	  (const :tag "Progress"     :value progress)))

(defcustom speechd-default-text-priority 'text
  "Default Speech Dispatcher priority of sent texts."
  :type 'speechd-priority-tag
  :group 'speechd)

(defcustom speechd-default-sound-priority 'message
  "Default Speech Dispatcher priority of sent sound icons."
  :type 'speechd-priority-tag
  :group 'speechd)

(defcustom speechd-default-char-priority 'notification
  "Default Speech Dispatcher priority of sent single letters."
  :type 'speechd-priority-tag
  :group 'speechd)

(defcustom speechd-default-key-priority 'notification
  "Default Speech Dispatcher priority of sent symbols of keys."
  :type 'speechd-priority-tag
  :group 'speechd)


(defmacro speechd--generate-customization-options (var)
  (let ((value (symbol-value var)))
    `(quote (choice :value ,(cdr (first value))
                    ,@(mapcar #'(lambda (o)
                                  `(const :tag ,(car o) ,(cdr o)))
                              value)))))

(defconst speechd--punctuation-modes '(("none" . none)
                                       ("some" . some)
                                       ("all" .  all)))

(defconst speechd--capital-character-modes '(("none" .  none)
                                             ("spell" . spell)
                                             ("icon" .  icon)))

(defcustom speechd-connection-parameters '()
  "Alist of connection names and their parameters.

Each element of the list is of the form (CONNECTION-NAME . PARAMETERS), where
CONNECTION-NAME is a connection name as expected to be in `speechd-client-name'
and PARAMETERS is a property list with the pairs of parameter identifiers and
parameter values.  Valid parameter names are the following symbols:
language, message-priority, punctuation-mode, capital-character-mode, voice,
rate, pitch, output-module.  See the corresponding speechd-set-* functions for
valid parameter values.

If the symbol t is specified as the connection name, the element defines
default connection parameters if no connection specification applies.  Only one
such an element is allowed in the whole alist.

The message-priority parameter has a special meaning: It overrides priority of
all messages sent through the connection.

You must reopen the connections to apply the changes to this variable."
  :get #'(lambda (name)
           (mapcar
            #'(lambda (v)
                (cons (car v)
                      (let ((result '())
                            (params (cdr v)))
                        (while params
                          (push (cons (car params) (cadr params)) result)
                          (setq params (nthcdr 2 params)))
                        (nreverse result))))
            (symbol-value name)))
  :set #'(lambda (name value)
           (let ((real-value
                  (mapcar
                   #'(lambda (item)
                       (cons (car item)
                             (apply #'nconc
                                    (mapcar #'(lambda (ival)
                                                (list (car ival) (cdr ival)))
                                            (cdr item)))))
                   value)))
             (set-default name real-value))
           (when (fboundp 'speechd-reopen)
             (speechd-reopen)))
  :type `(repeat
          (cons
	   :tag "Connection"
	   (choice (string :tag "Connection name")
		   (const :tag "Default parameters" :value t))
	   (set
	    :tag "Parameters"
	    (cons :tag "Language" (const :format "" language) string)
	    (cons :tag "Messsage priority"
		  (const :format "" message-priority)
		  (speechd-priority-tag :value text))
	    (cons :tag "Punctuation mode" (const :format "" punctuation-mode)
		  ,(speechd--generate-customization-options
		    speechd--punctuation-modes))
	    (cons :tag "Important punctuation"
		  (const :format "" important-punctuation) string)
	    (cons :tag "Capital character mode"
		  (const :format "" capital-character-mode)
		  ,(speechd--generate-customization-options
		    speechd--capital-character-modes))
	    (cons :tag "Voice" (const :format "" voice) string)
	    (cons :tag "Rate" (const :format "" rate) integer)
	    (cons :tag "Pitch" (const :format "" pitch) integer)
	    (cons :tag "Output module" (const :format "" output-module)
		  string))))
  :group 'speechd)

(defcustom speechd-face-voices '()
  "Alist mapping faces to voices.
Each of the alist element is of the form (FACE . STRING) where FACE is a face
and string voice identifier.  Each face is spoken in the corresponding voice.
If there's no item for a given face in this variable, the face is spoken in the
current voice."
  :type '(alist :key-type face :value-type (string :tag "Voice"))
  :group 'speechd)


;;; External variables


(defvar speechd-client-name "default"
  "String defining current client name.
This variable's value defines which connection is used when communicating with
Speech Dispatcher, each connection has its own client name.  Usually, you
select the proper client (connection) by assigning a value to this variable
locally through `let'.")

(defvar speechd-language nil
  "If non-nil, it is an RFC 1766 language code, as a string.
If text is read and this variable is non-nil, the text is read in the given
language.")


;;; Internal constants and configuration variables


(defconst speechd--el-version "speechd-el $Id: speechd.el,v 1.75 2003-10-29 10:33:48 pdm Exp $"
  "Version stamp of the source file.
Useful only for diagnosing problems.")

(defconst speechd--buffer " *speechd*"
  "Name of the buffer associated with speechd connections.")

(defconst speechd--application-name "Emacs"
  "Name of the client as set to speechd.")

(defvar speechd--language-codes
  '(("czech" . "cs")
    ("english" . "en")
    ("french" . "fr")
    ("german" . "de")))

(defvar speechd--default-voice "male1")
(defvar speechd--default-language (or (cdr (assoc current-language-environment
                                                  speechd--language-codes))
                                      "en"))

(defconst speechd--coding-system (if (featurep 'xemacs)
				     'no-conversion-dos ;; No utf yet
				   'utf-8-dos))

(defconst speechd--parameter-names
  '((client-name . "CLIENT_NAME")
    (language . "LANGUAGE")
    (message-priority . "PRIORITY")
    (punctuation-mode . "PUNCTUATION")
    (pause-context . "PAUSE_CONTEXT")
    (capital-character-mode . "CAP_LET_RECOGN")
    (voice . "VOICE")
    (rate . "RATE")
    (pitch . "PITCH")
    (output-module . "OUTPUT_MODULE")	; TODO: to be removed sometimes
    ))

(defconst speechd--list-parameter-names
  '((voices . "VOICES")))

(defconst speechd--parameter-value-mappings
  '((message-priority
     (important .    "IMPORTANT")
     (message .      "MESSAGE")
     (text .         "TEXT")
     (notification . "NOTIFICATION")
     (progress .     "PROGRESS")
     )
    (punctuation-mode
     (none . "none")
     (some . "some")
     (all .  "all"))
    (capital-character-mode
     (none . "none")
     (spell . "spell")
     (icon . "icon"))))

(defconst speechd--volatile-parameters '(output-module))


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
  (in-block nil)
  (transaction-state 'nil)
  (new-state nil)
  (reading-answer-p nil)
  (parameters ())
  (forced-priority nil))

(defstruct speechd--request
  string
  (answer-type 'required)
  (transaction-state '(nil nil)))

(defvar speechd--connections (make-hash-table :test 'equal)
  "Hash table mapping client names to `speechd-connection' instances.")

(defvar speechd--protect nil
  "If non-nil, don't call `access-process-output'.")


;;; Utilities


(defmacro speechd-protect (&rest body)
  "Ensure proper operation of an asynchronous code BODY.
All asynchronously called code (i.e. code called invoked in process filters or
timers, etc.) invoking any of the speechd-el functions must be wrapped by this
macro."
  `(let ((already-protected speechd--protect)
	 (speechd--protect t))
     (prog1 (progn ,@body)
       (unless already-protected
	 (speechd--process-queues)))))

(defmacro speechd--iterate-connections (&rest body)
  `(maphash #'(lambda (_ connection) ,@body) speechd--connections))

(defun speechd--iterate-connections-collect (function)
  (let ((result '()))
    (speechd--iterate-connections
     (push (funcall function) result))
    result))

(defun speechd-connection-names ()
  "Return the list of all present connection names."
  (speechd--iterate-connections-collect
   #'(lambda () (speechd--connection-name connection))))

(defmacro speechd--with-current-connection (&rest body)
  `(let ((connection (speechd--connection)))
     ,@body))

(defmacro speechd--with-connection-setting (var value &rest body)
  (let ((accessor (intern (concat "speechd--connection-" (symbol-name var))))
	(orig-value (gensym)))
    `(let ((,orig-value (,accessor connection)))
       (setf (,accessor connection) ,value)
       (unwind-protect
	   (progn
	     ,@body)
	 (setf (,accessor connection) ,orig-value)))))

(defmacro speechd--with-connection-parameters (parameters &rest body)
  (let (($parameters (gensym))
        ($orig-parameters (gensym))
        ($p (gensym))
        ($v (gensym))
        ($orig-v (gensym))
        ($pv (gensym)))
    `(let* ((,$parameters ,parameters)
            (,$orig-parameters ()))
       (unwind-protect
           (progn
             (while ,$parameters
               (let* ((,$p (first ,$parameters))
                      (,$v (second ,$parameters))
                      (,$orig-v (plist-get
                                 (speechd--connection-parameters connection)
                                 ,$p)))
                 (when (and (not (equal ,$v ,$orig-v))
                            (or ,$v
                                (not (memq ,$p '(language)))))
                   (push (cons ,$p ,$orig-v) ,$orig-parameters)
                   (speechd--set-parameter ,$p ,$v)))
               (setq ,$parameters (nthcdr 2 ,$parameters)))
             ,@body)
         (dolist (,$pv ,$orig-parameters)
           (speechd--set-parameter (car ,$pv) (cdr ,$pv)))))))

(defconst speechd--maximum-queue-length 10)
(defun speechd--queue-too-long-p (queue)
  (>= (queue-length queue) speechd--maximum-queue-length))

(defun speechd-language (string language)
  "Put language property LANGUAGE on whole STRING.
Language should be a string recognizable by Speech Dispatcher as a language
code."
  (put-text-property 0 (length string) 'language language string)
  string)


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
  (speechd--with-current-connection
    (let ((c (if (eq process (speechd--connection-process connection))
                 connection
               (speechd--connection (substring
                                     (buffer-name (process-buffer process))
                                     (1+ (length speechd--buffer)))))))
      (setf (speechd--connection-process-output c)
            (concat (speechd--connection-process-output c) string)))))

(defun speechd--close-process (connection)
  (let ((process (speechd--connection-process connection)))
    (ignore-errors
      (let ((buffer (process-buffer process)))
	(when buffer
	  (kill-buffer buffer))))
    (ignore-errors
      (when process
	(delete-process process)))
    (setf (speechd--connection-process connection) nil)))

;;;###autoload
(defun* speechd-open (&optional host port &key quiet force-reopen)
  "Open connection to Speech Dispatcher running on the given host and port.
If the connection corresponding to the current `speechd-client-name' value
already exists, close it and reopen again, with the same connection parameters.

The optional arguments HOST and PORT identify the speechd server location
differing from the values of `speechd-host' and `speechd-port', see
`open-network-stream' for closer description of those arguments.

If the key argument QUIET is non-nil, don't report failures and quit silently.
If the key argument FORCE-REOPEN is non-nil, try to reopen an existent
connection even if it previously failed.

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
             (default-parameters (append
                                  (cdr (assoc speechd-client-name
                                              speechd-connection-parameters))
                                  (cdr (assoc t
                                              speechd-connection-parameters))))
	     (parameters (if connection
                             (append
                              (speechd--connection-parameters connection)
                              default-parameters)
                           default-parameters))
             (forced-priority (plist-get default-parameters 'message-priority))
	     (process (when (or
                             (not connection)
                             (not (speechd--connection-failure-p connection))
                             force-reopen)
                        (condition-case nil
                            (open-network-stream
                             "speechd-proc"
                             (concat speechd--buffer "-" name)
                             host port)
                          (error nil)))))
	(if process
	    (progn
	      ;; The process input encoding is set to raw-text and the coding
	      ;; conversion is performed by hand in `speechd--send-string'.
	      ;; The reason is that Emacs 21.3 is buggy when performing the
	      ;; automated conversion on longer strings.
	      (set-process-coding-system process
					 speechd--coding-system 'raw-text)
	      (set-process-filter process 'speechd--connection-filter)
	      (process-kill-without-query process))
	  (unless quiet
	    (message "Connection to Speech Dispatcher failed")))
	(setq connection (make-speechd--connection
			  :name name :host host :port port
			  :process process :failure-p (not process)))
	(puthash name connection speechd--connections)
	(when process
	  (speechd--set-connection-name name)
          (setq parameters (append parameters
                                   (list 'language speechd--default-language
                                         'voice speechd--default-voice)))
          (let ((already-set '(client-name)))
            (while parameters
              (destructuring-bind (parameter value . next) parameters
                (unless (memq parameter already-set)
                  (push parameter already-set)
                  (speechd--set-parameter parameter value))
                (setq parameters next)))))
        (let ((priority (and
                         connection
                         (plist-get default-parameters 'message-priority))))
          (when priority
            (speechd--set-parameter 'message-priority priority)
            (setf (speechd--connection-forced-priority connection) t)))))
    connection))

(defun* speechd-close (&optional (name speechd-client-name))
  "Close speechd connection named NAME."
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
		   (if (speechd-open :quiet t :force-reopen t)
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

(defun speechd--permanent-connection-failure (connection)
  (speechd--close-process connection)
  (setf (speechd--connection-failure-p connection) t
	(speechd--connection-process-output connection) nil
	(speechd--connection-request-queue connection) (queue-create)
	(speechd--connection-paused-p connection) nil
	(speechd--connection-transaction-state connection) nil
	(speechd--connection-new-state connection) nil
	(speechd--connection-reading-answer-p connection) nil
	(speechd--connection-parameters connection) ()))

(defun speechd--send-string (string)
  (speechd--with-current-connection
    (let ((process (speechd--connection-process connection))
	  (string (encode-coding-string string speechd--coding-system)))
      (when process
	(unwind-protect
	    (process-send-string process string)
	  (when (not (speechd-running-p))
            (speechd--permanent-connection-failure connection)))))))

(defvar speechd--in-recursion nil)

(defun speechd--process-queues ()
  (unless speechd--in-recursion
    (let ((speechd--in-recursion t))
      (speechd--iterate-connections
        (let ((queue (speechd--connection-request-queue connection)))
          (while (not (queue-empty queue))
            (let* ((request (queue-dequeue queue))
                   (answer-type (speechd--request-answer-type request)))
              (speechd--process-request request))))))))

(defun speechd--process-request (request)
  (speechd--with-current-connection
   (save-match-data
    (flet ((read-command-answer ()
	     (while (let ((output (speechd--connection-process-output
				   connection)))
		      (and output
			   (not (string-match speechd--end-regexp output))))
	       (let ((output (speechd--connection-process-output connection)))
		 ;; We have to be very careful with accept-process-output -- it
		 ;; can invoke another speechd-el functions in various places.
		 (speechd--with-connection-setting reading-answer-p t
		   (accept-process-output
		     (speechd--connection-process connection)
		     speechd-timeout))
		 (when (string= (speechd--connection-process-output connection)
				output)
		   (speechd--permanent-connection-failure connection)
		   (error "Timeout during communication with speechd."))))
	     ;; Process the answer
	     (let ((answer (speechd--connection-process-output connection))
		   (data '())
		   success
		   result)
               (while (and answer
                           (string-match speechd--result-regexp answer))
                 (push (match-string 1 answer) data)
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
      ;; Read pending answers
      (speechd--iterate-connections
        (when (speechd--connection-process-output connection)
          (read-command-answer)))
      ;; Ensure proper transaction state
      (let* ((state-spec (speechd--request-transaction-state request))
	     (required-state (first state-spec))
	     (new-state (second state-spec)))
	(labels ((check-state (reopen-if-needed)
		   (let ((current-state (speechd--connection-transaction-state
					 connection)))
		     (when (and (not (eq current-state required-state))
				(not (eq current-state new-state)))
		       (let ((speechd--in-recursion t))
			 (cond
			  ((and (eq required-state 'in-data)
				(not (eq new-state nil)))
			   (speechd--send-data-begin t))
			  ((eq required-state nil)
			   (speechd--send-data-end t)))))
		     (setq current-state (speechd--connection-transaction-state
					  connection))
		     (if (and reopen-if-needed
			      (not (eq current-state required-state))
			      (not (eq current-state new-state))
			      (not (speechd--connection-failure-p connection)))
			 (progn
			   (speechd-open)
			   (setq connection (speechd--connection))
			   (check-state nil))
		       (eq current-state required-state)))))
	  ;; Continue only if the state can be set properly after reopen,
	  ;; otherwise give up and ignore the request completely.
	  ;; This also works for the "." command when in non-data state.
	  (prog1
	      (when (check-state t)
		(speechd--send-string (speechd--request-string request))
		(let ((answer-type (speechd--request-answer-type request)))
		  (when (and answer-type
			     (not (speechd--connection-failure-p connection)))
		    (setf (speechd--connection-process-output connection) ""
			  (speechd--connection-new-state connection) new-state)
		    (when (or (eq answer-type 'required)
			      speechd--in-recursion
			      (not (queue-empty
				    (speechd--connection-request-queue
				     connection))))
		      (read-command-answer)))))
	    ;; Free the commands blocked by this speechd--command-answer
	    (speechd--process-queues))))))))

(defun speechd--send-request (request &optional now)
  (speechd--with-current-connection
    (if (and (not now)
	     (or (speechd--connection-reading-answer-p connection)
		 speechd--protect))
	(let ((queue (speechd--connection-request-queue connection)))
	  (when (or (not (speechd--queue-too-long-p queue))
		    (first (speechd--request-transaction-state request)))
	    (queue-enqueue queue request))
	  nil)
      (speechd--process-request request))))

(defun* speechd--send-command (command &optional delay-answer
				       (transaction-state '(nil nil))
				       &key now)
  (unless (listp command)
    (setq command (list command)))
  (speechd--send-request
   (make-speechd--request :string (concat (mapconcat #'identity command " ")
					  speechd--eol)
			  :answer-type (if delay-answer 'delayed 'required)
			  :transaction-state transaction-state)
   now))

(defun speechd--send-data-begin (&optional now)
  (speechd--send-command "SPEAK" nil '(nil in-data) :now now))

(defun speechd--send-data (text)
  (let ((text* text))
    (flet ((send (string)
             (speechd--send-request
              (make-speechd--request :string string :answer-type nil
                                     :transaction-state '(in-data in-data)))))
      (save-match-data
        (while (and (> (length text*) 0)
                    (string-match
                     "\\(\\`\\|.*\n\\)\\(\\..*\\)\\(\n\\|\\'\\)"
                     text*))
          (let ((start (match-end 1))
                (end (match-end 0)))
            (send (substring text* 0 start))
            (send (concat "." (substring text* start end)))
            (setq text* (substring text* end))))
        (send text*)
        (unless (string= text "")
          (send speechd--eol))))))

(defun speechd--send-data-end (&optional now)
  (speechd--send-command "." t '(in-data nil) :now now))


;;; Value retrieval functions


(defun speechd--list (parameter)
  (second (speechd--send-command
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
      (when (or (memq parameter speechd--volatile-parameters)
                (and (not (equal orig-value value))
                     (or
                      (not (eq parameter 'message-priority))
                      (not (speechd--connection-forced-priority connection)))))
	(let ((answer
	       (speechd--send-command
                (let ((p (cdr (assoc parameter speechd--parameter-names)))
                      (v (speechd--transform-parameter-value parameter value)))
                  (unless p
                    (error "Invalid parameter name: `%s'" parameter))
                  (unless v
                    (error "Invalid parameter value: %s=%s" parameter value))
                  (list "SET" "self" p v)))))
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
   'client-name
   (format "%s:%s:%s" (user-login-name) speechd--application-name name)))

(defun speechd-set-language (language)
  "Set language of the current client connection to LANGUAGE.
Language must be an RFC 1766 language code, as a string."
  (interactive (list (read-string "Language: ")))
  (speechd--set-parameter 'language language)
  (setq speechd-language language))

(defmacro speechd--generate-set-command (parameter prompt argdesc)
  (let* ((prompt* (concat prompt ": "))
         (argdesc* (eval argdesc))
         (docstring
          (format "Set %s of the current connection.
VALUE must be %s."
                  (downcase prompt)
                  (cond
                   ((integerp argdesc*)
                    "a number between -100 and 100")
                   ((not argdesc*)
                    "a string")
                   ((listp argdesc*)
                    (concat "one of the symbols "
                            (mapconcat #'(lambda (x) (symbol-name (cdr x)))
                                       argdesc* ", ")))
                   (t
                    (concat "one of the strings allowed by your "
                            "Speech Dispatcher\ninstallation"))))))
    `(defun ,(intern (concat "speechd-set-" (symbol-name parameter)))
            (value)
       ,docstring
       (interactive
	,(cond
	  ((integerp argdesc*)
	   (concat "n" prompt*))
	  ((not argdesc*)
	   (concat "s" prompt*))
          ((listp argdesc*)
            `(list
              (cdr
               (assoc (completing-read ,prompt* ,argdesc nil t) ,argdesc))))
	  (t
	   `(list
	     (completing-read ,prompt*
			      (mapcar #'list
                                      (speechd--list (quote ,argdesc*))))))))
       (when value
         (speechd--set-parameter (quote ,parameter) value)
         (message "%s set to %s." ,prompt value)))))

(speechd--generate-set-command capital-character-mode "Capital character mode"
                               speechd--capital-character-modes)
(speechd--generate-set-command pitch "Pitch" 0)
(speechd--generate-set-command rate "Rate" 0)
(speechd--generate-set-command voice "Voice" 'voices)
(speechd--generate-set-command punctuation-mode "Punctuation mode"
                               speechd--punctuation-modes)
(speechd--generate-set-command pause-context "Pause context" 0)
;; TODO: Remove this one once proper output module setting is defined.
(speechd--generate-set-command output-module "Output module" nil)

(defun speechd-add-connection-settings ()
  "Add current connection and its settings to `speechd-connection-parameters'."
  (interactive)
  (setq speechd-connection-parameters
        (cons
         (speechd--with-current-connection
          (cons (speechd--connection-name connection)
                (let ((parameters ())
                      (orig-parameters
                       (speechd--connection-parameters connection)))
                  (while orig-parameters
                    (unless (memq (first orig-parameters)
                                  '(client-name message-priority))
                      (push (first orig-parameters) parameters)
                      (push (second orig-parameters) parameters))
                    (setq orig-parameters (nthcdr 2 orig-parameters)))
                  (nreverse parameters))))
         (remove (assoc speechd-client-name speechd-connection-parameters)
                 speechd-connection-parameters))))

;;; Blocks


(defmacro speechd-block (parameters &rest body)
  "Set PARAMETERS and enclose BODY by an SSIP block.
Before invoking BODY, the BLOCK BEGIN command is sent, and the BLOCK END
command is sent afterwards.
PARAMETERS is a property list defining parameters to be set before sending the
BLOCK BEGIN command.  The property-value pairs correspond to the arguments of
the `speechd--set-parameter' function."
  `(speechd--with-current-connection
     (speechd--with-connection-parameters ,parameters
       (if (and connection (speechd--connection-in-block connection))
           (progn ,@body)
         (let ((block-connection connection))
           (speechd--send-command '("BLOCK BEGIN"))
           (unwind-protect
               (progn
                 (speechd--with-current-connection
                  (when connection
                    (setf (speechd--connection-in-block connection) t)))
                 ,@body)
             (let ((connection block-connection))
               (when connection
                 (setf (speechd--connection-in-block connection) nil)
                 (let ((speechd-client-name
                        (speechd--connection-name connection)))
                   (speechd--send-command '("BLOCK END")))))))))))


;;; Speaking functions


;;;###autoload
(defun* speechd-say-text (text &key (priority speechd-default-text-priority)
			            (finish t))
  "Speak the given TEXT, represented by a string.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'.
If the key argument `finish' is t, TEXT completes the message -- the next
invocation of this function will start a new text message to speechd.
Otherwise the message leaves open and the next invocation this function will
append the next text to it.  Regardless of the FINISH value, the function
initiates sending text data to speechd immediately."
  (interactive "sText: ")
  (speechd--set-parameter 'message-priority priority)
  (unless (string= text "")
    (flet ((properties (point)
             (let ((voice (cdr (assq (get-text-property point 'face text)
                                     speechd-face-voices)))
                   (language (get-text-property point 'language text)))
               (append (when voice (list 'voice voice))
                       (when language (list 'language language))))))
      (speechd-block `(language ,speechd-language)
        (let* ((beg 0)
               (new-properties (properties beg)))
          (while beg
            (let* ((properties new-properties)
                   (change-point beg)
                   (end (progn
                          (while (and
                                  (setq change-point (next-property-change
                                                      change-point text))
                                  (equal properties
                                         (setq new-properties
                                               (properties change-point)))))
                          change-point))
                   (substring (substring text beg end)))
              (if properties
                  (speechd--with-current-connection
                   (speechd--with-connection-parameters properties
                     (speechd--send-data substring)))
                (speechd--send-data substring))
              (setq beg end)))))))
  (when finish
    (speechd--send-data-end)))

(defun* speechd-say-sound (name &key (priority speechd-default-sound-priority))
  "Ask speechd to play an auditory icon.
NAME is the name of the icon, any string acceptable by speechd.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (speechd--set-parameter 'message-priority priority)
  (speechd--send-command (list "SOUND_ICON" name)))

(defun* speechd-say-char (char &key (priority speechd-default-char-priority))
  "Speak the given CHAR, any UTF-8 character.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (speechd--set-parameter 'message-priority priority)
  (speechd--with-current-connection
    (speechd--with-connection-parameters `(language ,speechd-language)
      (speechd--send-command
       (list "CHAR" (format "%s" (case char
                                   (?  "space")
                                   (?\n "linefeed")
                                   (t (char-to-string char)))))))))

(defun* speechd-say-key (key &key (priority speechd-default-key-priority))
  "Speak the given KEY, represented by a key event.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (let* ((modifiers (event-modifiers key))
         (character (event-basic-type key))
         (string (if (numberp character)
                     (cond
                      ((< character 32)
                       (push 'control modifiers)
                       (format "%c" (+ ?a (1- character))))
                      ((and (>= character 128) (< character 160)) "?")
                      ((eql character ? ) "space")
                      ((eql character ?_) "underscore")
                      ((eql character ?_) "double-quote")
                      (t (format "%c" character)))
                   (format "%s" character))))
    (dolist (m modifiers)
      (setq string (concat (symbol-name m) "_" string)))
    (speechd--set-parameter 'message-priority priority)
    (speechd--with-current-connection
      (speechd--with-connection-parameters `(language ,speechd-language)
        (speechd--send-command (list "KEY" (format "%s" string)))))))


;;; Control functions


(defun speechd--control-command (command all)
  (if all
      (speechd--send-command (list command "all"))
    (speechd--iterate-connections
      (speechd--send-command (list command "self")))))

;;;###autoload
(defun speechd-cancel (&optional all)
  "Stop speaking all the messages sent through the current client so far.
If the universal argument is given, stop speaking messages of all clients.
If a numeric argument is given, stop speaking messages of all current Emacs
session clients."
  (interactive "P")
  (if (numberp all)
      (mapcar #'(lambda (name)
                  (let ((speechd-client-name name))
                    (speechd-cancel)))
              (speechd-connection-names))
    (speechd--control-command "CANCEL" all)))

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

;;;###autoload
(defun speechd-repeat ()
  "Repeat the last message sent to speechd."
  (interactive)
  (if t
      (message "Not yet implemented in speechd")
    (let ((id (car (second (speechd--send-command
                            '("HISTORY" "GET" "LAST"))))))
      (when id
        (speechd--send-command (list "HISTORY" "SAY" id))))))


;;; Announce

(provide 'speechd)


;;; speechd.el ends here
