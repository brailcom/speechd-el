;;; speechd.el --- Library for accessing Speech Dispatcher  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2003-2010 Brailcom, o.p.s.

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
;; See docstrings and the Texinfo manual for full details.

;;; Code:


(require 'cl-lib)
(require 'xml)

(require 'speechd-common)


;;; User variables


(defgroup speechd ()
  "SSIP interface."
  :group 'speechd-el)

(defcustom speechd-connection-method 'unix-socket
  "Connection method to Speech Dispatcher.
Possible values are `unix-socket' for Unix domain sockets and
`inet-socket' for Internet sockets on a given host and port (see
`speechd-host' and `speechd-port' variables)."
  :type '(choice (const :tag "Unix domain socket" unix-socket)
                 (const :tag "Internet socket" inet-socket))
  :group 'speechd)

(defcustom speechd-host (or (getenv "SPEECHD_HOST") "localhost")
  "Name of the default host running speechd to connect to.
Value of this variable matters only when Internet sockets are
used for communication with Speech Dispatcher."
  :type 'string
  :group 'speechd)

(defcustom speechd-port
  (or (condition-case _
          (car (read-from-string (getenv "SPEECHD_PORT")))
        (error))
      6560)
  "Default port of speechd.
Value of this variable matters only when Internet sockets are
used for communication with Speech Dispatcher."
  :type 'integer
  :group 'speechd)

(defcustom speechd-autospawn t
  "If non-nil, Emacs will attempt to automatically start Speech Dispatcher.
This means that if speechd-el gets a speech request and the
Speech Dispatcher server is not running already, speechd-el will
launch it."
  :type 'boolean
  :group 'speechd)

(defcustom speechd-timeout 3
  "Number of seconds to wait for speechd response."
  :type 'integer
  :group 'speechd)

(define-widget 'speechd-priority-tag 'choice
  "Radio group for selecting a speechd priority tag."
  :args '((const :tag "Important"    :value important)
	  (const :tag "Message"      :value message)
	  (const :tag "Text"         :value text)
	  (const :tag "Notification" :value notification)
	  (const :tag "Progress"     :value progress)))

(define-widget 'speechd-voice-tag 'choice
  "Voice selector."
  :args '((const :tag "Default" nil)
          (symbol :tag "Named" :value nil
                  :match (lambda (w value)
                           (or (eq value nil)
                               (assoc value speechd-voices))))))


(defmacro speechd--generate-customization-options (var)
  (let ((value (symbol-value var)))
    `(quote (choice :value ,(cdr (cl-first value))
                    ,@(mapcar #'(lambda (o)
                                  `(const :tag ,(car o) ,(cdr o)))
                              value)))))

(defconst speechd--punctuation-modes '(("none" . none)
                                       ("some" . some)
                                       ("all" .  all)))

(defconst speechd--capital-character-modes '(("none" .  none)
                                             ("spell" . spell)
                                             ("icon" .  icon)))

(defun speechd--set-variable-and-reopen (name value)
  (let ((orig-value (when (default-boundp name) (default-value name))))
    (set-default name value)
    (when (and (not (equal orig-value value))
               (fboundp 'speechd-reopen))
      (speechd-reopen))))

(defcustom speechd-voices '()
  "Alist of voice identifiers and their parameters.

Each element of the list is of the form (VOICE-ID . PARAMETERS), where VOICE-ID
is a symbol under which the voice will be accessed and PARAMETERS is an alist
of parameter identifiers and parameter values.  Valid parameter names are the
following symbols: language, gender, age, style, name, rate, pitch, volume,
punctuation-mode, capital-character-mode, message-priority, output-module.
Please note that any parameter entry present will change the corresponding
parameter, even if the parameter value is nil or empty; if you don't want to
change the parameter in any way by the voice, don't put it to the list (and
don't enable its entry in customize).

Name is a string identifying Speech Dispatcher voice name.  If it is not given,
the parameters gender, age, and style are considered to select a Speech
Dispatcher voice.  Gender can be one of the symbols `male', `female',
`neutral'.  Age can be one of the symbols `middle-adult', `child'. `neutral'.
Style can be one of the numbers 1, 2, 3 (style values are likely to be changed
in future).

The message-priority parameter sets priority of any message of the voice.  Its
value is any of the message priority symbols.

See the corresponding speechd-set-* functions for valid values of other
parameters.

The voice named nil is special, it defines a default voice.  Explicit
definition of its parameters is optional."
  :type `(repeat
          (cons
	   :tag "Voice"
           (speechd-voice-tag :tag "Voice")
	   (set
	    :tag "Parameters"
            ;;
	    (cons :tag "Language" (const :format "" language)
                  string)
            (cons :tag "Gender" (const :format "" gender)
                  (choice (const male) (const female) (const neutral)))
            (cons :tag "Age" (const :format "" age)
                  (choice (const middle-adult) (const child) (const neutral)))
            (cons :tag "Style" (const :format "" style)
                  (choice (const 1) (const 2) (const 3)))
            (cons :tag "SSIP voice identifier" (const :format "" name)
                  (choice (const "male1") (const "male2") (const "male3")
                          (const "female1") (const "female2") (const "female3")
                          (const "child_male") (const "child_female")
                          (string :tag "Other")))
            ;;
	    (cons :tag "Rate" (const :format "" rate)
                  integer)
	    (cons :tag "Pitch" (const :format "" pitch)
                  integer)
	    (cons :tag "Volume" (const :format "" volume)
                  integer)
            ;;
	    (cons :tag "Punctuation mode" (const :format "" punctuation-mode)
		  ,(speechd--generate-customization-options
		    speechd--punctuation-modes))
	    (cons :tag "Capital character mode"
		  (const :format "" capital-character-mode)
		  ,(speechd--generate-customization-options
		    speechd--capital-character-modes))
            ;;
	    (cons :tag "Message priority" (const :format "" message-priority)
		  (speechd-priority-tag :value text))
	    (cons :tag "Output module" (const :format "" output-module)
		  string))))
  :set #'speechd--set-variable-and-reopen
  :group 'speechd)

(defcustom speechd-connection-voices '()
  "Alist of connection names and corresponding voices.

Each element of the list is of the form (CONNECTION-NAME . VOICE-ID), where
CONNECTION-NAME is a string naming a connection and VOICE-ID is a voice
identifier defined in the variable `speechd-voices'.

For connections that are not specified here, the voice named nil is used.

You must reopen the connections to apply the changes to this variable."
  :type '(alist :key-type (string :tag "Connection name")
                :value-type (speechd-voice-tag :tag "Voice"))
  :set #'speechd--set-variable-and-reopen
  :group 'speechd)

(defcustom speechd-cancelable-connections '()
  "List of names of connections on which cancel operation applies by default."
  :type '(repeat (string :tag "Connection name"))
  :group 'speechd)

(defcustom speechd-face-voices '()
  "Alist mapping faces to voices.
Each of the alist element is of the form (FACE . VOICE) where FACE is a face
and string is a voice identifier defined in `speechd-voices'.  Each face is
spoken in the corresponding voice.
If there's no item for a given face in this variable, the face is spoken in the
current voice."
  :type '(alist :key-type face :value-type (speechd-voice-tag :tag "Voice"))
  :group 'speechd)


;;; External variables


(defvar speechd-spell nil
  "If non-nil, any spoken text is spelled.")


;;; Internal constants and configuration variables


(defconst speechd--buffer " *speechd*"
  "Name of the buffer associated with speechd connections.")

(defconst speechd--application-name "Emacs"
  "Name of the client as set to speechd.")

(defvar speechd--language-codes
  '(("czech" . "cs")
    ("english" . "en")
    ("french" . "fr")
    ("german" . "de")))

(defvar speechd--default-language (or (cdr (assoc current-language-environment
                                                  speechd--language-codes))
                                      "en"))

(defvar speechd--default-connection-parameters
  '(punctuation-mode some
    spelling-mode nil
    capital-character-mode none
    voice male1
    rate 0
    pitch 0
    volume 100
    index-marks t
    ssml-mode nil))

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
    (synthesizer-voice . "SYNTHESIS_VOICE")
    (rate . "RATE")
    (pitch . "PITCH")
    (volume . "VOLUME")
    (spelling-mode . "SPELLING")
    (index-marks . "NOTIFICATION INDEX_MARKS")
    (ssml-mode . "SSML_MODE")
    (output-module . "OUTPUT_MODULE")	; TODO: to be removed sometimes
    ))

(defconst speechd--list-parameters
  '((voices
     "VOICES" identity)
    (synthesis-voices
     "SYNTHESIS_VOICES" (lambda (line) (cl-first (split-string line))))))

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
     (icon . "icon"))
    (spelling-mode
     (t . "on")
     (nil . "off"))
    (index-marks
     (t . "on")
     (nil . "off"))
    (ssml-mode
     (t . "on")
     (nil . "off"))))

(defconst speechd--volatile-parameters '(output-module))


;;; Internal variables


(cl-defstruct speechd--connection
  name
  host
  port
  (failure-p nil)
  (last-try-time nil)
  process
  (process-output "")
  (paused-p nil)
  (in-block nil)
  (parameters ())
  (forced-priority nil)
  (last-command nil))

(cl-defstruct speechd--request
  string)

(defvar speechd--connections (make-hash-table :test 'equal)
  "Hash table mapping client names to `speechd-connection' instances.")

(defvar speechd--retry-time 1.0)

(defvar speechd-client-name)

(defvar speechd--current-connection)


;;; Utilities


(defmacro speechd--iterate-connections (&rest body)
  `(maphash #'(lambda (_ connection)
                (let ((speechd--current-connection connection))
                  ,@body))
            speechd--connections))

(defmacro speechd--iterate-clients (&rest body)
  `(maphash #'(lambda (name _)
                (let ((speechd-client-name name))
                  ,@body))
            speechd--connections))

(defun speechd-connection-names ()
  "Return the list of all present connection names."
  (let ((names '()))
    (speechd--iterate-clients
      (push speechd-client-name names))
    names))

(defmacro speechd--with-current-connection (&rest body)
  `(let ((speechd--current-connection (speechd--connection)))
     (when (and (speechd--connection-failure-p speechd--current-connection)
                (>= (- (float-time) (speechd--connection-last-try-time speechd--current-connection))
                    speechd--retry-time))
       (ignore-errors (speechd-reopen t))
       (setq speechd--current-connection (speechd--connection)))
     (unless (speechd--connection-failure-p speechd--current-connection)
       ,@body)))

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
        ($cparameters (gensym))
        ($p (gensym))
        ($v (gensym))
        ($orig-v (gensym))
        ($pv (gensym)))
    `(let* ((,$parameters ,parameters)
            (,$orig-parameters ()))
       (unwind-protect
           (let ((,$cparameters (speechd--connection-parameters speechd--current-connection)))
             (while ,$parameters
               (let* ((,$p (cl-first ,$parameters))
                      (,$v (cl-second ,$parameters))
                      (,$orig-v (plist-get ,$cparameters ,$p)))
                 (when (and (not (equal ,$v ,$orig-v))
                            (or ,$v
                                (not (memq ,$p '(language)))))
                   (when (and (plist-member ,$cparameters ,$p)
                              (not (memq ,$p '(message-priority))))
                     (push (cons ,$p ,$orig-v) ,$orig-parameters))
                   (speechd--set-parameter ,$p ,$v)))
               (setq ,$parameters (nthcdr 2 ,$parameters)))
             ,@body)
         (dolist (,$pv ,$orig-parameters)
           (speechd--set-parameter (car ,$pv) (cdr ,$pv)))))))

(defun speechd--voice-name (parameters)
  (or (cdr (assoc 'name parameters))
      (let ((gender (cdr (assoc 'gender parameters)))
            (age (cdr (assoc 'age parameters)))
            (style (cdr (assoc 'style parameters))))
        (format "%s%s%s"
                (if (eq age 'child) "child_" "")
                (if (eq gender 'female) "female" "male")
                (or style "1")))))

(defun speechd--voice-parameters (voice)
  (let* ((parameters (cdr (assoc voice speechd-voices)))
         (voice-name (speechd--voice-name parameters))
         (result (if voice-name (list 'voice voice-name) '())))
    (dolist (p parameters)
      (unless (memq (car p) '(name gender age style))
        (plist-put result (car p) (cdr p))))
    result))

(defmacro speechd--with-voice (voice &rest body)
  `(speechd--with-connection-parameters (speechd--voice-parameters ,voice)
     ,@body))

(defun speechd--current-language ()
  speechd-language)


;;; Process management functions


(cl-defun speechd--connection (&optional (name speechd-client-name) (create-if-needed t))
  (or (gethash name speechd--connections)
      (and create-if-needed
	   (let ((speechd-client-name name))
	     (speechd-open)))))

(defun speechd--jump-to-marker (index-mark)
  (let ((marker (cdr (assoc index-mark speechd--markers))))
    (when marker
      (let ((buffer (marker-buffer marker))
            (position (marker-position marker)))
        (when buffer
          (select-window (or (get-buffer-window buffer) (selected-window)))
          (unless (eq (window-buffer) buffer)
            (switch-to-buffer buffer))
          (goto-char position))))))

(defun speechd--process-notifications (output)
  ;; 700-msg_id
  ;; 700-client_id
  ;; 700-index_mark
  ;; 700 END
  (let ((notifications '())
        (pos 0))
    (while (string-match speechd--notification-regexp output pos)
      (push (list (match-string 1 output) (match-string 2 output) (match-string 3 output))
            notifications)
      (setq pos (match-end 0)))
    ;; There can be additional index marks inserted by Speech Dispatcher,
    ;; let's skip them and find the first meaningful index mark.
    (while notifications
      (cl-destructuring-bind (code separator _message) (car notifications)
        (setq notifications (cdr notifications))
        (when (and (equal code speechd--index-mark-code)
                   (equal separator " "))
          (when notifications
            (cl-destructuring-bind (code separator message) (car notifications)
              (when (and (equal code speechd--index-mark-code)
                         (equal separator "-"))
                (speechd--jump-to-marker message)
                (setq notifications nil))))))))
  output)

(defun speechd--process-filter (_process output)
  (speechd--with-current-connection
   (setf (speechd--connection-process-output speechd--current-connection)
         (speechd--process-notifications
          (concat (speechd--connection-process-output speechd--current-connection) output)))))

(defun speechd--open-connection (method host port socket-name)
  (let ((process
         (cond
          ((eq method 'unix-socket)
           (make-network-process
            :name "speechd" :family "local"
            :remote (or socket-name
                        (or (getenv "SPEECHD_SOCK")
                            (expand-file-name
			     (let ((runtime-dir (getenv "XDG_RUNTIME_DIR")))
			       (concat (if runtime-dir (concat runtime-dir "/") "~/.")
                                       "speech-dispatcher/speechd.sock")))))))
          ((eq method 'inet-socket)
           (open-network-stream "speechd" nil host port))
          (t (error "Invalid communication method: `%s'" method)))))
    (when process
      (set-process-coding-system process
				 speechd--coding-system
				 speechd--coding-system)
      (if (fboundp 'set-process-query-on-exit-flag)
	  (set-process-query-on-exit-flag process nil)
	(set-process-query-on-exit-flag process nil))
      (set-process-filter process #'speechd--process-filter))
    process))

(defun speechd--close-connection (connection)
  (let ((process (speechd--connection-process connection)))
    (when process
      (delete-process (speechd--connection-process connection))
      (setf (speechd--connection-process connection) nil))))

(defun speechd--send-connection (connection command)
  (let ((process (speechd--connection-process connection)))
    (when process
      (with-speechd-coding-protection
        (condition-case _
            (progn
              (process-send-string process command)
              (let ((output (speechd--connection-process-output connection)))
                (while (not (string-match speechd--end-regexp output))
                  (unless (accept-process-output process speechd-timeout nil 1)
                    (signal 'ssip-connection-error
                            "Timeout in communication with Speech Dispatcher"))
                  (setq output (speechd--connection-process-output connection)))
                (let ((pos (match-end 0)))
                  ;; If there are more responses, try to get in sync:
                  (while (string-match speechd--end-regexp output pos)
                    (setq output (substring output pos))
                    (setq pos (- (match-end 0) pos)))
                  (setf (speechd--connection-process-output connection)
                        (substring output pos))
                  (substring output 0 pos))))
          (error
           (speechd--permanent-connection-failure connection)
           (signal 'ssip-connection-error
                   "Error in communication with Speech Dispatcher")))))))

(put 'ssip-connection-error 'error-conditions
     '(error speechd-connection-error ssip-connection-error))
(put 'ssip-connection-error 'error-message
     "Error on opening Speech Dispatcher connection")

;;;###autoload
(cl-defun speechd-open (&optional method &key host port socket-name quiet force-reopen)
  "Open connection to Speech Dispatcher using the given method.
If the connection corresponding to the current `speechd-client-name' value
already exists, close it and reopen again, with the same connection parameters.

Available methods are `unix-socket' and `inet-socket' for communication
over Unix sockets and TCP sockets respectively.  Default is 'unix-socket'.

The key arguments HOST and PORT are only relevant to the `inet-socket'
communication method and identify the speechd server location.  They can
override default values stored in the variables `speechd-host' and
`speechd-port'.

The SOCKET-NAME argument is only relevant to the `unix-socket' communication
method and can override the default path to the Dispatcher's Unix socket for
the given user.

If the key argument QUIET is non-nil, don't report failures and quit silently.
If the key argument FORCE-REOPEN is non-nil, try to reopen an existent
connection even if it previously failed.

Return the opened connection on success, nil otherwise."
  (interactive)
  (let ((connection (gethash speechd-client-name speechd--connections)))
    (let ((method (or method speechd-connection-method))
	  (host (or host speechd-host))
	  (port (or port speechd-port)))
      (when connection
	(speechd--close-connection connection)
	(setq host (speechd--connection-host connection)
	      port (speechd--connection-port connection)))
      (when speechd-autospawn
        (when (eql (ignore-errors
                     (call-process "speech-dispatcher" nil nil nil "--spawn"))
                   0)
          (sleep-for 1)))
      (let* ((name speechd-client-name)
             (voice (cdr (assoc name speechd-connection-voices)))
             (default-parameters (append
                                  (speechd--voice-parameters voice)
                                  speechd--default-connection-parameters))
	     (parameters (cond
                          ((and connection force-reopen)
                           (append
                            default-parameters
                            (speechd--connection-parameters connection)))
                          (connection
                           (append
                            (speechd--connection-parameters connection)
                            default-parameters))
                          (t
                           default-parameters)))
             (connection-error nil)
	     (process (when (or (not connection)
		                (not (speechd--connection-failure-p connection))
		                force-reopen)
                        (condition-case err (speechd--open-connection
                                             (or method 'unix-socket)
                                             host port socket-name)
                          (file-error
                           (setq connection-error err)
                           nil)))))
        (when (or process (not quiet))
          (setq connection (make-speechd--connection
                            :name name :host host :port port :process process))
          (puthash name connection speechd--connections))
        (when (and (not process) (not quiet))
          (speechd--permanent-connection-failure connection)
          (signal 'ssip-connection-error connection-error))
	(when process
	  (speechd--set-connection-name name)
          (setq parameters (append parameters
                                   (list 'language speechd--default-language)))
          (let ((already-set '(client-name)))
            (while parameters
              (cl-destructuring-bind (parameter value . next) parameters
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

(cl-defun speechd-close (&optional (name speechd-client-name))
  "Close speechd connection named NAME."
  (interactive)
  (let ((connection (speechd--connection name nil)))
    (when connection
      (speechd--close-connection connection)
      (remhash name speechd--connections))))

(defun speechd-close-all ()
  "Close all speechd connections."
  (speechd--iterate-clients (speechd-close)))

(defun speechd-reopen (&optional quiet)
  "Close and open again all the connections to speechd.
If QUIET is non-nil, don't echo success report."
  (interactive)
  (let ((number-of-connections 0)
	(number-of-failures 0))
    (maphash #'(lambda (name _)
		 (let ((speechd-client-name name))
		   (if (speechd-open nil :quiet t :force-reopen t)
		       (cl-incf number-of-connections)
		     (cl-incf number-of-failures))))
	     speechd--connections)
    (unless quiet
      (message (format "%d connections successfully reopened, %d failures"
                       number-of-connections number-of-failures)))))


;;; Process communication functions


(defconst speechd--eol "\n")
(defconst speechd--end-regexp (format "^[0-6][0-9][0-9] .*%s" speechd--eol))
(defconst speechd--result-regexp
  (format "\\`[0-9][0-9][0-9]-\\(.*\\)%s" speechd--eol))
(defconst speechd--success-regexp
  (format "^[1-2][0-9][0-9] .*%s" speechd--eol))
(defconst speechd--notification-regexp
  (format "^\\(7[0-9][0-9]\\)\\([- ]\\)\\(.*\\)%s" speechd--eol))
(defconst speechd--index-mark-code "700")

(defun speechd--permanent-connection-failure (connection)
  (speechd--close-connection connection)
  (setf (speechd--connection-failure-p connection) t
        (speechd--connection-last-try-time connection) (float-time)
	(speechd--connection-paused-p connection) nil
	(speechd--connection-parameters connection) ()))

(defun speechd--send-request (request)
  (speechd--with-current-connection
   (save-match-data
     (let ((answer (speechd--send-connection
                    speechd--current-connection (speechd--request-string request))))
       (let ((data '())
             success)
         (while (and answer
                     (string-match speechd--result-regexp answer))
           (push (match-string 1 answer) data)
           (setq answer (substring answer (match-end 0))))
         (setq success (and answer
                            (string-match speechd--success-regexp answer)))
         (unless success
           (setq data nil))
         (list success
               data
               (and success (substring answer 0 3))
               (and success (substring answer 4))))))))

(defconst speechd--block-commands
  '(("speak")
    (".")
    ("sound_icon")
    ("char")
    ("key")
    ("quit")
    ("block" ("end"))
    ("set" ("self" ("rate") ("pitch") ("volume") ("voice") ("language")
                   ("punctuation") ("cap_let_recogn") ("ssml_mode")))))

(defun speechd--block-command-p (command &optional allowed)
  (unless allowed
    (setq allowed speechd--block-commands))
  (let* ((match (assoc (downcase (cl-first command)) allowed))
         (rest-allowed (cdr match)))
    (and match
         (or (not rest-allowed)
             (speechd--block-command-p (cl-rest command) rest-allowed)))))

(cl-defun speechd--send-command (command)
  (unless (listp command)
    (setq command (list command)))
  (speechd--with-current-connection
    (when (or (not (speechd--connection-in-block speechd--current-connection))
              (speechd--block-command-p command))
      (setf (speechd--connection-last-command speechd--current-connection) command)
      (speechd--send-request
       (make-speechd--request
        :string (concat (mapconcat #'identity command " ") speechd--eol))))))

(defun speechd--send-text (text)
  (when (cl-first (or (speechd--send-command "SPEAK") '(t)))
    ;; We must be careful here.  There is no answer from SSIP until all data
    ;; including the terminator is sent.  Thus, if we send the data in pieces,
    ;; there may be noticeable delays when waiting for TCP packet
    ;; acknowledgments.  So we send all data, including the final dot
    ;; terminator, in a single piece.
    (save-match-data
      (let ((i 0))
        (while (string-match "[\200-\377]" text i)
          (let ((char (string-to-char (match-string 0 text))))
	    (if (char-charset char '(eight-bit))
                (setq text (replace-match (format "\\%o" (encode-char char 'eight-bit)) t t text))
              (setq i (match-end 0))))))
      (let ((i 0))
        (while (and (> (length text) 0)
                    (string-match
                     "\\(\\`\\|\n\\)\\(\\(\\.\\).*\\)\\(\n\\|\\'\\)"
                     text i))
          (setq text (replace-match ".." nil nil text 3))
          (setq i (1+ (match-end 0))))))
    ;; We must remove text properties from the string, otherwise Emacs does
    ;; strange things when recoding non-ASCII characters to UTF-8.
    (set-text-properties 0 (length text) nil text)
    (unless (cl-first (or (speechd--send-request
                           (make-speechd--request
                            :string (concat text speechd--eol "." speechd--eol)))
                          '(t)))
      ;; We must reset the connection on failure, to bring it back to the
      ;; command state from the text reading state.
      (speechd-close))))


;;; Value retrieval functions


(defun speechd--list (parameter)
  (cl-multiple-value-bind (command processor)
      (cdr (assoc parameter speechd--list-parameters))
    (mapcar processor (cl-second (speechd--send-command (list "LIST" command))))))


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

(defun speechd--call-set-parameter (parameter value globally)
  (let ((p (or (cdr (assoc parameter speechd--parameter-names))
               (error "Invalid parameter name: `%s'" parameter)))
        (v (or (speechd--transform-parameter-value parameter value)
               (error "Invalid parameter value: %s=%s" parameter value))))
    (speechd--send-command (list "SET" (if globally "all" "self") p v))))

(defun speechd--set-connection-parameter (parameter value)
  (let* ((plist (speechd--connection-parameters speechd--current-connection))
         (orig-value (if (plist-member plist parameter)
                         (plist-get plist parameter)
                       'unknown)))
    (when (or (memq parameter speechd--volatile-parameters)
              (and (not (equal orig-value value))
                   (or
                    (not (eq parameter 'message-priority))
                    (not (speechd--connection-forced-priority speechd--current-connection)))))
      (let ((answer (speechd--call-set-parameter parameter value nil)))
        (setq speechd--current-connection (speechd--connection))
        (when (cl-first answer)
          (setf (speechd--connection-parameters speechd--current-connection)
                (plist-put (speechd--connection-parameters speechd--current-connection)
                           parameter value))))
      ;; Speech Dispatcher bug work-around
      (when (eq parameter 'language)
        (let ((output-module (plist-get
                              (speechd--connection-parameters speechd--current-connection)
                              'output-module)))
          (when output-module
            (speechd--set-connection-parameter 'output-module
                                               output-module)))))))

(defun speechd--set-parameter (parameter value &optional all)
  (if all
      (progn
        ;; We must iterate clients even on global all, to update speechd-el
        ;; connection parameters.
        (speechd--iterate-connections
         (speechd--set-connection-parameter parameter value))
        (when (consp all)
          (speechd--with-current-connection
           (speechd--call-set-parameter parameter value t))))
    (speechd--with-current-connection
     (speechd--set-connection-parameter parameter value))))

(defun speechd--set-connection-name (name)
  (speechd--set-parameter
   'client-name
   (format "%s:%s:%s" (user-login-name) speechd--application-name name)))

(defun speechd-set-language (language)
  "Set language of the current client connection to LANGUAGE.
Language must be an RFC 1766 language code, as a string."
  (interactive (list (read-string "Language: ")))
  (speechd--set-parameter 'language language))

(defun speechd-set-ssml-mode (value)
  "Set SSML mode value.
SSML is reset to \"off\" when connection is reopened."
  (speechd--set-parameter 'ssml-mode value))

(defmacro speechd--generate-set-command (parameter prompt argdesc)
  (let* ((prompt* (concat prompt ": "))
         (argdesc* (eval argdesc))
         (docstring
          (format "Set %s of the current connection.
VALUE must be %s.
If called with a prefix argument, set it for all connections."
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
            (value &optional all)
       ,docstring
       (interactive
	,(cond
	  ((integerp argdesc*)
	   (concat "n" prompt* "\nP"))
	  ((not argdesc*)
	   (concat "s" prompt* "\nP"))
          ((listp argdesc*)
            `(list
              (cdr (assoc (completing-read ,prompt* ,argdesc nil t) ,argdesc))
              current-prefix-arg))
	  (t
	   `(list
	     (completing-read ,prompt*
			      (mapcar #'list
                                      (speechd--list (quote ,argdesc*))))
             current-prefix-arg))))
       (when value
         (speechd--set-parameter (quote ,parameter) value all)
         (message "%s set to %s." ,prompt value)))))

(speechd--generate-set-command capital-character-mode "Capital character mode"
                               speechd--capital-character-modes)
(speechd--generate-set-command pitch "Pitch" 0)
(speechd--generate-set-command volume "Volume" 0)
(speechd--generate-set-command rate "Rate" 0)
(speechd--generate-set-command voice "Voice" 'voices)
(speechd--generate-set-command synthesizer-voice "Synthesizer voice" 'synthesis-voices)
(speechd--generate-set-command punctuation-mode "Punctuation mode"
                               speechd--punctuation-modes)
(speechd--generate-set-command pause-context "Pause context" 0)
;; TODO: Remove this one once proper output module setting is defined.
(speechd--generate-set-command output-module "Output module" nil)

(defun speechd-add-connection-settings (voice)
  "Add current connection and its settings to `speechd-connection-voices'."
  (interactive "SNew voice name: ")
  (if (or (not (assoc voice speechd-voices))
          (y-or-n-p "Voice already exists, replace it? "))
      (setq ;; the voice
            speechd-voices
            (cons
             (speechd--with-current-connection
              (cons voice
                    (let ((voice-parameters ())
                          (connection-parameters
                           (speechd--connection-parameters speechd--current-connection)))
                      (while connection-parameters
                        (cl-destructuring-bind (p v . next) connection-parameters
                          (unless (memq p '(client-name message-priority
                                            spelling-mode))
                            (push
                             (cons
                              (cl-case p
                                (voice 'name)
                                (t p))
                              (cl-case p
                                (punctuation-mode
                                 (cdr (assoc
                                       v speechd--punctuation-modes)))
                                (capital-character-mode
                                 (cdr (assoc
                                       v speechd--capital-character-modes)))
                                (t v)))
                             voice-parameters))
                          (setq connection-parameters next)))
                      (nreverse voice-parameters))))
             (remove (assoc speechd-client-name speechd-voices)
                     speechd-voices))
            ;; the connection
            speechd-connection-voices
            (cons (cons speechd-client-name voice)
                  (remove
                   (assoc speechd-client-name speechd-connection-voices)
                   speechd-connection-voices)))
    (message "Settings NOT added")))


;;; Blocks


(defun speechd-block (function &optional parameters)
  "Call FUNCTION inside an SSIP block.
FUNCTION is called without any arguments."
  (speechd--with-current-connection
    (let ((%block-connection speechd--current-connection)
          (nested (and speechd--current-connection
                       (speechd--connection-in-block speechd--current-connection))))
      (speechd--with-connection-parameters parameters
        (let ((speechd-client-name (speechd--connection-name speechd--current-connection)))
          (unless nested
            (speechd--send-command '("BLOCK BEGIN"))
            (when speechd--current-connection
              (setf (speechd--connection-in-block speechd--current-connection) t)))
          (unwind-protect (funcall function)
            (unless nested
              (let ((speechd--current-connection %block-connection))
                (when speechd--current-connection
                  (setf (speechd--connection-in-block speechd--current-connection) nil)
                  (speechd--send-command '("BLOCK END")))))))))))

(defmacro speechd-block* (parameters &rest body)
  "Set PARAMETERS and enclose BODY by an SSIP block.
Before invoking BODY, the BLOCK BEGIN command is sent, and the BLOCK END
command is sent afterwards.
PARAMETERS is a property list defining parameters to be set before sending the
BLOCK BEGIN command.  The property-value pairs correspond to the arguments of
the `speechd--set-parameter' function."
  `(speechd-block (lambda () ,@body) ,parameters))


;;; Speaking functions


(defconst speechd--index-mark-prefix "index-")
(defvar speechd--markers '())  ; assoc list of (INDEX . MARKER)

(defun speechd--ssml (text beg markers)
  (let ((end (1- (+ beg (length text)))))
    (dolist (m markers)
      (cl-destructuring-bind (pos index _marker) m
        (when (<= beg pos end)
          (let ((text-pos (- pos beg)))
            (put-text-property text-pos (1+ text-pos) 'index index text))))))
  (setq text (xml-escape-string text))
  (let ((pos 0))
    (while (setq pos (next-single-property-change pos 'index text))
      (let ((mark (format "<mark name=\"%s%s\"/>"
                          speechd--index-mark-prefix
                          (get-text-property pos 'index text))))
        (setq text (concat (substring text 0 pos) mark (substring text pos)))
        (setq pos (+ pos (length mark) 1)))))
  (concat "<speak>" text "</speak>"))

;;;###autoload
(cl-defun speechd-say-text (text &key (priority speechd-default-text-priority) say-if-empty markers)
  "Speak the given TEXT, represented by a string.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'.
The key argument SAY-IF-EMPTY is non-nil, TEXT is sent through SSIP even if it
is empty."
  (interactive "sText: ")
  (when (or say-if-empty
            (not (string= text "")))
    (cl-flet ((properties (point)
             (let ((voice (cdr (assq (get-text-property point 'face text)
                                     speechd-face-voices)))
                   (language (get-text-property point 'language text)))
               (append (when (stringp voice) (list 'voice voice))
                       (when language (list 'language language))
                       (when (and voice (symbolp voice))
                         (speechd--voice-parameters voice))))))
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
            (when markers
              (setq speechd--markers
                    (mapcar #'(lambda (m)
                                (cons (format "%s%s" speechd--index-mark-prefix
                                              (cl-second m))
                                      (cl-third m)))
                            markers))
              (setq substring (speechd--ssml substring beg markers)))
            (speechd-block* `(message-priority ,priority
                              language ,(speechd--current-language)
                              spelling-mode ,speechd-spell
                              ssml-mode ,(not (null markers))
                              ,@properties)
              (speechd--send-text substring))
            (setq beg end)))))))

(cl-defun speechd-say-sound (name &key (priority speechd-default-sound-priority))
  "Ask speechd to play an auditory icon.
NAME is the name of the icon, any string acceptable by speechd.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (speechd--set-parameter 'message-priority priority)
  (speechd--send-command (list "SOUND_ICON" name)))

(cl-defun speechd-say-char (char &key (priority speechd-default-char-priority))
  "Speak the given CHAR, any UTF-8 character.
The key argument `priority' defines the priority of the message and must be one
of the symbols `important', `message', `text', `notification' or
`progress'."
  (speechd--set-parameter 'message-priority priority)
  (speechd--with-current-connection
    (speechd--with-connection-parameters
        `(language ,(speechd--current-language))
      (speechd--send-command
       (list "CHAR" (format "%s" (cl-case char
                                   (?  "space")
                                   (?\n "linefeed")
                                   (t (char-to-string char)))))))))

(cl-defun speechd-say-key (key &key (priority speechd-default-key-priority))
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
                      ((= character ? ) "space")
                      ((= character ?_) "underscore")
                      ((= character ?\") "double-quote")
                      ((= character 127) "backspace")
                      (t (format "%c" character)))
                   (format "%s" character))))
    (dolist (m modifiers)
      (setq string (concat (symbol-name m) "_" string)))
    (speechd--set-parameter 'message-priority priority)
    (speechd--with-current-connection
      (speechd--with-connection-parameters
          `(language ,(speechd--current-language))
        (speechd--send-command (list "KEY" (format "%s" string)))))))


;;; Control functions


(defun speechd--control-command (command all &optional repeatable)
  (cond
   ((not all)
    (when (or repeatable
              (not (equal (cl-first (speechd--connection-last-command
                                     (speechd--connection)))
                          command)))
      (speechd--send-command (list command "self"))))
   ((eq all 'some)
    (let ((speechd-client-name$ speechd-client-name))
      (speechd--iterate-clients
       (when (or (equal speechd-client-name$ speechd-client-name)
                 (member speechd-client-name speechd-cancelable-connections))
         (speechd--control-command command nil repeatable)))))
   ((numberp all)
    (speechd--iterate-clients
     (speechd--control-command command nil repeatable)))
   (t
    (speechd--send-command (list command "all")))))

;;;###autoload
(defun speechd-cancel (&optional all)
  "Stop speaking all the messages sent through the current client so far.
If the universal argument is given, stop speaking messages of all clients.
If a numeric argument is given, stop speaking messages of all current Emacs
session clients.
If no argument is given, stop speaking messages of the current client and all
the clients of the current Emacs session named in
`speechd-cancelable-connections'."
  (interactive "P")
  (speechd--control-command "CANCEL" (or all 'some) t))

;;;###autoload
(defun speechd-stop (&optional all)
  "Stop speaking the currently spoken message (if any) of this client.
If the optional argument ALL is non-nil, stop speaking the currently spoken
messages of all clients."
  (interactive "P")
  (speechd--control-command "STOP" all t))

;;;###autoload
(defun speechd-pause (&optional all)
  "Pause speaking in the current client.
If the optional argument ALL is non-nil, pause speaking in all clients."
  (interactive "P")
  (if all
      (speechd--iterate-connections
        (setf (speechd--connection-paused-p speechd--current-connection) t))
    (speechd--with-current-connection
      (setf (speechd--connection-paused-p speechd--current-connection) t)))
  (speechd--control-command "PAUSE" (not (not all))))

;;;###autoload
(defun speechd-resume (&optional all)
  "Resume previously stopped speaking in the current client.
If the optional argument ALL is non-nil, resume speaking messages of all
clients."
  (interactive "P")
  (speechd--with-current-connection
    (when (or all (speechd--connection-paused-p speechd--current-connection))
      (speechd--control-command "RESUME" (not (not all)))
      (if all
          (setf (speechd--connection-paused-p speechd--current-connection) nil)
        (speechd--iterate-connections
          (setf (speechd--connection-paused-p speechd--current-connection) nil))))))

;;;###autoload
(defun speechd-repeat ()
  "Repeat the last message sent to speechd."
  (interactive)
  (if t
      (message "Not yet implemented in speechd")
    (let ((id (car (cl-second (speechd--send-command
                               '("HISTORY" "GET" "LAST"))))))
      (when id
        (speechd--send-command (list "HISTORY" "SAY" id))))))


;;; Announce

(provide 'speechd)


;;; speechd.el ends here
