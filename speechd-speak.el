;;; speechd-speak.el --- simple speechd-el based Emacs client  -*- lexical-binding: t -*-

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

;; This is an Emacs client to speechd.  Some ideas taken from the Emacspeak
;; package (http://emacspeak.sourceforge.net) by T. V. Raman.

;;; Code:


(require 'cl-lib)
(require 'wid-edit)

(require 'speechd)
(require 'speechd-common)
(require 'speechd-out)

(require 'speechd-brltty)
(require 'speechd-ssip)


;;; User options


(defgroup speechd-speak ()
  "Speechd-el user client customization."
  :group 'speechd-el)

(defcustom speechd-speak-echo 'character
  "Symbol determining how to read typed characters.
It can have one of the following values:
`character' -- read characters when they are typed
`word' -- read only whole words when they are written
`nil' -- don't echo anything on typing"
  :type '(choice (const :tag "Characters" character)
                 (const :tag "Words" word)
                 (const :tag "Nothing" nil))
  :group 'speechd-speak)

(defcustom speechd-speak-deleted-char t
  "If non-nil, speak the deleted char, otherwise speak the adjacent char."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-buffer-name 'text
  "If non-nil, speak buffer name on a buffer change.
If the value is the symbol `text', speak the text from the cursor position in
the new buffer to the end of line as well.  If nil, speak the text only, not
the buffer name."
  :type '(choice (const :tag "Buffer name and buffer text" text)
                 (const :tag "Buffer name" t)
                 (const :tag "Buffer text" nil))
  :group 'speechd-speak)

(defcustom speechd-speak-on-minibuffer-exit t
  "If non-nil, always try to speak something when exiting the minibuffer."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-auto-speak-buffers '("*Help*")
  "List of names of other-window buffers to speak if nothing else fits.
If nothing else is to be spoken after a command and a visible window in the
current frame displaying a buffer with a name contained in this list is
changed, the contents of the window buffer is spoken."
  :type '(repeat (string :tag "Buffer name"))
  :group 'speechd-speak)

(defcustom speechd-speak-force-auto-speak-buffers '()
  "List of names of other-window buffers to speak on visible changes.
Like `speechd-speak-auto-speak-buffers' except that the window content is
spoken even when there are other messages to speak."
  :type '(repeat (string :tag "Buffer name"))
  :group 'speechd-speak)

(defcustom speechd-speak-buffer-insertions 'one-line
  "Defines whether insertions in a current buffer should be read automatically.
The value is a symbol and can be from the following set:
- nil means don't speak them
- t means speak them all
- `one-line' means speak only first line of any change
- `whole-buffer' means speak whole buffer if it was changed in any way
Only newly inserted text is read, the option doesn't affect processing of
deleted text.  Also, the option doesn't affect insertions within commands
processed in a different way by speechd-speak or user definitions."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "One-line changes only" one-line)
                 (const :tag "Always" t)
                 (const :tag "Read whole buffer" whole-buffer))
  :group 'speechd-speak)

(defcustom speechd-speak-insertions-in-buffers
  '(" widget-choose" "*Choices*")
  "List of names of buffers, in which insertions are automatically spoken.
See also `speechd-speak-buffer-insertions'."
  :type '(repeat (string :tag "Buffer name"))
  :group 'speechd-speak)

(defcustom speechd-speak-priority-insertions-in-buffers '()
  "List of names of buffers, in which insertions are spoken immediately.
Unlike `speechd-speak-insertions-in-buffers', speaking is not delayed until a
command is completed.
This is typically useful in comint buffers."
  :type '(repeat (string :tag "Buffer name"))
  :group 'speechd-speak)

(defcustom speechd-speak-align-buffer-insertions t
  "If non-nil, read insertions aligned to the beginning of the first word."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-movement-on-insertions 'read-only
  "If t, speak the text around moved cursor even in modified buffers.
If nil, additional cursor movement doesn't cause speaking the text around the
new cursor position in modified buffers.
If `read-only', speak the text around cursor in read-only buffers only."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In read-only buffers" read-only))
  :group 'speechd-speak)

(defcustom speechd-speak-read-command-keys t
  "Defines whether command keys should be read after their command.
If t, always read command keys, before the command is performed.
If nil, never read them.
Otherwise it is a list, consisting of one or more of the following symbols:
`movement' -- read the keys if the cursor has moved without any buffer change
`modification' -- read the keys if the buffer was modified without moving the
  cursor
`modification-movement' -- read the keys if the buffer was modified and the
  cursor has moved
and the keys are read after the command is performed."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (set :tag "Sometimes"
                      (const movement)
                      (const modification)
                      (const modification-movement)))
  :group 'speechd-speak)

(defcustom speechd-speak-allow-prompt-commands t
  "If non-nil, allow speechd-speak commands in read-char prompts."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-ignore-command-keys
  '(forward-char backward-char right-char left-char next-line previous-line
    delete-char comint-delchar-or-maybe-eof delete-backward-char
    backward-delete-char-untabify delete-forward-char
    c-electric-backspace c-electric-delete-forward)
  "List of commands for which their keys are never read."
  :type '(repeat function)
  :group 'speechd-speak)

(defcustom speechd-speak-read-command-name nil
  "If non-nil, read command name instead of command keys."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-use-index-marks nil
  "If non-nil, move point over text in spoken buffers."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-index-mark-regexp ""
  "Regular expression defining the places where to put index marks to.
An index mark is put at the end of the regexp at each of the places.
If the regexp is empty, `sentence-end' is used.  If `sentence-end' is nil,
a default regexp matching common punctuation is used."
  :type 'regexp
  :group 'speechd-speak)

(defcustom speechd-speak-by-properties-on-movement t
  "Method of selection of the piece of text to be spoken on movement.
Unless a command provides its speechd feedback in a different way, it speaks
the current line by default if the cursor has moved.  However, if this variable
is t, it speaks the uniform text around the cursor, where \"uniform\"
means the maximum amount of text without any text property change.

If the variable is a list of faces, uniform text is spoken only when the cursor
is on one of the named faces.

Speaking uniform text only works if font-lock-mode is enabled for the current
buffer.

See also `speechd-speak-by-properties-always' and
`speechd-speak-by-properties-never'."
  :type '(choice (const :tag "Always" t)
                 (repeat :tag "On faces" (face :tag "Face")))
  :group 'speechd-speak)

(defcustom speechd-speak-by-properties-always '()
  "List of commands to always speak by properties on movement.
The elements of the list are command names, symbols.

See `speechd-speak-by-properties-on-movement' for more information about
property speaking."
  :type '(repeat
          (function :tag "Command" :match (lambda (w val) (commandp val))))
  :group 'speechd-speak)

(defcustom speechd-speak-by-properties-never '()
  "List of commands to never speak by properties on movement.
The elements of the list are command names, symbols.

See `speechd-speak-by-properties-on-movement' for more information about
property speaking."
  :type '(repeat
          (function :tag "Command" :match (lambda (w val) (commandp val))))
  :group 'speechd-speak)

(defcustom speechd-speak-faces '()
  "Alist of faces and speaking functions.
Each element of the list is of the form (FACE . ACTION).
If a movement command leaves the cursor on a FACE and there is no explicit
speaking bound to the command, ACTION is invoked.

If ACTION is a string, the string is spoken.
If ACTION is a function, it is invoked, with no arguments."
  :type '(alist
          :key-type face
          :value-type (choice
                       (string :tag "String to speak")
                       (function :tag "Function to call"
                                 :match (lambda (w val) (commandp val)))))
  :group 'speechd-speak)

(defcustom speechd-speak-display-modes '(gnus-agent-mode)
  "List of minor modes to be spoken by their display string rather than name."
  :type '(repeat symbol)
  :group 'speechd-speak)

(defcustom speechd-speak-whole-line nil
  "If non-nil, speak whole line on movement by default.
Otherwise speak from the point to the end of line on movement by default."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-separator-regexp nil
  "If non-nil don't read parts of a line separated by the given regexp.
This is typically useful in w3m where columns are separated only
by whitespace.  \"   \" may be good value of this variable in such
a case."
  :type 'regexp
  :group 'speechd-speak)

(defcustom speechd-speak-message-time-interval 30
  "Minimum time in seconds, after which the same message may be repeated.
If the message is the same as the last one, it is not spoken unless the number
of seconds defined here has passed from the last spoken message."
  :type 'integer
  :group 'speechd-speak)

(defcustom speechd-speak-connections '()
  "Alist mapping major modes and buffers to speechd connection.
By default, there's a single connection to speechd, named \"default\".  This
variable can define special connections for particular major modes and buffers.

Each element of the alist is of the form (MODE-OR-BUFFER . CONNECTION-NAME).

MODE-OR-BUFFER may be, in the order of preference from the highest to the
lowest:

- a list, representing a function call returning non-nil iff the element should
  be applied
- buffer name
- the symbol `:minibuffer', representing minibuffers
- major mode symbol
- nil, representing non-buffer areas, e.g. echo area
- t, representing the default value if nothing else matches

CONNECTION-NAME is an arbitrary non-empty string naming the corresponding
connection.  If connection with such a name doesn't exist, it is automatically
created."
  :type '(alist :key-type
                (choice :tag "Matcher" :value nil
                        (const :tag "Default" t)
                        (const :tag "Non-buffers" nil)
                        (const :tag "Minibuffer" :value :minibuffer)
                        (symbol :tag "Major mode" :value fundamental-mode)
                        (regexp :tag "Buffer name regexp" :value "")
                        (restricted-sexp :tag "Function call"
                                         :match-alternatives (listp)))
                :value-type (string :tag "Connection name"))
  :group 'speechd-speak)

(defcustom speechd-speak-signal-events
  '(empty whitespace beginning-of-line end-of-line start finish minibuffer
    message)
  "List of symbolic names of events to signal with a standard icon.
The following actions are supported: `empty', `whitespace', `beginning-of-line',
`end-of-line', `start', `finish', `minibuffer', `message'."
  :type '(set (const empty)
              (const whitespace)
              (const beginning-of-line)
              (const end-of-line)
              (const start)
              (const finish)
              (const minibuffer)
              (const message))
  :group 'speechd-speak)

(defcustom speechd-speak-input-method-languages '()
  "Alist mapping input methods to languages.
Each of the alist element is of the form (INPUT-METHOD-NAME . LANGUAGE), where
INPUT-METHOD-NAME is a string naming the input method and LANGUAGE is an ISO
language code accepted by SSIP.
If the current input method is present in the alist, the corresponding language
is selected unless overridden by another setting."
  :type '(alist :key-type (string :tag "Input method")
                :value-type (string :tag "Language code"))
  :group 'speechd-speak)

(defcustom speechd-speak-emacs-language "en"
  "Language to use for texts originating from Emacs.
Messages, minibuffer prompts, completions often contain English texts,
so it is desirable to speak them in English.  This variable allows to
speak them in English, in another language or in the current language.
The value is an ISO language code string accepted by SSIP.
If nil, use the current language."
  :type '(choice (string :tag "Language code")
                 (const :tag "Current" nil))
  :group 'speechd-speak)

(defcustom speechd-speak-in-debugger t
  "If nil, speechd-speak functions won't speak in Elisp debuggers.
This may be useful when debugging speechd-el itself."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-prefix "\C-e"
  "Default prefix key used for speechd-speak commands."
  :set (lambda (name value)
         (set-default name value)
         (speechd-speak--build-mode-map))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'speechd-speak)

(defcustom speechd-speak-hook nil
  "Hook run in the `speechd-speak' function."
  :options '((lambda () (require 'speechd-ssip))
             (lambda () (require 'speechd-brltty)))
  :type 'hook
  :group 'speechd-speak)


;;; Internal constants and variables


(defconst speechd-speak--c-buffer-name "*Completions*")

(defvar speechd-speak--info)


;;; Debugging support


(defvar speechd-speak--debug ())
(defvar speechd-speak--max-debug-length 12)
(defvar speechd-speak--debug-all nil)
(defvar speechd-speak--debug-mark nil)

(defun speechd-speak--debug (info &optional optional)
  (when (or (not optional) speechd-speak--debug-all)
    (setq speechd-speak--debug
          (cons info
                (if (and (not speechd-speak--debug-all)
                         (>= (length speechd-speak--debug)
                             speechd-speak--max-debug-length))
                    (butlast speechd-speak--debug)
                  speechd-speak--debug)))))


;;; Control functions


(defvar speechd-speak--predefined-rates
  '((1 . -100)
    (2 . -75)
    (3 . -50)
    (4 . -25)
    (5 . 0)
    (6 . 25)
    (7 . 50)
    (8 . 75)
    (9 . 100)))
(defun speechd-speak-set-predefined-rate (level)
  "Set speech rate to one of nine predefined levels.
Level 1 is the slowest, level 9 is the fastest."
  (interactive "nSpeech rate level (1-9): ")
  (setq level (min (max level 1) 9))
  (let ((rate (cdr (assoc level speechd-speak--predefined-rates))))
    (speechd-set-rate rate)
    (message "Speech rate set to %d" rate)))

(defvar speechd-speak--char-to-number
  '((?1 . 1) (?2 . 2) (?3 . 3) (?4 . 4) (?5 . 5)
    (?6 . 6) (?7 . 7) (?8 . 8) (?9 . 9)))
(defun speechd-speak-key-set-predefined-rate ()
  "Set speech rate to one of nine predefined levels via a key binding.
Level 1 is the slowest, level 9 is the fastest."
  (interactive)
  (let ((level (cdr (assoc last-input-event speechd-speak--char-to-number))))
    (when level
      (speechd-speak-set-predefined-rate level))))


;;; Supporting functions and options


(defvar speechd-speak-command-done nil
  "When non-nil, no post command speaking is performed, except for keys.
This variable is reset to nil before each command in pre-command-hook.")

(defun speechd-speak--name (&rest args)
  (intern (mapconcat #'symbol-name args "-")))

(defvar speechd-speak-mode nil)   ; forward definition to make everything happy

(defvar speechd-speak--started nil)

(defvar speechd-speak--last-buffer-mode t)
(defvar speechd-speak--last-connection-name nil)
(defvar speechd-speak--last-connections nil)
(defvar speechd-speak--default-connection-name "default")
(defvar speechd-speak--special-area nil)
(defvar speechd-speak--emulate-minibuffer nil)
(defvar speechd-speak--client-name-set nil)
(make-variable-buffer-local 'speechd-speak--client-name-set)
(defun speechd-speak--connection-name ()
  (let ((buffer-mode (if speechd-speak--special-area
                         nil
                       (cons major-mode (buffer-name)))))
    (cond
     (speechd-speak--client-name-set
      speechd-client-name)
     ((and (not speechd-speak--client-name-set)
           (eq speechd-speak-connections speechd-speak--last-connections)
           (equal buffer-mode speechd-speak--last-buffer-mode))
      speechd-speak--last-connection-name)
     (t
      (setq speechd-speak--last-buffer-mode buffer-mode
            speechd-speak--last-connections speechd-speak-connections
            speechd-speak--last-connection-name
            (if buffer-mode
                (or (cdr (or
                          ;; minibuffer-like prompts
                          (and speechd-speak--emulate-minibuffer
                               (assoc :minibuffer speechd-speak-connections))
                          ;; functional test
                          (let ((specs speechd-speak-connections)
                                (result nil))
                            (while (and (not result) specs)
                              (if (and (consp (caar specs))
                                       (eval (caar specs)))
                                  (setq result (car specs))
                                (setq specs (cdr specs))))
                            result)
                          ;; buffer name
                          (let ((buffer-name (buffer-name)))
                            (cl-assoc-if (lambda (key)
                                           (when (stringp key) (string-match key buffer-name)))
                                         speechd-speak-connections))
                          ;; minibuffer
                          (and (speechd-speak--in-minibuffer-p)
                               (assoc :minibuffer speechd-speak-connections))
                          ;; major mode
                          (assq major-mode speechd-speak-connections)
                          ;; default
                          (assq t speechd-speak-connections)))
                    speechd-speak--default-connection-name)
              (or (cdr (assq nil speechd-speak-connections))
                  speechd-speak--default-connection-name)))
      (set (make-local-variable 'speechd-client-name)
           speechd-speak--last-connection-name)))))

(defun speechd-speak--in-debugger ()
  (and (not speechd-speak-in-debugger)
       (or (eq major-mode 'debugger-mode)
           (and (boundp 'edebug-active) edebug-active))))

(defmacro speechd-speak--maybe-speak* (&rest body)
  `(when (and speechd-speak-mode
              (not (speechd-speak--in-debugger)))
     ,@body))

(defmacro speechd-speak--maybe-speak (&rest body)
  `(speechd-speak--maybe-speak*
     (let ((speechd-client-name (speechd-speak--connection-name))
           (speechd-language
            (or (and speechd-speak-input-method-languages
                     current-input-method
                     (cdr (assoc current-input-method
                                 speechd-speak-input-method-languages)))
                speechd-language)))
       ,@body)))

(defmacro speechd-speak--interactive (&rest body)
  `(let ((speechd-speak-mode (or (called-interactively-p 'interactive)
                                 (and speechd-speak-mode
                                      (not (speechd-speak--in-debugger)))))
         (speechd-default-text-priority (if (called-interactively-p 'interactive)
                                            'message
                                          speechd-default-text-priority)))
     ,@body))

(defun speechd-speak--remove-invisible-text (text)
  (let ((pieces '())
        (max (length text))
        (pos 0)
        (invisibility-spec (if (eq buffer-invisibility-spec t)
                               t
                             (mapcar (lambda (s) (if (consp s) (car s) s))
                                     buffer-invisibility-spec))))
    (while (and pos (< pos max))
      (let ((next-pos (next-single-char-property-change pos 'invisible text)))
        (when (let ((invisibility (get-char-property pos 'invisible text)))
                (not (cond
		      ((eq invisibility-spec t)
		       invisibility)
		      ((consp invisibility)
		       (cl-intersection invisibility-spec invisibility))
		      (t
		       (memq invisibility buffer-invisibility-spec)))))
          (push (substring text pos next-pos) pieces))
        (setq pos next-pos)))
    (apply 'concat (nreverse pieces))))

(defun speechd-speak--text (text &rest args)
  (speechd-speak--maybe-speak
   ;; TODO: replace repeating patterns
   ;; TODO: handle selective display
   (setq text (speechd-speak--remove-invisible-text text))
   (when speechd-speak--debug-mark
     (speechd-speak--debug (cons speechd-speak--debug-mark text) t))
   (apply #'speechd-out-text text args)))

(defun speechd-speak--char (char &rest args)
  (speechd-speak--maybe-speak
   (apply #'speechd-out-char char args)))

(defvar speechd-speak--last-report "")

(defun speechd-speak-report (message &rest args)
  "Output text or icon MESSAGE.
If MESSAGE is a non-empty string, it is the text to output.  If it is a non-nil
symbol present in the `speechd-speak-signal-events' variable, it is the icon to
output.  Otherwise nothing happens.

ARGS are appended to the arguments of the corresponding speaking
function (`speechd-out-text' or `speechd-out-icon') without change after the
message argument."
  (speechd-speak--maybe-speak
   (when (and message
              (not (string= message ""))
              (not (equal message speechd-speak--last-report))
              (or (stringp message)
                  (memq message speechd-speak-signal-events)))
     (apply (if (symbolp message) #'speechd-out-icon #'speechd-out-text)
            message args)
     (setq speechd-speak--last-report message))))

(defun speechd-speak-read-char (&optional char)
  "Output character CHAR.
If CHAR is nil, output the character just after current point."
  (interactive)
  (speechd-speak--interactive
   (speechd-speak--char (or char (following-char)))))

(defun speechd-speak-read-region (&optional beg end empty-text index-marks)
  "Output region of the current buffer between BEG and END.
If BEG is nil, current mark is used instead.
If END is nil, current point is used instead.
EMPTY-TEXT is a text to output if the region is empty; if nil, empty text icon
is output.
If INDEX-MARKS is non-nil, insert index marks to the spoken text."
  (interactive "r")
  (speechd-speak--interactive
   (let* ((beg (or beg (mark)))
          (end (or end (point)))
          (text (speechd-speak--buffer-substring beg end)))
     (cond
      ((string= text "")
       (speechd-speak-report (or empty-text 'empty)
                             :priority speechd-default-text-priority))
      ((save-match-data (string-match "\\`[ \t]+\\'" text))
       (speechd-speak-report 'whitespace
                             :priority speechd-default-text-priority))
      (t
       (let* ((point (point))
              (cursor (and (>= point beg) (<= point end) (- (point) beg)))
              (markers (when index-marks (speechd-speak--markers beg end))))
         (speechd-speak--text text :cursor cursor :markers markers)))))))

(defun speechd-speak-read-line (&optional rest-only)
  "Output current line.
If the prefix argument is given, output the line only from the current point
to the end of the line."
  (interactive "P")
  (speechd-speak--interactive
   (let* ((inhibit-field-text-motion t)
          (in-isearch-p (and isearch-mode (not (called-interactively-p 'interactive))))
          (beg (if (and rest-only (not in-isearch-p)) (point) (line-beginning-position)))
          (end (if truncate-lines
                   (line-end-position)
                 (save-excursion (end-of-visual-line) (point)))))
     (when speechd-speak-separator-regexp
       (save-match-data
         (save-excursion
           (when (re-search-backward speechd-speak-separator-regexp beg t)
             (setq beg (match-end 0))))
         (save-excursion
           (when (re-search-forward speechd-speak-separator-regexp end t)
             (setq end (match-beginning 0))))))
     (speechd-speak-read-region beg end
                                (when (speechd-speak--in-minibuffer-p) "")))))

(defun speechd-speak-read-next-line ()
  "Speak the next line after the current line.
If there is no such line, play the empty text icon."
  (interactive)
  (speechd-speak--interactive
   (save-excursion
     (if (= (forward-line 1) 0)
         (speechd-speak-read-line)
       (speechd-speak-report 'empty)))))

(defun speechd-speak-read-previous-line ()
  "Speak the previous line before the current line.
If there is no such line, play the empty text icon."
  (interactive)
  (speechd-speak--interactive
   (save-excursion
     (if (= (forward-line -1) 0)
         (speechd-speak-read-line)
       (speechd-speak-report 'empty)))))

(defun speechd-speak-read-buffer (&optional buffer)
  "Read BUFFER.
If BUFFER is nil, read current buffer."
  (interactive)
  (speechd-speak--interactive
   (save-excursion
     (when buffer
       (set-buffer buffer))
     (speechd-speak-read-region (point-min) (point-max) nil
                                speechd-speak-use-index-marks))))

(defun speechd-speak-read-rest-of-buffer ()
  "Read current buffer from the current point to the end of the buffer."
  (interactive)
  (speechd-speak--interactive
   (speechd-speak-read-region (point) (point-max) nil
                              speechd-speak-use-index-marks)))

(defun speechd-speak-read-rectangle (beg end)
  "Read text in the region-rectangle."
  (interactive "r")
  (speechd-speak--interactive
   (speechd-speak--text
    (mapconcat #'identity (extract-rectangle beg end) "\n"))))

(defun speechd-speak-read-other-window ()
  "Read buffer of the last recently used window."
  (interactive)
  (speechd-speak--interactive
   (speechd-speak-read-buffer (window-buffer (get-lru-window)))))

(defun speechd-speak-read-mode-line ()
  "Read mode line.
This function works only in Emacs 22 or higher."
  (interactive)
  (when (fboundp 'format-mode-line)
    (speechd-speak--interactive
     (speechd-speak--text (format-mode-line mode-line-format t)))))

(defun speechd-speak--window-contents ()
  (sit-for 0)                           ; to update window start and end
  (speechd-speak-read-region (window-start) (window-end)))

(defun speechd-speak--uniform-text-around-point ()
  (let ((beg (speechd-speak--previous-property-change (1+ (point))))
	(end (speechd-speak--next-property-change (point))))
    (speechd-speak-read-region beg end)))

(defun speechd-speak--speak-piece (start)
  (let ((point (point)))
    (if (> (count-lines start point) 1)
	(speechd-speak-read-line)
      (speechd-speak-read-region start point))))

(defun speechd-speak--speak-current-column ()
  (speechd-speak--text (format "Column %d" (current-column))))

(defmacro speechd-speak--def-speak-object (type)
  (let* ((function-name (speechd-speak--name 'speechd-speak-read type))
	 (backward-function (speechd-speak--name 'backward type))
	 (forward-function (speechd-speak--name 'forward type)))
    `(defun ,function-name ()
       ,(format "Speak current %s." type)
       (interactive)
       (speechd-speak--interactive
        (save-excursion
          (let* ((point (point))
                 (end (progn (,forward-function 1) (point)))
                 (beg (progn (,backward-function 1) (point))))
            (when (<= (progn (,forward-function 1) (point)) point)
              (setq beg end))
            (speechd-speak-read-region beg end)))))))

(speechd-speak--def-speak-object word)
(speechd-speak--def-speak-object sentence)
(speechd-speak--def-speak-object paragraph)
(speechd-speak--def-speak-object page)
(speechd-speak--def-speak-object sexp)

(cl-defstruct speechd-speak--command-info-struct
  marker
  modified
  (changes '())
  (change-end nil)
  (deleted-chars nil)
  (other-changes '())
  other-changes-buffer
  other-window
  other-buffer-modified
  completion-buffer-modified
  minibuffer-contents
  info)

(defmacro speechd-speak--cinfo (slot)
  `(,(speechd-speak--name 'speechd-speak--command-info-struct slot)
    speechd-speak--info))

(defun speechd-speak--command-info-struct-buffer (_info)
  (let ((marker (speechd-speak--cinfo marker)))
    (and marker (marker-buffer marker))))

(defun speechd-speak--command-info-struct-point (_info)
  (let ((marker (speechd-speak--cinfo marker)))
    (and marker (marker-position marker))))

(defvar speechd-speak--command-start-info (make-vector 5 nil))

(defmacro speechd-speak--with-minibuffer-depth (&rest body)
  `(let ((depth (minibuffer-depth)))
     (when (>= depth (length speechd-speak--command-start-info))
       (setq speechd-speak--command-start-info
	     (vconcat speechd-speak--command-start-info
		      (make-vector
		       (- (1+ depth)
			  (length speechd-speak--command-start-info))
		       nil))))
     ,@body))

(defun speechd-speak--in-minibuffer-p ()
  (window-minibuffer-p (selected-window)))

(defun speechd-speak--command-start-info ()
  (speechd-speak--with-minibuffer-depth
    (aref speechd-speak--command-start-info depth)))

(defun speechd-speak--set-command-start-info (&optional reset)
  (speechd-speak--with-minibuffer-depth
    (aset speechd-speak--command-start-info depth
	  (if reset
	      nil
	    (ignore-errors
	      (let ((other-window (next-window)))
		(make-speechd-speak--command-info-struct
                 :marker (point-marker)
		 :modified (buffer-modified-tick)
		 :other-window other-window
		 :other-buffer-modified
                   (and other-window
                        (buffer-modified-tick (window-buffer other-window)))
                 :completion-buffer-modified
                   (let ((buffer (get-buffer speechd-speak--c-buffer-name)))
                     (and buffer (buffer-modified-tick buffer)))
                 :minibuffer-contents
                   (if (speechd-speak--in-minibuffer-p)
                       (minibuffer-contents)
                     'unset)
                 :info (speechd-speak--current-info)
                )))))))

(defun speechd-speak--reset-command-start-info ()
  (speechd-speak--set-command-start-info t))

(defmacro speechd-speak--with-command-start-info (&rest body)
  `(let ((speechd-speak--info (speechd-speak--command-start-info)))
     (when speechd-speak--info
       ,@body)))

(defmacro speechd-speak--defadvice (function class &rest body)
  (let* ((function* function)
         (functions (if (listp function*) function* (list function*)))
         (class* class)
         (where (intern (concat ":" (symbol-name class*))))
         (aroundp (eq class* 'around))
         (lambda-list (if aroundp '(orig-fun &rest args) '(&rest args)))
         (aname (if (listp function*) 'speechd-speak 'speechd-speak-user)))
    `(progn
       ,@(mapcar
          (lambda (fname)
            `(define-advice ,fname (,where ,lambda-list ,aname)
               (cl-flet ,(when aroundp `((call-orig-func () (apply orig-fun args))))
                 (if (not speechd-speak--started)
                     ,(if aroundp
                          `(call-orig-func)
                        'args)  ;; not nil, to silence warnings about unused `args'
                   ,@body))))
          functions))))

(defmacro speechd-speak--report (feedback &rest args)
  (if (stringp feedback)
      `(speechd-speak-report ,feedback ,@args)
    feedback))

(defmacro speechd-speak-function-feedback (function position feedback)
  "Report FEEDBACK on each invocation of FUNCTION.
FUNCTION is a function name.
POSITION may be one of the symbols `before' (the feedback is run before the
function is invoked) or `after' (the feedback is run after the function is
invoked.
FEEDBACK is a string or a sexp.  If it is a string, it is simply passed as an
argument to `speechd-speak-report'.  If it is a sexp then it is evaluated as it
is when the feedback is run; the expression must itself deliver something to
output (by using speechd-speak-report or other function)."
  `(speechd-speak--defadvice ,(list function) ,position
     (speechd-speak--report ,feedback :priority 'message)))

(defmacro speechd-speak-command-feedback (function position feedback)
  "Report FEEDBACK on each invocation of FUNCTION.
The arguments are the same as in `speechd-speak-function-feedback'.
Unlike `speechd-speak-function-feedback', the feedback is reported only when
FUNCTION is invoked interactively."
  `(speechd-speak--defadvice ,(list function) ,position
     (when (called-interactively-p 'interactive)
       (speechd-speak--report ,feedback :priority 'message))))

(defmacro speechd-speak--command-feedback (commands position &rest body)
  (let ((commands* (if (listp commands) commands (list commands)))
	(position* position)
	(body* `(progn (speechd-speak--reset-command-start-info) ,@body)))
    `(progn
       ,@(mapcar #'(lambda (command)
		     `(speechd-speak--defadvice ,command ,position*
			,(if (eq position* 'around)
			     `(if (called-interactively-p 'interactive)
				  ,body*
				(call-orig-func))
			   `(when (called-interactively-p 'interactive)
			      ,body*))))
		 commands*)
       ,(unless (eq position 'before)
          `(setq speechd-speak-command-done t)))))

(defmacro speechd-speak--command-feedback-region (commands)
  `(speechd-speak--command-feedback ,commands around
     (let ((start (point)))
       (prog1 (call-orig-func)
         (speechd-speak--speak-piece start)))))

(cl-defun speechd-speak--next-property-change (&optional (point (point)) (limit (point-max)))
  (next-char-property-change point limit))

(cl-defun speechd-speak--previous-property-change (&optional (point (point)) (limit (point-min)))
  ;; Let's be careful about isearch overlays not to cut texts in isearch
  (let* ((i-overlays (and isearch-mode
                          (cl-intersection (overlays-at (- point 2))
                                           (append (list isearch-overlay)
                                                   isearch-opened-overlays
                                                   isearch-lazy-highlight-overlays))))
         (i-overlays-start (1- (apply #'min (1+ point) (mapcar #'overlay-start i-overlays)))))
    (when (>= i-overlays-start limit)
      ;; This may omit other property borders as well.  But in isearch we
      ;; should retain enough context anyway.
      (setq point i-overlays-start))
    (previous-char-property-change point limit)))

(defmacro speechd-speak--with-updated-text (&rest body)
  `(speechd-out-with-updated-text (speechd-speak--updated-text)
     ,@body))

(defun speechd-speak-set-language (language)
  "Set default language to LANGUAGE.
Language must be an RFC 1766 language code, as a string."
  (interactive "sLanguage: ")
  (speechd-set-language language)
  (setq speechd-language language))

(defun speechd-speak--language (&optional special)
  (or (and special speechd-speak-emacs-language) speechd-language))

(defun speechd-speak--in-comint-p ()
  "Return non-nil if the current buffer is any sort of a comint buffer."
  (and (boundp 'comint-accum-marker)
       comint-accum-marker))

(defvar speechd-speak--marker-counter 0)

(defun speechd-speak--markers (beg end)
  ;; Return list of (POSITION INDEX MARKER)
  (let ((regexp speechd-speak-index-mark-regexp)
        (markers '()))
    (when (string= regexp "")
      (setq regexp (or sentence-end "[.,;!?]+")))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp end t)
        (push (list (cl-incf speechd-speak--marker-counter) (point-marker))
              markers)))
    (mapcar #'(lambda (m) (cons (- (marker-position (cl-second m)) beg) m))
            markers)))


;;; Basic speaking

(speechd-speak--command-feedback (next-line previous-line) after
  (speechd-speak-read-line (not speechd-speak-whole-line)))

(speechd-speak--command-feedback (forward-word backward-word) after
  (speechd-speak-read-word))

(speechd-speak--command-feedback (forward-sentence backward-sentence) after
  (speechd-speak-read-sentence))

(speechd-speak--command-feedback (forward-paragraph backward-paragraph) after
  (speechd-speak-read-paragraph))

(speechd-speak--command-feedback (forward-page backward-page) after
  (speechd-speak-read-page))

(speechd-speak--command-feedback (scroll-up scroll-down) after
  (speechd-speak--window-contents))

(speechd-speak--command-feedback-region
 (backward-sexp forward-sexp forward-list backward-list up-list
  backward-up-list down-list))

(speechd-speak--command-feedback (upcase-word downcase-word capitalize-word)
				 after
  (speechd-speak-read-word))

(defun speechd-speak--store-deleted-chars (text)
  (speechd-speak--with-command-start-info
    (setf (speechd-speak--cinfo deleted-chars) text)))

(speechd-speak--defadvice (delete-backward-char backward-delete-char) around
  (if (speechd-speak--command-start-info)
      (speechd-speak--with-command-start-info
        (let ((speechd-speak--deleted-chars
               (when speechd-speak-deleted-char
                 (speechd-speak--buffer-substring
                  (max (- (point) (or (cl-first args) 1))
                       (point-min))
                  (point)))))
          (apply orig-fun args)
          (speechd-speak--store-deleted-chars (if speechd-speak-deleted-char
                                                  speechd-speak--deleted-chars
                                             (format "%c" (preceding-char))))))
    (call-orig-func)))

(speechd-speak--defadvice (delete-forward-char delete-char) around
  (let ((speechd-speak--deleted-chars
         (when speechd-speak-deleted-char
           (speechd-speak--buffer-substring
            (point)
            (min (+ (point) (or (cl-first args) 1))
                 (point-max))))))
    (call-orig-func)
    (speechd-speak--store-deleted-chars (if speechd-speak-deleted-char
                                            speechd-speak--deleted-chars
                                          (format "%c" (following-char))))))

(speechd-speak--command-feedback (quoted-insert) after
  (speechd-speak-read-char (preceding-char)))

(speechd-speak--command-feedback (newline newline-and-indent) before
  (speechd-speak-read-line))

(speechd-speak--command-feedback (undo) after
  (speechd-speak-read-line))


;;; Killing and yanking


(speechd-speak--command-feedback (kill-word) before
  (speechd-speak-read-word))

(speechd-speak--command-feedback (backward-kill-word) before
  (save-excursion
    (forward-word -1)
    (speechd-speak-read-word)))

(speechd-speak--command-feedback (kill-line) before
  (speechd-speak-read-line t))

(speechd-speak--command-feedback (kill-sexp) before
  (speechd-speak-read-sexp))

(speechd-speak--command-feedback (kill-sentence) before
  (speechd-speak-read-sentence))

(speechd-speak--command-feedback (zap-to-char) after
  (speechd-speak-read-line))

(speechd-speak--command-feedback (yank yank-pop) after
  (speechd-speak-read-region))

(speechd-speak--command-feedback (kill-region completion-kill-region) around
  (let ((nlines (count-lines (region-beginning) (region-end))))
    (prog1 (call-orig-func)
      (speechd-speak--maybe-speak*
        (message "Killed region containing %s lines" nlines)))))


;;; Messages


(defvar speechd-speak--last-message "")
(defvar speechd-speak--last-spoken-message "")
(defvar speechd-speak--last-spoken-message-time 0)

(defun speechd-speak-last-message ()
  "Speak last message from the echo area."
  (interactive)
  (speechd-speak--interactive
   (let ((speechd-speak-input-method-languages nil)
         (speechd-language (speechd-speak--language t)))
     (speechd-speak--text speechd-speak--last-message))))

(defun speechd-speak--read-message (message)
  (let ((speechd-speak--special-area t))
    (speechd-speak--maybe-speak         ; necessary to select proper connection
      (speechd-speak--minibuffer-prompt message :icon 'message
                                        :priority 'progress))))

(defun speechd-speak--message (message &optional reset-last-spoken)
  (speechd-speak--maybe-speak*
   (when (and message
              (or (not (string= message speechd-speak--last-spoken-message))
                  (>= (- (float-time) speechd-speak--last-spoken-message-time)
                      speechd-speak-message-time-interval)))
     (let* ((oldlen (length speechd-speak--last-spoken-message))
            (len (length message))
            ;; The following tries to handle answers to y-or-n questions, e.g.
            ;; "Do something? (y or n) y", which are reported as a single
            ;; string, including the prompt.
            (message* (if (and (= (- len oldlen) 1)
                               (save-match-data
                                 (string-match "\?.*) .$" message))
                               (string= (substring message 0 oldlen)
                                        speechd-speak--last-spoken-message))
                          (substring message oldlen)
                        message)))
       (setq speechd-speak--last-message message
             speechd-speak--last-spoken-message message
             speechd-speak--last-spoken-message-time (float-time))
       (speechd-speak--read-message message*)))
   (when reset-last-spoken
     (setq speechd-speak--last-spoken-message ""))))

(defun speechd-speak--current-message (&optional reset-last-spoken)
  (speechd-speak--message (current-message) reset-last-spoken))

(speechd-speak--defadvice message after
  (speechd-speak--current-message))

(defmacro speechd-speak--unhide-message (function)
  ;; If `message' is invoked within a built-in function, there's no way to get
  ;; notified automatically about it.  So we have to wrap the built-in
  ;; functions displaying messages to check for the otherwise hidden messages.
  `(speechd-speak--defadvice ,function after
     (unless (string= (current-message) speechd-speak--last-message)
       (speechd-speak--current-message))))

(speechd-speak--unhide-message write-region)


;;; Minibuffer


(defvar speechd-speak--minibuffer-inherited-language nil)

(speechd-speak--defadvice read-from-minibuffer around
  (let ((speechd-speak--minibuffer-inherited-language
         (and (cl-seventh args) speechd-language)))
    (call-orig-func)))

(defun speechd-speak--prompt (prompt &optional no-icon)
  (speechd-speak--text prompt :priority 'message
                       :icon (unless no-icon 'minibuffer)))

(defun speechd-speak--speak-minibuffer-prompt ()
  (speechd-speak--with-command-start-info
   ;; Discard changes, otherwise they would be read *after* the prompt, perhaps
   ;; confusing the user
   (setf (speechd-speak--cinfo changes) '()))
  (let ((speechd-language (speechd-speak--language t))
        (speechd-speak-input-method-languages nil))
    (speechd-speak--prompt (minibuffer-prompt)))
  (speechd-speak--prompt (minibuffer-contents) t))

(defun speechd-speak--minibuffer-setup-hook ()
  (set (make-local-variable 'speechd-language)
       speechd-speak--minibuffer-inherited-language)
  (speechd-speak--enforce-speak-mode)
  (speechd-speak--with-command-start-info
   (setf (speechd-speak--cinfo minibuffer-contents) (minibuffer-contents)))
  (speechd-speak--speak-minibuffer-prompt))

(defun speechd-speak--minibuffer-exit-hook ()
  (speechd-speak--with-command-start-info
   (setf (speechd-speak--cinfo minibuffer-contents) 'unset)))

(defun speechd-speak--speak-minibuffer ()
  (speechd-speak--text (minibuffer-contents)))

(defvar speechd-speak--last-other-changes "")
(defvar speechd-speak--last-other-changes-buffer nil)

(defun speechd-speak--read-changes (text buffer &rest speak-args)
  (with-current-buffer (or (get-buffer buffer) (current-buffer))
    (apply #'speechd-speak--text text speak-args)))

(defun speechd-speak--read-other-changes ()
  (speechd-speak--with-command-start-info
   (when (speechd-speak--cinfo other-changes)
     (let ((speechd-speak--special-area nil)
           (buffer (get-buffer (speechd-speak--cinfo other-changes-buffer)))
           (text (mapconcat
                  #'identity
                  (nreverse (speechd-speak--cinfo other-changes)) "")))
       (setq speechd-speak--last-other-changes text
             speechd-speak--last-other-changes-buffer buffer)
       (speechd-speak--read-changes text buffer :priority 'message))
     (setf (speechd-speak--cinfo other-changes) '()))))

(defun speechd-speak-last-insertions ()
  "Speak last insertions read in buffers with automatic insertion speaking.
This command applies to buffers defined in
`speechd-speak-insertions-in-buffers' and
`speechd-speak-priority-insertions-in-buffers'."
  (interactive)
  (speechd-speak--interactive
   (let ((speechd-speak--special-area nil))
     (speechd-speak--read-changes speechd-speak--last-other-changes
                                  speechd-speak--last-other-changes-buffer))))

(defun speechd-speak--minibuffer-prompt (prompt &rest args)
  (speechd-speak--read-other-changes)
  (let ((speechd-language (speechd-speak--language t))
        (speechd-speak-input-method-languages nil))
    (apply #'speechd-speak--text prompt args)))

(speechd-speak--command-feedback minibuffer-message after
  (speechd-speak--minibuffer-prompt (cl-first args) :priority 'notification))

;; Some built-in functions, reading a single character answer, prompt in the
;; echo area.  They don't invoke minibuffer-setup-hook and may put other
;; messages to the echo area by invoking other built-in functions.  There's no
;; easy way to catch the prompts and messages, the only way to deal with this
;; is to use idle timers.
(defvar speechd-speak--message-timer nil)
(defun speechd-speak--message-timer ()
  (let ((message (current-message)))
    (when (and cursor-in-echo-area
               (not (string= message speechd-speak--last-spoken-message)))
      (let ((speechd-speak--emulate-minibuffer t))
        (speechd-speak--minibuffer-prompt message))
      (setq speechd-speak--last-spoken-message message))))

;; The following functions don't invoke `minibuffer-setup-hook' and don't put
;; the cursor into the echo area.  Sigh.
(speechd-speak--defadvice read-key-sequence before
  (let ((prompt (cl-first args))
        (speechd-speak--emulate-minibuffer t))
    (when prompt
      (speechd-speak--minibuffer-prompt prompt :priority 'message))))
(speechd-speak--defadvice read-event before
  (let ((prompt (cl-first args)))
    (when prompt
      (let ((speechd-speak--emulate-minibuffer t)
            (speechd-language (speechd-speak--language (not (cl-second args)))))
        (speechd-speak--minibuffer-prompt prompt :priority 'message)))))


;;; Repetition in character reading commands


(defvar speechd-speak-read-char-keymap (make-sparse-keymap)
  "Keymap used by speechd-el for repetitions during reading characters.
Only single characters are allowed in the keymap.")
(define-key speechd-speak-read-char-keymap
  "\C-a" 'speechd-speak-last-insertions)
(define-key speechd-speak-read-char-keymap
  "\C-e" 'speechd-speak-last-message)

(speechd-speak--defadvice read-char-exclusive around
  (if speechd-speak-allow-prompt-commands
      (let ((char nil))
        (while (not char)
          (setq char (call-orig-func))
          (let ((command (lookup-key speechd-speak-read-char-keymap
                                     (vector char))))
            (when command
              (setq char nil)
              (call-interactively command))))
        char)
    (call-orig-func)))


;;; Commands


(defun speechd-speak--updated-text ()
  (let ((line-beg (line-beginning-position))
        (line-end (line-end-position)))
    (make-speechd-out-update :text (speechd-speak--buffer-substring line-beg line-end)
                             :cursor (- (point) line-beg) :group 'line)))

(defun speechd-speak--command-keys (&optional priority)
  (speechd-speak--maybe-speak
    (let* ((keys-to-output '())
           (text-to-output nil)
           (command-keys (append (this-command-keys-vector) nil)))
      (while (and command-keys (not text-to-output))
        (let ((key (car command-keys)))
          (push key keys-to-output)
          (setq text-to-output
                (and (equal (event-basic-type key) ?x)
                     (equal (event-modifiers key) '(meta))
                     (apply #'concat
                            (mapcar #'(lambda (k)
                                        (let ((km (event-modifiers k))
                                              (kb (event-basic-type k)))
                                          (if (or (not (numberp kb))
                                                  (< kb 32) (>= kb 128)
                                                  km)
                                              (char-to-string kb)
                                            "")))
                                    (cdr command-keys)))))
          (setq command-keys (cdr command-keys))))
      (speechd-out-keys (nreverse keys-to-output) :text text-to-output
                        :priority priority))))

(defun speechd-speak--add-command-text (beg end)
  (let ((last (cl-first (speechd-speak--cinfo changes)))
        (last-end (speechd-speak--cinfo change-end))
        (text (speechd-speak--buffer-substring beg end t)))
    (setf (speechd-speak--cinfo change-end) end)
    (cond
     ((and last (string= last text))
      ;; nothing to do
      )
     ((and last-end (= last-end beg))
      (rplaca (speechd-speak--cinfo changes)
              (concat last (speechd-speak--buffer-substring beg end t))))
     (t
      (push text (speechd-speak--cinfo changes))))))

(defun speechd-speak--buffer-substring (beg end &optional maybe-align-p)
  (let ((text (buffer-substring
               (if (and maybe-align-p
                        speechd-speak-align-buffer-insertions
                        (not (eq this-command 'self-insert-command)))
                   (save-excursion
                     (goto-char beg)
                     (when (save-match-data
                             (and (looking-at "\\w")
                                  (not (looking-at "\\<"))))
                       (backward-word 1))
                     (point))
                 beg)
               end)))
    (dolist (o (overlays-in beg end))
      (let ((face (overlay-get o 'face))
            (invisible (overlay-get o 'invisible)))
        (when (or face invisible)
          (let ((beg* (max (overlay-start o) beg))
                (end* (min (overlay-end o) end)))
            (add-text-properties (- beg* beg) (- end* beg)
                                 `(face ,face invisible ,invisible) text)))))
    text))

(defun speechd-speak--minibuffer-update-report (old new)
  (speechd-speak--add-command-text
   (+ (minibuffer-prompt-end)
      (if (and (<= (length old) (length new))
               (string= old (substring new 0 (length old))))
          (length old)
        0))
   (point-max)))

(defun speechd-speak--minibuffer-update ()
  (speechd-speak--with-command-start-info
   (let ((old-content (speechd-speak--cinfo minibuffer-contents))
         (new-content (minibuffer-contents)))
     (unless (or (eq old-content 'unset)
                 (string= old-content new-content))
       (setf (speechd-speak--cinfo minibuffer-contents) new-content)
       (speechd-speak--minibuffer-update-report old-content new-content)))))

(defun speechd-speak--read-buffer-change (buffer-name text)
  (with-current-buffer (or (get-buffer buffer-name)
                           (get-buffer "*scratch*")
                           (current-buffer))
    (speechd-speak--text text :priority 'message)))

(defvar speechd-speak--current-change-string nil)
(defun speechd-speak--before-change-hook (beg end)
  ;; Beware, before-change-functions may be sometimes called for several times
  ;; with different arguments (an Emacs bug?)
  (unless speechd-speak--current-change-string
    (setq speechd-speak--current-change-string
          (buffer-substring-no-properties beg end))))
(defun speechd-speak--after-change-hook (beg end _len)
  (speechd-speak--enforce-speak-mode)
  (speechd-speak--with-command-start-info
    (unless (or (= beg end)
                ;; Avoid reading changes when only text properties have changed
                (equal speechd-speak--current-change-string
                       (buffer-substring-no-properties beg end)))
      (cond
       ((or (member (buffer-name) speechd-speak-priority-insertions-in-buffers)
            (speechd-speak--in-comint-p)
            ;; Asynchronous buffer changes
            (and (not this-command)
                 (member (buffer-name) speechd-speak-insertions-in-buffers)))
        (unless (memq this-command '(self-insert-command quoted-insert newline
                                     newline-and-indent undo yank yank-pop))
          ;; Don't align buffer substring here so that the last word of comint
          ;; input is not read
          (speechd-speak--read-buffer-change
           (buffer-name) (speechd-speak--buffer-substring beg end nil))))
       ((not this-command)
        ;; Asynchronous buffer change -- we are not interested in it by
        ;; default
        nil)
       ((member (buffer-name) speechd-speak-insertions-in-buffers)
        (setf (speechd-speak--cinfo other-changes-buffer) (buffer-name))
        (push (speechd-speak--buffer-substring beg end t)
              (speechd-speak--cinfo other-changes)))
       ((eq (current-buffer) (speechd-speak--cinfo buffer))
        (if (speechd-speak--in-minibuffer-p)
            (progn
              (speechd-speak--read-other-changes)
              (speechd-speak--minibuffer-update))
          (speechd-speak--add-command-text beg end))))))
  (setq speechd-speak--current-change-string nil))

(defconst speechd-speak--dont-cancel-on-commands
  '(speechd-speak speechd-unspeak
    speechd-out-cancel speechd-out-stop speechd-out-pause speechd-out-resume))

(defun speechd-speak--pre-command-hook ()
  (condition-case err
      (progn
        (unless (or (memq this-command speechd-speak--dont-cancel-on-commands)
                    (and (eq this-command 'self-insert-command)
                         (eq speechd-speak-echo 'word)))
          (speechd-out-cancel))
        (speechd-speak--set-command-start-info)
        (setq speechd-speak--last-report "")
        (setq speechd-speak-command-done nil)
        (when speechd-speak-spell-command
          (speechd-speak-spell-mode 1))
        (speechd-speak--maybe-speak
          (when (and (eq speechd-speak-read-command-keys t)
                     (not (eq this-command 'self-insert-command))
                     (not (memq this-command
                                speechd-speak-ignore-command-keys)))
            (speechd-speak--command-keys 'message))
          ;; Some parameters of interactive commands don't set up the
          ;; minibuffer, so we have to speak the prompt in a special way.
          (let ((interactive (and (commandp this-command)
                                  (cadr (interactive-form this-command)))))
            (save-match-data
              (when (and (stringp interactive)
                         (string-match "^[@*]*\\([eipPmnr]\n\\)*[ckK]\\(.+\\)"
                                       interactive))
                (speechd-speak--prompt (match-string 2 interactive)))))))
    (error
     (speechd-speak--debug (list 'pre-command-hook-error err))
     (signal (car err) (cdr err))))
  (add-hook 'pre-command-hook 'speechd-speak--pre-command-hook))

(defmacro speechd-speak--post-defun (name shy new-state guard &rest body)
  (let* ((name (speechd-speak--name 'speechd-speak--post-read name))
         (state-condition (cl-case shy
                            ((t) `(eq state nil))
                            (sometimes `(not (eq state t)))
                            (t t))))
    `(defun ,name (state buffer-changed buffer-modified point-moved
                   in-minibuffer other-buffer)
       ;; Avoid compiler warnings about unused variables
       (list buffer-changed buffer-modified point-moved in-minibuffer other-buffer)
       (if (and ,state-condition
                ,guard)
           (let ((speechd-speak--debug-mark (quote ,name)))
             ,@body
             ,new-state)
         state))))

(speechd-speak--post-defun special-commands t t
  ;; Speak commands that can't speak in a regular way
  (and (not speechd-speak-command-done)
       (memq this-command '(forward-char backward-char right-char left-char)))
  (speechd-speak--with-updated-text
    (cond
     ((looking-at "^")
      (speechd-speak--char (following-char) :icon 'beginning-of-line))
     ((looking-at "$")
      (speechd-speak-report 'end-of-line
                            :priority speechd-default-char-priority))
     (t
      (speechd-speak-read-char)))))

(speechd-speak--post-defun buffer-switch t t
  ;; Any buffer switch
  (and buffer-changed (not speechd-speak-command-done))
  (when speechd-speak-buffer-name
    (speechd-speak--text (buffer-name) :priority 'message))
  (when (memq speechd-speak-buffer-name '(text nil))
    (speechd-speak-read-line t)))

(speechd-speak--post-defun deleted-chars t t
  ;; Reading delete-char etc. interactive feedback
  (let ((deleted-chars (speechd-speak--cinfo deleted-chars))
        (changes (speechd-speak--cinfo changes)))
    (and (stringp deleted-chars)
         (or (null changes)
             (and (= (length changes) 1)
                  (equal (cl-first changes) deleted-chars)))))
  (when (eq speechd-speak-read-command-keys t)
    ;; Cancel reading the DEL key etc. -- perhaps too daring?
    (speechd-out-cancel))
  (speechd-speak--with-updated-text
   (let ((deleted-chars (speechd-speak--cinfo deleted-chars)))
     (if (= (length deleted-chars) 1)
         (speechd-speak-read-char (string-to-char deleted-chars))
       (speechd-speak--text deleted-chars)))))

(speechd-speak--post-defun command-keys t nil
  ;; Keys that invoked the command
  (and (not (memq this-command speechd-speak-ignore-command-keys))
       (not (eq this-command 'self-insert-command))
       (not (eq speechd-speak-read-command-keys t))
       (or (and buffer-modified point-moved
                (memq 'modification-movement speechd-speak-read-command-keys))
           (and buffer-modified (not point-moved)
                (memq 'modification speechd-speak-read-command-keys))
           (and (not buffer-modified) point-moved
                (memq 'movement speechd-speak-read-command-keys))))
  (if speechd-speak-read-command-name
      (speechd-speak--text (symbol-name this-command) :priority 'message)
    (speechd-speak--command-keys 'message)))

(speechd-speak--post-defun command-done t t
  ;; No speaking when the command has already been handled in a special way
  speechd-speak-command-done)

(speechd-speak--post-defun info-change t nil
  ;; General status information has changed
  (not (equal (speechd-speak--cinfo info) (speechd-speak--current-info)))
  (let ((old-info (speechd-speak--cinfo info))
        (new-info (speechd-speak--current-info)))
    (dolist (item new-info)
      (let* ((id (car item))
             (new (cdr item))
             (old (cdr (assq id old-info))))
        (when (and (memq id speechd-speak-state-changes)
                   (not (equal old new)))
          (funcall (speechd-speak--name 'speechd-speak--update id)
                   old new))))))

(speechd-speak--post-defun speaking-commands nil t
  ;; Avoid additional reading on speaking commands
  (let ((command-name (symbol-name this-command))
        (prefix "speechd-speak-read-"))
    (and (> (length command-name) (length prefix))
         (string= (substring command-name 0 (length command-name)) prefix))))

(speechd-speak--post-defun buffer-modifications t
    (cond
     ((eq this-command 'self-insert-command) t)
     ((not speechd-speak-movement-on-insertions) t)
     ((and (eq speechd-speak-movement-on-insertions 'read-only)
           (not buffer-read-only))
      t)
     (t 'insertions))
  ;; Any buffer modification, including completion, abbrev expansions and
  ;; self-insert-command
  buffer-modified
  ;; We handle self-insert-command in a special way.  We don't speak the
  ;; inserted character itself, we only read other buffer modifications caused
  ;; by the command (typically abbrev expansions).  Instead of speaking the
  ;; inserted character, we try to speak the command key.
  (let ((self-insert (eq this-command 'self-insert-command))
        (changes (speechd-speak--cinfo changes)))
    (when speechd-speak-buffer-insertions
      (let ((text (mapconcat #'identity
                             (funcall (if self-insert
                                          #'butlast #'identity)
                                      (reverse changes))
                             " ")))
        (when (and self-insert
                   (> (length (cl-first changes)) 1))
          (setq text (concat text " " (cl-first changes))))
        (cond
         ((and (eq speechd-speak-buffer-insertions 'whole-buffer)
               (not self-insert))
          (speechd-speak-read-buffer))
         (t
          (speechd-speak--text (if (eq speechd-speak-buffer-insertions t)
                                   text
                                 (save-match-data
                                   (string-match "^.*$" text)
                                   (match-string 0 text))))))))
    (when (and self-insert
               speechd-speak-echo
               (not (memq 'self-insert-command
                          speechd-speak-ignore-command-keys)))
      (cl-case speechd-speak-echo
        (word
         (let ((point (point)))
           (when (and (> point 1)
                      (not (save-match-data (string-match "\\w" (buffer-substring (1- point) point)))))
             (speechd-speak--text (buffer-substring (save-excursion (forward-word -1) (point)) point)))))
        (character
         (speechd-speak--with-updated-text
          (speechd-speak--command-keys 'notification)))))))

(speechd-speak--post-defun completions t t
  ;; *Completions* buffer
  (and in-minibuffer
       (get-buffer speechd-speak--c-buffer-name)
       (/= (speechd-speak--cinfo completion-buffer-modified)
           (buffer-modified-tick (get-buffer speechd-speak--c-buffer-name))))
  (with-current-buffer speechd-speak--c-buffer-name
      (let ((speechd-language (speechd-speak--language t))
            (speechd-speak-input-method-languages nil))
        (goto-char (point-min))
        (save-match-data
          (re-search-forward "\n\n+" nil t))
        (speechd-speak-read-region (point) (point-max) nil))))

(speechd-speak--post-defun special-face-movement sometimes
    (or (not (stringp (cdr (assq (get-char-property (point) 'face)
                                 speechd-speak-faces))))
        state)
  ;; Special face hit
  (and (not in-minibuffer)
       point-moved
       (assq (get-char-property (point) 'face) speechd-speak-faces))
  (let ((action (cdr (assq (get-char-property (point) 'face)
                           speechd-speak-faces))))
    (cond
     ((stringp action)
      (speechd-speak--text action :priority 'message))
     ((functionp action)
      (ignore-errors
        (funcall action))))))

(speechd-speak--post-defun text-property-movement sometimes t
  ;; General text or overlay property hit
  (and (not in-minibuffer)
       point-moved
       (not (memq this-command speechd-speak-by-properties-never))
       (or (eq speechd-speak-by-properties-on-movement t)
           (memq this-command speechd-speak-by-properties-always)
           (memq (get-char-property (point) 'face)
                 speechd-speak-by-properties-on-movement))
       (or (let ((properties (text-properties-at (point)))
                 (look-props '(face font-lock-face mouse-face display help-echo
                               keymap local-map read-only field))
                 (marked nil))
             (while (and look-props (not marked))
               (setq marked (plist-member properties (car look-props)))
               (setq look-props (cdr look-props)))
             marked)
           (overlays-at (point)))
       (let ((position (speechd-speak--cinfo point)))
         (or (> (speechd-speak--previous-property-change
                 (1+ (point)) position)
                position)
             (<= (speechd-speak--next-property-change
                  (point) (1+ position))
                 position))))
  (speechd-speak--uniform-text-around-point))

(speechd-speak--post-defun plain-movement sometimes t
  ;; Other kinds of movement
  point-moved
  (speechd-speak-read-line (not speechd-speak-whole-line)))

(speechd-speak--post-defun other-window-event t 'other-window
  ;; Something interesting in other window
  (and (not in-minibuffer)
       other-buffer
       (member (buffer-name other-buffer) speechd-speak-auto-speak-buffers)
       (or (not (eq (next-window) (speechd-speak--cinfo other-window)))
           (not (= (buffer-modified-tick other-buffer)
                   (speechd-speak--cinfo other-buffer-modified)))))
  (speechd-speak-read-buffer (window-buffer (next-window))))

(speechd-speak--post-defun other-window-buffer nil t
  ;; Other window buffer is very interesting
  (and (not (eq state 'other-window))
       (not in-minibuffer)
       other-buffer
       (member (buffer-name other-buffer)
               speechd-speak-force-auto-speak-buffers)
       (or (not (eq (next-window) (speechd-speak--cinfo other-window)))
           (not (= (buffer-modified-tick other-buffer)
                   (speechd-speak--cinfo other-buffer-modified)))))
  (speechd-speak-read-buffer (window-buffer (next-window))))

(speechd-speak--post-defun minibuffer-exit t t
  (and speechd-speak-on-minibuffer-exit
       (/= (minibuffer-depth) speechd-speak--last-minibuffer-depth))
  (speechd-speak-read-line t))

(defvar speechd-speak--last-minibuffer-depth 0)

(defvar speechd-speak--post-command-speaking-defaults
  '(speechd-speak--post-read-special-commands
    speechd-speak--post-read-buffer-switch
    speechd-speak--post-read-deleted-chars
    speechd-speak--post-read-command-keys
    speechd-speak--post-read-command-done
    speechd-speak--post-read-info-change
    speechd-speak--post-read-speaking-commands
    speechd-speak--post-read-buffer-modifications
    speechd-speak--post-read-completions
    speechd-speak--post-read-special-face-movement
    speechd-speak--post-read-text-property-movement
    speechd-speak--post-read-plain-movement
    speechd-speak--post-read-other-window-event
    speechd-speak--post-read-other-window-buffer
    speechd-speak--post-read-minibuffer-exit))
(defvar speechd-speak--post-command-speaking nil)

(defun speechd-speak--post-command-hook ()
  (speechd-speak--enforce-speak-mode)
  (when (and speechd-speak-spell-command speechd-speak-spell-mode)
    ;; Only in spell mode to avoid disabling it after speechd-speak-spell
    (setq speechd-speak-spell-command nil)
    (speechd-speak-spell-mode 0))
  ;; Now, try to speak something useful
  (speechd-speak--maybe-speak
    (condition-case err
        (progn
          ;; Messages should be handled by an after change function.
          ;; Unfortunately, in Emacs 21 after change functions in the
          ;; *Messages* buffer don't work in many situations.  This is a
          ;; property of the Emacs implementation, so the mechanism can't be
          ;; used.
          (speechd-speak--current-message t)
          (speechd-speak--with-command-start-info
           (let* ((state nil)
                  (buffer-changed (not (eq (speechd-speak--cinfo buffer)
                                           (current-buffer))))
                  (buffer-modified (and (not buffer-changed)
                                        (/= (speechd-speak--cinfo modified)
                                            (buffer-modified-tick))))
                  (point-moved (and (not buffer-changed)
                                    (not (= (speechd-speak--cinfo point)
                                            (point)))))
                  (in-minibuffer (speechd-speak--in-minibuffer-p))
                  (other-window (next-window))
                  (other-buffer (let* ((buffer (and other-window
                                                    (window-buffer
                                                     other-window))))
                                  (unless (eq buffer (current-buffer))
                                    buffer))))
             (dolist (f speechd-speak--post-command-speaking)
               (let ((new-state state))
                 (condition-case err
                     (setq new-state (funcall f state buffer-changed
                                              buffer-modified point-moved
                                              in-minibuffer other-buffer))
                   (error
                    (speechd-speak--debug
                     (list 'post-command-hook-error f err))
                    (setq speechd-speak--post-command-speaking
                          (remove f speechd-speak--post-command-speaking))))
                 (setq state new-state)))
             (setq speechd-speak--last-minibuffer-depth (minibuffer-depth)))))
        (error
         (speechd-speak--debug (list 'post-command-hook-top-error err))
         (signal (car err) (cdr err))))
    (add-hook 'post-command-hook 'speechd-speak--post-command-hook)))


;;; Comint


(speechd-speak--command-feedback comint-show-output after
  (speechd-speak-read-region))


;;; Completions, menus, etc.


(defun speechd-speak--speak-completion ()
  ;; Taken from `choose-completion'
  (let (beg end completion)
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
    (if (null beg)
	(error "No completion here"))
    (setq beg (previous-single-property-change beg 'mouse-face))
    (setq end (or (next-single-property-change end 'mouse-face) (point-max)))
    (setq completion (speechd-speak--buffer-substring beg end))
    (speechd-speak--text completion)
    (speechd-speak--reset-command-start-info)))

(speechd-speak--command-feedback (next-completion previous-completion) after
  (speechd-speak--speak-completion))

(speechd-speak--command-feedback choose-completion before
  (speechd-speak--speak-completion))

(speechd-speak--defadvice widget-choose around
  (let ((widget-menu-minibuffer-flag (or speechd-speak-mode
                                         widget-menu-minibuffer-flag)))
    (call-orig-func)))


;;; Other functions and packages


(speechd-speak--command-feedback (occur-prev occur-next
				  occur-mode-goto-occurence)
				 after
  (speechd-speak-read-line))

(speechd-speak--command-feedback transpose-chars after
  (speechd-speak--char (following-char)))

(speechd-speak--command-feedback transpose-lines after
  (speechd-speak-read-line))

(speechd-speak--command-feedback transpose-words after
  (speechd-speak-read-word))

(speechd-speak--command-feedback transpose-sexps after
  (speechd-speak-read-sexp))

(speechd-speak--command-feedback undefined after
  (speechd-speak--text "No command on this key"))

(speechd-speak--command-feedback indent-for-tab-command after
  (speechd-speak--speak-current-column))

(defvar speechd-speak--handle-braille-keys nil)
(defun speechd-speak-toggle-braille-keys ()
  (interactive)
  (speechd-out-set 'brltty-accept-keys (not speechd-speak--handle-braille-keys))
  (setq speechd-speak--handle-braille-keys (not speechd-speak--handle-braille-keys))
  (message "Braille keys handled by %s" (if speechd-speak--handle-braille-keys "speechd-el" "BrlTTY")))


;;; Spelling


(define-minor-mode speechd-speak-spell-mode
  "Toggle spelling.
When the mode is enabled, all spoken text is spelled."
  nil " Spell" nil
  (set (make-local-variable 'speechd-spell) speechd-speak-spell-mode))

(defvar speechd-speak-spell-command nil)

(defun speechd-speak-spell ()
  "Let the very next command to spell the text it reads."
  (interactive)
  (unless speechd-speak-spell-mode
    (setq speechd-speak-spell-command t)))



;;; Informatory commands


(defvar speechd-speak-info-map (make-sparse-keymap))

(defvar speechd-speak--info-updates nil)

(cl-defmacro speechd-speak--watch (name get-function &key on-change info info-string key)
  `(progn
     (fset (quote ,(speechd-speak--name 'speechd-speak--get name))
           ,get-function)
     ,(when info
        `(fset (quote ,(speechd-speak--name 'speechd-speak name 'info))
               #'(lambda (info)
                   (interactive)
                   (funcall ,info info))))
     ,(when info-string
        `(fset (quote ,(speechd-speak--name 'speechd-speak name 'info))
               #'(lambda ()
                   (interactive)
                   (speechd-speak--text
                    (format ,info-string
                            (funcall
                             (function ,(speechd-speak--name
                                         'speechd-speak--get name))))))))
     ,(when (and (or info info-string) key)
        `(define-key speechd-speak-info-map ,key
           (quote ,(speechd-speak--name 'speechd-speak name 'info))))
     ,(when on-change
        `(defun ,(speechd-speak--name 'speechd-speak--update name) (old new)
           (speechd-speak--maybe-speak
            (let ((speechd-default-text-priority 'message)
                  (speechd-language (speechd-speak--language t))
                  (speechd-speak-input-method-languages nil))
              (funcall ,on-change old new)))))
     ,(when on-change
        `(add-to-list 'speechd-speak--info-updates (quote ,name)))))

(speechd-speak--watch buffer-name #'buffer-name
  :on-change #'(lambda (_old new)
                 (speechd-speak--text (format "Buffer %s" new))))

(speechd-speak--watch buffer-identification
  #'(lambda ()
      (when (fboundp 'format-mode-line)
        (let ((ident (format-mode-line mode-line-buffer-identification)))
          (set-text-properties 0 (length ident) nil ident)
          ident)))
  :on-change #'(lambda (_old new)
                 (speechd-speak--text
                  (format "New buffer identification: %s" new))))

(speechd-speak--watch buffer-modified #'buffer-modified-p
  :on-change #'(lambda (_old new)
                 (speechd-speak--text
                  (if new "Buffer modified" "No buffer modification"))))

(speechd-speak--watch buffer-read-only #'(lambda () buffer-read-only)
  :on-change #'(lambda (_old new)
                 (speechd-speak--text
                  (if new "Buffer writable" "Buffer read-only"))))

(defun speechd-speak-buffer-info ()
  "Speak current buffer information."
  (interactive)
  (speechd-speak--text
   (format "Buffer %s, %s %s %s; %s"
           (speechd-speak--get-buffer-name)
           (or vc-mode "")
           (if (speechd-speak--get-buffer-read-only) "read only" "")
           (if (speechd-speak--get-buffer-modified) "modified" "")
           (let ((ident (speechd-speak--get-buffer-identification)))
             (if ident
                 (format "buffer identification: %s" ident)
               "")))))
(define-key speechd-speak-info-map "b" 'speechd-speak-buffer-info)

(speechd-speak--watch frame-name #'(lambda () (frame-parameter nil 'name))
  :on-change #'(lambda (old new)
                 (speechd-speak--text
                  (format "Frame changed to: %s; was: %s" new old))))

(speechd-speak--watch frame-identification
  #'(lambda ()
      (when (fboundp 'format-mode-line)
        (let ((ident (format-mode-line mode-line-frame-identification)))
          (set-text-properties 0 (length ident) nil ident)
          ident)))
  :on-change #'(lambda (_old new)
                 (speechd-speak--text
                  (format "Frame identification changed to: %s" new))))

(defun speechd-speak-frame-info ()
  "Speak current frame information."
  (interactive)
  (speechd-speak--text
   (format "Frame: %s; %s"
           (speechd-speak--get-frame-name)
           (let ((ident (speechd-speak--get-frame-identification)))
             (if ident
                 (format "frame identification: %s" ident)
               "")))))
(define-key speechd-speak-info-map "f" 'speechd-speak-frame-info)

(speechd-speak--watch header-line
  #'(lambda ()
      (if (fboundp 'format-mode-line)
          (let ((line (format-mode-line header-line-format)))
            (if (string= line "") "empty" line))
        "unknown"))
  :on-change #'(lambda (_old new)
                 (speechd-speak--text
                  (format "Header line changed to: %s" new)))
  :info-string "Header line: %s"
  :key "h")

(speechd-speak--watch major-mode #'(lambda () mode-name)
  :on-change #'(lambda (old new)
                 (speechd-speak--text
                  (format "Major mode changed to: %s; was: %s" new old))))

(speechd-speak--watch minor-modes
  #'(lambda ()
      (cl-loop for mode-spec in minor-mode-alist
               for mode = (car mode-spec)
               when (and (boundp mode) (symbol-value mode))
               collect (if (memq mode speechd-speak-display-modes)
                           (cadr mode-spec)
                         mode)))
  :on-change #'(lambda (old new)
                 (cl-flet ((set-difference (x y)
                          (cl-loop for i in x
                                   unless (memq i y)
                                   collect i)))
                   (let ((disabled (set-difference old new))
                         (enabled (set-difference new old)))
                     (when enabled
                       (speechd-speak--text
                        (format "Enabled minor modes: %s" enabled)))
                     (when disabled
                       (speechd-speak--text
                        (format "Disabled minor modes: %s" disabled)))))))

(defun speechd-speak-mode-info ()
  "Speak information about current major and minor modes."
  (interactive)
  (speechd-speak--text
   (format "Major mode: %s; minor modes: %s"
           (speechd-speak--get-major-mode)
           (speechd-speak--get-minor-modes))))
(define-key speechd-speak-info-map "m" 'speechd-speak-mode-info)

(speechd-speak--watch buffer-file-coding
  #'(lambda () buffer-file-coding-system)
  :on-change #'(lambda (old new)
                 (speechd-speak--text
                  (format "Buffer file coding changed to: %s; was %s"
                          new old))))

(speechd-speak--watch terminal-coding #'terminal-coding-system
  :on-change #'(lambda (old new)
                 (speechd-speak--text
                  (format "Terminal coding changed to: %s; was: %s" new old))))

(defun speechd-speak-coding-info ()
  "Speak information about current codings."
  (interactive)
  (speechd-speak--text
   (format "Buffer file coding is %s, terminal coding is %s"
           (speechd-speak--get-buffer-file-coding)
           (speechd-speak--get-terminal-coding))))
(define-key speechd-speak-info-map "c" 'speechd-speak-coding-info)

(speechd-speak--watch input-method #'(lambda ()
                                       (or current-input-method "none"))
  :on-change #'(lambda (old new)
                 (speechd-speak--text
                  (format "Input method changed to: %s; was: %s" new old)))
  :info-string "Input method %s"
  :key "i")

(speechd-speak--watch process
  #'(lambda ()
      (let ((process (get-buffer-process (current-buffer))))
        (and process (process-status process))))
  :on-change #'(lambda (old new)
                 (speechd-speak--text
                  (format "Process status changed to: %s; was: %s" new old)))
  :info-string "Process status: %s"
  :key "p")

(defcustom speechd-speak-state-changes
  '(buffer-identification buffer-read-only frame-name frame-identification
    major-mode minor-modes buffer-file-coding terminal-coding input-method
    process)
  "List of identifiers of the Emacs state changes to be automatically reported.
The following symbols are valid state change identifiers: `buffer-name',
`buffer-identification', `buffer-modified', `buffer-read-only', `frame-name',
`frame-identification', `header-line', `major-mode', `minor-modes',
`buffer-file-coding', `terminal-coding', `input-method', `process'."
  :type `(set ,@(mapcar #'(lambda (i) `(const ,i))
                        (reverse speechd-speak--info-updates)))
  :group 'speechd-speak)

(defun speechd-speak--current-info ()
  (sort (mapcar #'(lambda (i)
                    (cons i (funcall
                             (speechd-speak--name 'speechd-speak--get i))))
                speechd-speak--info-updates)
        #'(lambda (x y) (string< (symbol-name (car x))
                                 (symbol-name (car y))))))


;;; Mode definition


(defvar speechd-speak-mode-map nil
  "Keymap used by speechd-speak-mode.")

(define-prefix-command 'speechd-speak-prefix-command 'speechd-speak-mode-map)

(define-key speechd-speak-mode-map "b" 'speechd-speak-read-buffer)
(define-key speechd-speak-mode-map "c" 'speechd-speak-read-char)
(define-key speechd-speak-mode-map "i" 'speechd-speak-last-insertions)
(define-key speechd-speak-mode-map "l" 'speechd-speak-read-line)
(define-key speechd-speak-mode-map "m" 'speechd-speak-last-message)
(define-key speechd-speak-mode-map "o" 'speechd-speak-read-other-window)
(define-key speechd-speak-mode-map "p" 'speechd-out-pause)
(define-key speechd-speak-mode-map "q" 'speechd-speak-toggle-speaking)
(define-key speechd-speak-mode-map "r" 'speechd-speak-read-region)
(define-key speechd-speak-mode-map "s" 'speechd-out-stop)
(define-key speechd-speak-mode-map "w" 'speechd-speak-read-word)
(define-key speechd-speak-mode-map "x" 'speechd-out-cancel)
(define-key speechd-speak-mode-map "z" 'speechd-out-repeat)
(define-key speechd-speak-mode-map "." 'speechd-speak-read-sentence)
(define-key speechd-speak-mode-map "{" 'speechd-speak-read-paragraph)
(define-key speechd-speak-mode-map " " 'speechd-out-resume)
(define-key speechd-speak-mode-map "'" 'speechd-speak-read-sexp)
(define-key speechd-speak-mode-map "[" 'speechd-speak-read-page)
(define-key speechd-speak-mode-map ">" 'speechd-speak-read-rest-of-buffer)
(define-key speechd-speak-mode-map "\C-a" 'speechd-add-connection-settings)
(define-key speechd-speak-mode-map "\C-i" speechd-speak-info-map)
(define-key speechd-speak-mode-map "\C-l" 'speechd-speak-spell)
(define-key speechd-speak-mode-map "\C-m" 'speechd-speak-read-mode-line)
(define-key speechd-speak-mode-map "\C-n" 'speechd-speak-read-next-line)
(define-key speechd-speak-mode-map "\C-p" 'speechd-speak-read-previous-line)
(define-key speechd-speak-mode-map "\C-r" 'speechd-speak-read-rectangle)
(define-key speechd-speak-mode-map "\C-s" 'speechd-speak)
(define-key speechd-speak-mode-map "\C-x" 'speechd-unspeak)
(define-key speechd-speak-mode-map "\C-bk" 'speechd-speak-toggle-braille-keys)
(dotimes (i 9)
  (define-key speechd-speak-mode-map (format "%s" (1+ i))
              'speechd-speak-key-set-predefined-rate))
(define-key speechd-speak-mode-map "d." 'speechd-set-punctuation-mode)
(define-key speechd-speak-mode-map "dc" 'speechd-set-capital-character-mode)
(define-key speechd-speak-mode-map "dl" 'speechd-speak-set-language)
(define-key speechd-speak-mode-map "do" 'speechd-set-output-module)
(define-key speechd-speak-mode-map "dp" 'speechd-set-pitch)
(define-key speechd-speak-mode-map "dr" 'speechd-set-rate)
(define-key speechd-speak-mode-map "dv" 'speechd-set-voice)
(define-key speechd-speak-mode-map "d\C-v" 'speechd-set-synthesizer-voice)
(define-key speechd-speak-mode-map "dV" 'speechd-set-volume)

(defvar speechd-speak--mode-map (make-sparse-keymap))
(defvar speechd-speak--prefix nil)

(defun speechd-speak--build-mode-map ()
  (let ((map speechd-speak--mode-map))
    (when speechd-speak--prefix
      (define-key map speechd-speak--prefix nil))
    (setq speechd-speak--prefix speechd-speak-prefix)
    (define-key map speechd-speak-prefix 'speechd-speak-prefix-command)
    (unless (lookup-key speechd-speak-mode-map speechd-speak-prefix)
      (define-key map (concat speechd-speak-prefix speechd-speak-prefix)
        (lookup-key global-map speechd-speak-prefix)))))

(define-minor-mode speechd-speak-map-mode
  "Toggle use of speechd-speak keymap.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  nil nil speechd-speak--mode-map)

(easy-mmode-define-global-mode
 global-speechd-speak-map-mode speechd-speak-map-mode
 (lambda () (speechd-speak-map-mode 1))
 :group 'speechd-speak)

(defun speechd-speak--shutdown ()
  ;; We don't have to call CANCEL here, since Emacs exit is usually called
  ;; interactively, so it is preceded by the pre-command CANCEL.  Moreover,
  ;; calling CANCEL here means trouble with stopping the final exit messages.
  (speechd-speak-report 'finish :priority 'important))

;;;###autoload
(define-minor-mode speechd-speak-mode
  "Toggle speaking, the speechd-speak mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When speechd-speak mode is enabled, speech output is provided to Speech
Dispatcher on many actions.

The following key bindings are offered by speechd-speak mode, prefixed with
the value of the `speechd-speak-prefix' variable:

\\{speechd-speak-mode-map}
"
  nil " S" speechd-speak--mode-map
  (if speechd-speak-mode
      (progn
        (speechd-speak-map-mode 1)
        (add-hook 'pre-command-hook 'speechd-speak--pre-command-hook)
        (add-hook 'post-command-hook 'speechd-speak--post-command-hook)
        (add-hook 'before-change-functions 'speechd-speak--before-change-hook)
        (add-hook 'after-change-functions 'speechd-speak--after-change-hook)
        (add-hook 'minibuffer-setup-hook 'speechd-speak--minibuffer-setup-hook)
        (add-hook 'minibuffer-exit-hook 'speechd-speak--minibuffer-exit-hook)
        (add-hook 'kill-emacs-hook 'speechd-speak--shutdown))
    ;; We used to call `speechd-cancel' here, but that slows down global mode
    ;; disabling if there are many buffers present.  So `speechd-cancel' is
    ;; called only on global mode disabling now.
    )
  (when (called-interactively-p 'interactive)
    (let ((state (if speechd-speak-mode "on" "off"))
          (speechd-speak-mode t))
      (message "Speaking turned %s" state))))

;;;###autoload
(easy-mmode-define-global-mode
 global-speechd-speak-mode speechd-speak-mode
 (lambda () (speechd-speak-mode 1))
 :group 'speechd-speak)

(speechd-speak--defadvice global-speechd-speak-mode before
  (when global-speechd-speak-mode
    (speechd-out-cancel)))

;; global-speechd-speak-map-mode is not enabled until kill-all-local-variables
;; is called.  So we have to be a bit more aggressive about it sometimes.  The
;; same applies to global-speechd-speak-mode.
(defun speechd-speak--enforce-speak-mode ()
  (cl-flet ((enforce-mode (global-mode local-mode-var)
           (when (and global-mode
                      (not (symbol-value local-mode-var))
                      (not (local-variable-p local-mode-var)))
             (funcall local-mode-var 1))))
    (enforce-mode global-speechd-speak-map-mode 'speechd-speak-map-mode)
    (enforce-mode global-speechd-speak-mode 'speechd-speak-mode)))

(defun speechd-speak-toggle-speaking (arg)
  "Toggle speaking.
When prefix ARG is non-nil, toggle it locally, otherwise toggle it globally."
  (interactive "P")
  (if arg
      (speechd-speak-mode 'toggle)
    (global-speechd-speak-mode 'toggle))
  (when (called-interactively-p 'interactive)
    (let ((state (if speechd-speak-mode "on" "off"))
          (speechd-speak-mode t))
      (message "Speaking turned %s %s" state (if arg "locally" "globally")))))

(defun speechd-unspeak ()
  "Try to avoid invoking any speechd-speak function.
This command is useful as the last help in case speechd-speak gets crazy and
starts blocking your Emacs functions."
  (interactive)
  (setq speechd-speak--started nil)
  (ignore-errors (global-speechd-speak-mode -1))
  (when speechd-speak--message-timer
    (cancel-timer speechd-speak--message-timer)
    (setq speechd-speak--message-timer nil))
  (remove-hook 'pre-command-hook 'speechd-speak--pre-command-hook)
  (remove-hook 'post-command-hook 'speechd-speak--post-command-hook)
  (remove-hook 'before-change-functions 'speechd-speak--before-change-hook)
  (remove-hook 'after-change-functions 'speechd-speak--after-change-hook)
  (remove-hook 'minibuffer-setup-hook 'speechd-speak--minibuffer-setup-hook)
  (remove-hook 'minibuffer-exit-hook 'speechd-speak--minibuffer-exit-hook)
  (remove-hook 'kill-emacs-hook 'speechd-speak--shutdown)
  (speechd-out-shutdown)
  (global-speechd-speak-map-mode -1))

;;;###autoload
(defun speechd-speak ()
  "Start or restart speaking."
  (interactive)
  (when speechd-speak--started
    (speechd-unspeak))
  (setq speechd-speak--started t)
  (speechd-speak--build-mode-map)
  (setq speechd-speak--post-command-speaking
        speechd-speak--post-command-speaking-defaults)
  (global-speechd-speak-mode 1)
  (global-speechd-speak-map-mode 1)
  (speechd-speak--debug 'start)
  (condition-case err (speechd-speak-report 'start)
    (speechd-connection-error
     (message (format "%s: %s" (car err) (cdr err)))
     (sit-for 2)
     (message "Customize `speechd-out-active-drivers' to disable drivers you don't use.")))
  (setq speechd-speak--message-timer
        (run-with-idle-timer 0 t 'speechd-speak--message-timer))
  (run-hooks 'speechd-speak-hook))


;;; Announce


(provide 'speechd-speak)


;;; speechd-speak.el ends here
