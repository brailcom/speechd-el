;;; speechd-speak.el --- simple speechd-el based Emacs client

;; Copyright (C) 2003 Brailcom, o.p.s.
;; Copyright (C) 1995 -- 2002, T. V. Raman 
;; Copyright (C) 1994, 1995 by Digital Equipment Corporation.

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

;; This is a simple experimental Emacs client to speechd.  It is based on the
;; code and ideas taken from the Emacspeak (http://emacspeak.sourceforge.net)
;; package by T. V. Raman.

;;; Code:


(eval-when-compile (require 'cl))
(require 'speechd)


(defconst speechd-speak-version "$Id: speechd-speak.el,v 1.2 2003-06-13 13:10:47 pdm Exp $"
  "Version of the speechd-speak file.")


;;; User options


(defgroup speechd-speak nil
  "Speechd-el user client customization."
  :group 'speechd-el)

(defcustom speechd-speak-startup-hook nil
  "Hook to run after starting speechd-speak."
  :type 'sexp
  :group 'speechd-speak)

(defcustom speechd-speak-deleted-char t
  "If non-nil, speak the deleted char, otherwise speak the adjacent char."
  :type 'boolean
  :group 'speechd-speak)

(defcustom speechd-speak-buffer-name t
  "If non-nil, speak buffer name on a buffer change, otherwise speak a line."
  :type 'boolean
  :group 'speechd-speak)


;;; Control functions


(defvar speechd-speak-quiet t
  "If non-nil, no speech output is produced.")
(make-variable-buffer-local 'speechd-speak-quiet)

(defun speechd-speak-toggle-quiet (&optional prefix)
  "Turn speaking on or off.
If the optional PREFIX argument is non-nil, toggle speaking globally, otherwise
toggle it in the current buffer only."
  (interactive "P")
  (if prefix
      (progn
	(setq-default speechd-speak-quiet (not speechd-speak-quiet))
	(set (make-local-variable 'speechd-speak-quiet)
	     (default-value 'speechd-speak-quiet)))
    (setq speechd-speak-quiet (not speechd-speak-quiet))))

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
(defun speechd-speak-set-predefined-rate ()
  "Set speech rate to one of nine predefined levels.
Level 1 is the slowest, level 9 is the fastest."
  (interactive)
  (let ((level (condition-case nil
		   (read (format "%c" last-input-char))
		 (error nil))))
    (or (numberp level)
        (setq level
              (read-minibuffer
	       "Enter level between 1 and 9 to set speech rate:")))
    (when (or (not (numberp level))
	      (< level 1)
	      (> level 9))
      (error "Invalid level %s" level))
    (speechd-set-rate (cdr (assoc level speechd-speak--predefined-rates)))))
  

;;; Supporting functions and options


(defmacro* speechd-speak--maybe-speak (&body body)
  `(unless speechd-speak-quiet
     ,@body))

(defun speechd-speak--text (text &rest args)
  (speechd-speak--maybe-speak
   ;; TODO: skip invisible text
   ;; TODO: replace repeating patterns
   ;; TODO: handle selective display
   (apply #'speechd-say-text text args)))

(defun speechd-speak--char (&rest args)
  (speechd-speak--maybe-speak
   (apply #'speechd-say-char args)))

(defun speechd-speak--key (&rest args)
  (speechd-speak--maybe-speak
   (apply #'speechd-say-key args)))

(defun speechd-speak--sound (&rest args)
  (speechd-speak--maybe-speak
   (apply #'speechd-say-sound args)))

(defun speechd-speak-read-char (&optional char)
  (interactive)
  (speechd-speak--char (or char (following-char))))

(defun speechd-speak-read-region (&optional beg end)
  (interactive "r")
  (speechd-speak--text (buffer-substring (or beg (mark)) (or end (point)))
		       :priority :text))

(defun speechd-speak-read-line ()
  (interactive)
  (speechd-speak-read-region (line-beginning-position) (line-end-position)))

(defun speechd-speak-read-next-line ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (speechd-speak-read-line)))

(defun speechd-speak-read-previous-line ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (speechd-speak-read-line)))

(defun speechd-speak-read-buffer ()
  (interactive)
  (speechd-speak-read-region (point-min) (point-max)))

(defun speechd-speak-read-rest-of-buffer ()
  (interactive)
  (speechd-speak-read-region (point) (point-max)))

(defun speechd-speak--window-contents ()
  (speechd-speak-read-region (window-start) (window-end)))

(defun speechd-speak--speak-piece (start)
  (let ((point (point)))
    (if (> (count-lines start point) 1)
	(speechd-speak-read-line)
      (speechd-speak-read-region start point))))

(defun speechd-speak--speak-current-column ()
  (speechd-speak--text (format "Column %d" (current-column))))

(defmacro speechd-speak--def-speak-object (type)
  (let* ((name (symbol-name type))
	 (function-name (intern (format "speechd-speak-read-%s" name)))
	 (backward-function (intern (format "backward-%s" name)))
	 (forward-function (intern (format "forward-%s" name))))
    `(defun ,function-name ()
       (interactive)
       (save-excursion
	 (let ((end (progn (,forward-function 1) (point)))
	       (beg (progn (,backward-function 1) (point))))
	   (speechd-speak-read-region beg end))))))

(speechd-speak--def-speak-object word)
(speechd-speak--def-speak-object sentence)
(speechd-speak--def-speak-object paragraph)
(speechd-speak--def-speak-object page)
(speechd-speak--def-speak-object sexp)

(defvar speechd-speak--command-start-info nil)

(defmacro* speechd-speak--command-feedback (commands position &body body)
  (let ((commands* (if (listp commands) commands (list commands)))
	(position* position)
	(body* `(progn (setq speechd-speak--command-start-info nil) ,@body))
	(c (gensym)))
    `(progn
       ,@(mapcar #'(lambda (command)
		     `(defadvice ,command (,position* speechd-speak
					   activate preactivate compile)
			,(if (eq position* 'around)
			     `(if (interactive-p)
				  ,body*
				ad-do-it)
			   `(when (interactive-p)
			      ,body*))))
		 commands*))))

(defmacro* speechd-speak--command-feedback-region (commands &key (move nil))
  `(speechd-speak--command-feedback ,commands around
     (let ((start (save-excursion ,move (point))))
       ad-do-it
       (speechd-speak--speak-piece start))))


;;; Basic speaking


;; These two simply don't work in Emacs 21.3 when invoked via key binding, for
;; an unknown reason. :-(

;; (speechd-speak--command-feedback (self-insert-command) after
;;   (speechd-speak--char (preceding-char)))

;; (speechd-speak--command-feedback (forward-char backward-char) after
;;   (speechd-speak-read-char))

(speechd-speak--command-feedback (next-line previous-line) after
  (speechd-speak-read-line))

(speechd-speak--command-feedback (forward-word backward-word) after
  (speechd-speak-read-word))

(speechd-speak--command-feedback (beginning-of-buffer end-of-buffer) after
  (speechd-speak-read-line))

(speechd-speak--command-feedback (forward-sentence backward-sentence) after
  (speechd-speak-read-sentence))

(speechd-speak--command-feedback (forward-paragraph backward-paragraph) after
  (speechd-speak-read-paragraph))

(speechd-speak--command-feedback (forward-page backward-page) after
  (speechd-speak-read-page))

(speechd-speak--command-feedback (beginning-of-defun end-of-defun) after
  (speechd-speak-read-line))

(speechd-speak--command-feedback (scroll-up scroll-down) after
  (speechd-speak--window-contents))

(speechd-speak--command-feedback-region
 (backward-sexp forward-sexp forward-list backward-list up-list
  backward-up-list down-list))

(speechd-speak--command-feedback (upcase-word downcase-word capitalize-word)
				 after
  (speechd-speak-read-word))

(speechd-speak--command-feedback (delete-backward-char backward-delete-char
				  backward-delete-char-untabify)
				 around
  (when speechd-speak-deleted-char
    (speechd-speak-read-char (preceding-char)))
  ad-do-it
  (unless speechd-speak-deleted-char
    (speechd-speak-read-char (preceding-char))))

(speechd-speak--command-feedback (delete-char) around
  (when speechd-speak-deleted-char
    (speechd-speak-read-char (following-char)))
  ad-do-it
  (unless speechd-speak-deleted-char
    (speechd-speak-read-char (following-char))))

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
  (speechd-speak-read-line))

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
    ad-do-it
    (message "Killed region containing %s lines" nlines)))

(speechd-speak--command-feedback (kill-ring-save) around
  (let ((nlines (count-lines (region-beginning) (region-end))))
    ad-do-it
    (message "Region containing %s lines copied to kill ring" nlines)))


;;; Messages


(defvar speechd-speak--last-message "")

;; Unfortunately, in Emacs 21.3, after change function is not invoked on the
;; `message' insertion and some other built-in call insertions.  See
;; `speechd-speak--post-command-hook'.
(defun speechd-speak--speak-new-message (beg end length)
  (when (= length 0)
    (let ((text (buffer-substring-no-properties beg end)))
      (unless (save-match-data (string-match "times[]]$" text))
	(setq speechd-speak--last-message text))
      (speechd-speak--text text :priority :progress))))
(save-excursion
  (set-buffer "*Messages*")
  (add-hook 'after-change-functions 'speechd-speak--speak-new-message nil t))

(defun speechd-speak-last-message ()
  (interactive)
  (speechd-speak--text speechd-speak--last-message))


;;; Minibuffer


(defun speechd-speak--speak-minibuffer-prompt ()
  (speechd-speak--text (minibuffer-prompt) :priority :message)
  (speechd-speak--text (minibuffer-contents) :priority :message))
(add-hook 'minibuffer-setup-hook 'speechd-speak--speak-minibuffer-prompt)

(defun speechd-speak--speak-minibuffer ()
  (speechd-speak--text (minibuffer-contents)))

(speechd-speak--command-feedback (previous-history-element next-history-element
				  previous-matching-history-element
				  next-matching-history-element
				  minibuffer-complete minibuffer-complete-word)
				 after
  (speechd-speak--speak-minibuffer))

(speechd-speak--command-feedback minibuffer-message after
  (speechd-speak--text (ad-get-arg 0) :priority :notification))


;;; Commands


(defun speechd-speak--pre-command-hook ()
  (when (= (minibuffer-depth) 0)
    (setq speechd-speak--command-start-info (ignore-errors
					      (list (current-buffer)
						    (point)))))
  (add-hook 'pre-command-hook 'speechd-speak--pre-command-hook))

(defun speechd-speak--post-command-hook ()
  (let ((message (current-message)))
    (when message
      (setq speechd-speak--last-message message)
      (speechd-speak--text message :priority :progress)))
  (when (and (= (minibuffer-depth) 0)
	     speechd-speak--command-start-info)
;    (speechd-speak--text (symbol-name this-command) :priority :notice)
    (multiple-value-bind (buffer position) speechd-speak--command-start-info
      (cond
       ((eq this-command 'self-insert-command)
	(speechd-speak-read-char (preceding-char)))
       ((memq this-command '(forward-char backward-char))
	(speechd-speak-read-char))
       ((not (eq buffer (current-buffer)))
	(if speechd-speak-buffer-name
	    (speechd-speak--text (buffer-name) :priority :message)
	  (speechd-speak-read-line)))
       ((not (= position (point)))
	(speechd-speak-read-line)))))
  (add-hook 'post-command-hook 'speechd-speak--post-command-hook))


;;; Comint


(speechd-speak--command-feedback (comint-next-matching-input-from-input
				  comint-previous-matching-input-from-input
				  shell-forward-command shell-backward-command
				  comint-copy-old-input
				  comint-next-input comint-next-matching-input
				  comint-previous-input
				  comint-previous-matching-input)
				 after
  (speechd-speak-read-line))

(speechd-speak--command-feedback comint-show-output after
  (speechd-speak-read-region))
  
(speechd-speak--command-feedback-region comint-dynamic-complete)

(defadvice comint-output-filter (around speechd-speak
				 activate preactivate compile)
  ;; TODO:
  ad-do-it)


;;; Completion


(defun speechd-speak--speak-completion ()
  ;; Taken from `choose-completion'
  (let (beg end completion (buffer completion-reference-buffer)
	(base-size completion-base-size))
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
    (if (null beg)
	(error "No completion here"))
    (setq beg (previous-single-property-change beg 'mouse-face))
    (setq end (or (next-single-property-change end 'mouse-face) (point-max)))
    (setq completion (buffer-substring beg end))
    (speechd-speak--text completion)))

(speechd-speak--command-feedback-region (expand-abbrev complete-symbol)
  :move (backward-word 1))

(speechd-speak--command-feedback-region (lisp-complete-symbol))

(speechd-speak--command-feedback dabbrev-expand after
  (speechd-speak--text dabbrev--last-expansion))

(speechd-speak--command-feedback (next-completion previous-completion) after
  (speechd-speak--speak-completion))

(speechd-speak--command-feedback choose-completion before
  (speechd-speak--speak-completion))


;;; Other functions and packages


(speechd-speak--command-feedback (isearch-search isearch-delete-char) after
  (speechd-speak--text isearch-string)
  (speechd-speak-read-line))

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


;;; The startup function


(defun speechd-speak ()
  "Start speaking."
  (interactive)
  (add-hook 'pre-command-hook 'speechd-speak--pre-command-hook)
  (add-hook 'post-command-hook 'speechd-speak--post-command-hook)
  (setq-default speechd-speak-quiet nil)
  (setq speechd-speak-quiet nil)
  (run-hooks 'speechd-speak-startup-hook)
  (message "Speechd-speak started"))


;;; Keymap


(defvar speechd-speak-prefix "\C-e"
  "Default prefix key used for speechd-speak commands.")

(defvar speechd-speak-keymap nil
  "Keymap used by speechd-speak.")

(define-prefix-command 'speechd-speak-prefix-command 'speechd-speak-keymap)
(global-set-key speechd-speak-prefix 'speechd-speak-prefix-command)

(define-key speechd-speak-keymap "e" 'end-of-line)
(define-key speechd-speak-keymap "\C-e" 'end-of-line)

(define-key speechd-speak-keymap "a" 'speechd-speak-last-message)
(define-key speechd-speak-keymap "b" 'speechd-speak-read-buffer)
(define-key speechd-speak-keymap "c" 'speechd-speak-read-char)
(define-key speechd-speak-keymap "l" 'speechd-speak-read-line)
(define-key speechd-speak-keymap "n" 'speechd-speak-read-rest-of-buffer)
(define-key speechd-speak-keymap "p" 'speechd-pause)
(define-key speechd-speak-keymap "q" 'speechd-speak-toggle-quiet)
(define-key speechd-speak-keymap "r" 'speechd-speak-read-region)
(define-key speechd-speak-keymap "s" 'speechd-stop)
(define-key speechd-speak-keymap "w" 'speechd-speak-read-word)
(define-key speechd-speak-keymap "{" 'speechd-speak-read-paragraph)
(define-key speechd-speak-keymap " " 'speechd-resume)
(define-key speechd-speak-keymap "'" 'speechd-speak-speak-sexp)
(define-key speechd-speak-keymap "[" 'speechd-speak-read-page)
(define-key speechd-speak-keymap "\C-s" 'speechd-reopen)
(define-key speechd-speak-keymap "\M-\C-k" 'kill-emacs)
(define-key speechd-speak-keymap '[down] 'speechd-speak-read-next-line)
(define-key speechd-speak-keymap '[up]  'speechd-speak-read-previous-line)
;(define-key speechd-speak-keymap "W" 'speechd-speak-speak-spell-current-word)
;(define-key speechd-speak-keymap "R" 'speechd-speak-speak-rectangle)
;(define-key speechd-speak-keymap "m" 'speechd-speak-speak-mode-line)
;(define-key speechd-speak-keymap "\C-l" 'speechd-speak-speak-line-number)
;; (define-key speechd-speak-keymap "|"
;;   'speechd-speak-speak-line-set-column-filter)
;(define-key speechd-speak-keymap "^" 'speechd-speak-filtertext)
;(define-key speechd-speak-keymap "k" 'speechd-speak-speak-current-kill)
;(define-key speechd-speak-keymap "\C-@" 'speechd-speak-speak-current-mark)
;; (define-key speechd-speak-keymap "\M-\C-@"
;;   'speechd-speak-speak-spaces-at-point)
;; (define-key speechd-speak-keymap "i" 'speechd-speak-tabulate-region)
;; (define-key speechd-speak-keymap "j" 'speechd-speak-hide-or-expose-block)
;; (define-key speechd-speak-keymap "\C-j"
;;   'speechd-speak-hide-speak-block-sans-prefix)
;(define-key speechd-speak-keymap "\C-m" 'speechd-speak-speak-continuously)
;(define-key speechd-speak-keymap "\C-i" 'speechd-speak-table-display-table-in-region)
;(define-key speechd-speak-keymap "\C-b" 'speechd-speak-daisy-open-book)
;(define-key speechd-speak-keymap "M" 'speechd-speak-speak-minor-mode-line)
;(define-key speechd-speak-keymap "," 'speechd-speak-speak-browse-buffer)
;(define-key speechd-speak-keymap "C" 'speechd-speak-customize)
;(define-key speechd-speak-keymap "\C-c" 'speechd-speak-clipboard-copy)
;; (define-key speechd-speak-keymap "\C-y" 'speechd-speak-clipboard-paste)
;; (define-key speechd-speak-keymap "\C-p" 'speechd-speak-speak-previous-window)
;; (define-key speechd-speak-keymap "\C-n" 'speechd-speak-speak-next-window)
;; (define-key speechd-speak-keymap "=" 'speechd-speak-speak-current-column)
;; (define-key speechd-speak-keymap "%" 'speechd-speak-speak-current-percentage)
;; (define-key speechd-speak-keymap "<" 'speechd-speak-speak-previous-field)
;; (define-key speechd-speak-keymap "." 'speechd-speak-speak-current-field)
;; (define-key speechd-speak-keymap ">"  'speechd-speak-speak-next-field)
;; (define-key speechd-speak-keymap "\C-w"
;;   'speechd-speak-speak-window-information)
(dotimes (i 10)
  (define-key speechd-speak-keymap
    (format "%s" i)
    'speechd-speak-set-predefined-rate))


;;; Announce


(provide 'speechd-speak)


;;; speechd-speak.el ends here

