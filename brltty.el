;;; brltty.el --- Interface to BRLTTY  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2004-2008 Brailcom, o.p.s.

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

;;; Code:


(require 'cl-lib)

(require 'speechd-common)


;;; User configuration and commands


(defgroup brltty ()
  "BRLTTY interface."
  :group 'speechd-el)

(defcustom brltty-default-host "localhost"
  "Default BRLTTY host to connect to."
  :type 'string
  :group 'brltty)

(defcustom brltty-default-port '(4101 4102 4103 4104 35751)
  "Default BRLTTY port to connect to.
If it is a list, the given port numbers are attempted in the order they are
given until Emacs connects to something."
  :type 'integer
  :group 'brltty)

(defcustom brltty-authentication-file "/etc/brlapi.key"
  "File containing the BrlAPI authentication key."
  :type '(file :must-match t)
  :group 'brltty)

(defcustom brltty-coding 'iso-8859-1
  "Coding in which texts should be sent to BRLTTY."
  :type 'coding-system
  :group 'brltty)

(defcustom brltty-tty (car (read-from-string
                            (or (let ((value (ignore-errors
                                               (shell-command-to-string "xprop -root XFree86_VT"))))
                                  (and value
                                       (string-match "= *\\([0-9]+\\)" value)
                                       (match-string 1 value)))
                                (getenv "CONTROLVT")
                                "0")))
  "Number of the Linux console on which brltty.el runs.
The default value is taken from the XFree86_VT root window property or, if not
available, from the  environment variable CONTROLVT."
  :type 'integer
  :group 'brltty)

(defcustom brltty-timeout 3
  "Maximum number of seconds to wait for a BRLTTY answer."
  :type 'integer
  :group 'brltty)


;;; Internal functions and data


(defconst brltty--emacs-accept-ok
  (condition-case _ (progn (accept-process-output nil 0 0 1) t) (error)))

(defconst brltty--emacs-process-ok (fboundp 'process-put))

(defconst brltty--supported-protocol-versions '(8 7))

(defconst brltty--protocol-version-error 13)

(defconst brltty--errors
  '((1 . "Not enough memory")
    (2 . "A connection is already running in this tty")
    (3 . "A connection is already using RAW or suspend mode")
    (4 . "Not implemented in protocol")
    (5 . "Forbidden in current mode")
    (6 . "Out of range or have no sense")
    (7 . "Invalid size")
    (8 . "Connection refused")
    (9 . "Operation not supported")
    (10 . "Getaddrinfo error")
    (11 . "Libc error")
    (12 . "Couldn't find out the tty number")
    (13 . "Bad protocol version")
    (14 . "Unexpected end of file")
    (15 . "Key file empty")
    (16 . "Packet returned by driver too large")
    (17 . "Authentication failed")))

(defconst brltty--packet-types
  '(;; commands
    (version . ?v)
    (auth . ?a)
    (authkey . ?K) ; protocol version < 8
    (gettty . ?t)
    (leavetty . ?L)
    (getdisplaysize . ?s)
    (write . ?w)
    (ignorekeyranges . ?m)
    (acceptkeyranges . ?u)
    ;; answers
    (ack . ?A)
    (err . ?e)
    (key . ?k)))

(defconst brltty--authentication-codes
  '((?N . none)
    (?K . key)
    (?C . credentials)))

(cl-defstruct brltty--connection
  process
  (protocol-version nil)
  (display-width nil)
  (output "")
  (key-handler nil)
  (answers '())
  (terminal-spec nil))

(defun brltty--add-answer (connection answer)
  (setf (brltty--connection-answers connection)
        (append (brltty--connection-answers connection) (list answer)))
  answer)

(defun brltty--next-answer (connection)
  (pop (brltty--connection-answers connection)))

(unless brltty--emacs-process-ok
  (defvar brltty--process-connections '()))

(let ((last-selected-frame 'uninitialized)
      (last-terminal-spec 'uninitialized))
  (defun brltty--terminal-spec ()
    (if (eq (selected-frame) last-selected-frame)
        last-terminal-spec
      (prog1 (setq last-terminal-spec (brltty--terminal-spec*))
        (setq last-selected-frame (selected-frame))))))

(defun brltty--terminal-spec* ()
  (let ((terminal-spec '()))
    (cond
     ((or (getenv "WINDOWPATH")
          (getenv "WINDOWSPATH"))
      (save-match-data
        (dolist (number (split-string (or (getenv "WINDOWPATH")
                                          (getenv "WINDOWSPATH"))
                                      ":"))
          (push (string-to-number number) terminal-spec))))
     ((eq window-system 'x)
      (push brltty-tty terminal-spec)))
    (cond
     ((eq window-system 'x)
      (push (string-to-number (frame-parameter (selected-frame) 'outer-window-id)) terminal-spec))
     ((getenv "WINDOWID")
      (push (string-to-number (getenv "WINDOWID")) terminal-spec))
     ((file-readable-p "/proc/self/stat")
      (with-temp-buffer
        (let ((standard-input (current-buffer)))
          (push (or (ignore-errors
                      (with-speechd-coding-protection
                        (insert-file-contents "/proc/self/stat")
                        (goto-char (point-min))
                        (dotimes (_ 6) (read))
                        (let ((tty (read)))
                          (if (and tty (= (/ tty 256) 4)) ; major
                              (mod tty 256)               ; minor
                            0))))
                    0)
                terminal-spec))))
     (t
      (push 0 terminal-spec)))
    (nreverse terminal-spec)))

(defun brltty--open-connection (host port key-handler)
  (let ((process nil)
        (ports (or port brltty-default-port))
        (host* (or host brltty-default-host)))
    (unless (consp ports)
      (setq ports (list port)))
    (while (and (not process)
                ports)
      (condition-case err
          (setq process (open-network-stream "brltty" nil host* (car ports)))
        (error
         (if (cdr ports)
             (setq ports (cdr ports))
           (signal (car err) (cdr err))))))
    (when process
      (set-process-coding-system process 'binary 'binary)
      (if (fboundp 'set-process-query-on-exit-flag)
          (set-process-query-on-exit-flag process nil)
        (set-process-query-on-exit-flag process nil))
      (let ((connection (make-brltty--connection :process process
                                                 :key-handler key-handler)))
        (if brltty--emacs-process-ok
            (process-put process 'brltty-connection connection)
          (push (cons process connection) brltty--process-connections))
        (set-process-filter process #'brltty--process-filter)
        connection))))

(defun brltty--process-connection (process)
  (if brltty--emacs-process-ok
      (process-get process 'brltty-connection)
    (cdr (assq process brltty--process-connections))))

(defun brltty--disable-connection (connection error)
  (let ((process (brltty--connection-process connection)))
    (when process
      (delete-process process)
      (setf (brltty--connection-process connection) nil)))
  (error "Error in communication with BRLTTY: %s" error))

(defun brltty--process-filter* (process output)
  (let ((connection (brltty--process-connection process)))
    (setf (brltty--connection-output connection)
          (concat (brltty--connection-output connection) output))))

(defun brltty--process-filter (process output)
  (set-process-filter process #'brltty--process-filter*)
  (unwind-protect
      (let ((connection (brltty--process-connection process)))
        (brltty--process-filter* process output)
        (while (not (string= (brltty--connection-output connection) ""))
          (brltty--read-input connection)))
    (set-process-filter process #'brltty--process-filter)))

(defun brltty--accept-process-output (process)
  (if brltty--emacs-accept-ok
      (accept-process-output process brltty-timeout nil 1)
    (accept-process-output process brltty-timeout)))

(defun brltty--read-integer (string)
  (+ (* 256 256 256 (aref string 0)) (* 256 256 (aref string 1))
     (* 256 (aref string 2)) (aref string 3)))

(defun brltty--read-integer64 (string)
  (list
   (+ (* 256 (aref string 0)) (aref string 1))
   (+ (* 256 256 (aref string 2)) (* 256 (aref string 3)) (aref string 4))
   (+ (* 256 256 (aref string 5)) (* 256 (aref string 6)) (aref string 7))))

(defun brltty--read-packet (connection)
  (condition-case err
      (let ((process (brltty--connection-process connection)))
        (cl-flet ((read-enough-output (size)
                 (while (< (length (brltty--connection-output connection)) size)
                   (brltty--accept-process-output process)))
               (read-integer ()
                 (let ((output (brltty--connection-output connection)))
                   (prog1 (brltty--read-integer output)
                     (setf (brltty--connection-output connection)
                           (substring output 4))))))
          (read-enough-output 8)
          (let* ((size (read-integer))
                 (type (car (rassoc (read-integer) brltty--packet-types)))
                 (data (progn
                         (read-enough-output size)
                         (let ((output (brltty--connection-output connection)))
                           (setf (brltty--connection-output connection)
                                 (substring output size))
                           (substring output 0 size)))))
            (cons type data))))
    (error
     (brltty--disable-connection connection err))))

(defun brltty--read-input (connection)
  (cl-destructuring-bind (type . data) (brltty--read-packet connection)
    (cl-case type
     (err
      (let ((err-number (brltty--read-integer data)))
        (if (= err-number brltty--protocol-version-error)
            (brltty--add-answer connection (list type err-number))
          (error (format "BRLTTY error %d: %s"
                         err-number (cdr (assoc err-number brltty--errors)))))))
     (key
      (let ((handler (brltty--connection-key-handler connection)))
        (when handler
          (funcall handler
                   (funcall (if (> (length data) 4)
                                #'brltty--read-integer64
                              #'brltty--read-integer)
                            data)))))
     (authkey
      (let ((version (brltty--read-integer (substring data 0 4)))
            (auth-methods '())
            (len (length data))
            (n 4))
        (while (< n len)
          (push (cdr (assoc (brltty--read-integer (substring data n (+ n 4)))
                            brltty--authentication-codes))
                auth-methods)
          (setq n (+ n 4)))
        (brltty--add-answer connection (list type version auth-methods))))
     (auth
      (let ((auth-methods '())
            (len (length data))
            (n 0))
        (while (< n len)
          (push (cdr (assoc (brltty--read-integer (substring data n (+ n 4)))
                            brltty--authentication-codes))
                auth-methods)
          (setq n (+ n 4)))
        (brltty--add-answer connection (cons type auth-methods))))
     (version
      (let ((version (brltty--read-integer (substring data 0 4))))
        (brltty--add-answer connection (list type version))))
     (nil
      ;; unknown packet type -- ignore
      )
     (getdisplaysize
      (brltty--add-answer connection
                          (list type
                                (brltty--read-integer (substring data 0 4))
                                (brltty--read-integer (substring data 4 8)))))
     (t
      (brltty--add-answer connection (list type data))))))

(defun brltty--read-answer (connection packet-id &optional none-ok)
  (let ((process (brltty--connection-process connection))
        (answer '(nothing-yet)))
    (while (and answer (not (eq (car answer) packet-id)))
      ;; The answer may be already present in answers
      (setq answer (brltty--next-answer connection))
      (unless answer
        (brltty--accept-process-output process)
        (setq answer (brltty--next-answer connection))))
    (when (and (not answer) (not none-ok))
      (error "BRLTTY answer not received"))
    (cdr answer)))

(defun brltty--send-packet (connection answer packet-id &rest data-list)
  (let ((length (cl-reduce #'+ data-list :initial-value 0
                           :key #'(lambda (data)
                                    (cond
				     ((integerp data) 4)
                                     ((consp data)
                                      (cl-ecase (car data)
                                        (integer64 8)))
				     (t (length data))))))
        (process (brltty--connection-process connection)))
    (when process
      (with-speechd-coding-protection
        (condition-case err
            (cl-flet ((send-integer (n)
                     (process-send-string
                      process
                      (encode-coding-string
                       (apply #'format "%c%c%c%c"
                              (funcall #'reverse
                                       (cl-loop for i from 1 to 4
                                                for x = n then (/ x 256)
                                                for rem = (% x 256)
                                                collect rem)))
                       brltty-coding))))
              (send-integer length)
              (send-integer (cdr (assoc packet-id brltty--packet-types)))
              (dolist (data data-list)
                (cond
                 ((integerp data)
                  (send-integer data))
                 ((vectorp data)
                  (dotimes (i (length data))
                    (process-send-string
                     process
                     (encode-coding-string (format "%c" (aref data i)) brltty-coding))))
                 ((consp data)
                  (cl-destructuring-bind (n1 n2 n3) (cdr data)
                    (cl-ecase (car data)
                      (integer64
                       (process-send-string
                        process
                        (encode-coding-string (format "%c%c%c%c%c%c%c%c"
                                                     (/ n1 256) (% n1 256)
                                                     (/ n2 (* 256 256)) (% (/ n2 256) 256) (% n2 256)
                                                     (/ n3 (* 256 256)) (% (/ n3 256) 256) (% n3 256))
                                              brltty-coding))))))
                 (t
                  (process-send-string process data))))
              (when answer
                (brltty--read-answer connection answer)))
          (error
           (brltty--disable-connection connection err)))))))

(defun brltty--authentication-key ()
  (with-temp-buffer
    (insert-file-contents brltty-authentication-file)
    (buffer-substring (point-min) (point-max))))

(defun brltty--update-terminal-spec (connection)
  (let ((terminal-spec (brltty--terminal-spec))
        (orig-terminal-spec (brltty--connection-terminal-spec connection)))
    (unless (cl-equalp terminal-spec orig-terminal-spec)
      (when orig-terminal-spec
        (brltty--send-packet connection 'ack 'leavetty))
      (apply 'brltty--send-packet
             (append (list connection 'ack 'gettty (length terminal-spec))
                     terminal-spec
                     (list [0])))
      (setf (brltty--connection-terminal-spec connection) terminal-spec))))


;;; Public functions and data

(put 'brltty-connection-error 'error-conditions
     '(error speechd-connection-error brltty-connection-error))
(put 'brltty-connection-error 'error-message
     "Error on opening BRLTTY connection")

(defun brltty-open (&optional host port key-handler)
  "Open and return connection to a BRLTTY server running on HOST and PORT.
If HOST or PORT is nil, `brltty-default-host' or `brltty-default-port' is used
respectively."
  (condition-case err
      ;; In protocol >= 8 server initiates communication, let's look if
      ;; there is any
      (let* ((connection (brltty--open-connection host port key-handler))
             (version (or (cl-first (brltty--read-answer connection 'version t))
                          7)))
        (if (> version 7)
            (progn
              (unless (member version brltty--supported-protocol-versions)
                (setq version (apply 'max brltty--supported-protocol-versions)))
              (let ((auth-methods (brltty--send-packet connection 'auth 'version version)))
                (cond
                 ((memq 'none auth-methods)
                  nil)
                 ((memq 'key auth-methods)
                  (brltty--send-packet
                   connection 'ack 'auth
                   (car (rassoc 'key brltty--authentication-codes))
                   (brltty--authentication-key)))
                 (t
                  (signal 'brltty-connection-error
                          "No supported BrlAPI authentication method")))))
          ;; No server initial message, assume protocol version 7 (older
          ;; versions are not supported)
          (let ((answer (brltty--send-packet
                         connection 'ack 'authkey version
                         (brltty--authentication-key))))
            (when (equal answer brltty--protocol-version-error)
              (signal 'brltty-error answer))))
        (setf (brltty--connection-protocol-version connection) version)
        (brltty--update-terminal-spec connection)
        connection)
    (error
     (signal 'brltty-connection-error err))))

(defun brltty-close (connection)
  "Close BRLTTY CONNECTION."
  (when connection
    (when (brltty--connection-terminal-spec connection)
      (brltty--send-packet connection 'ack 'leavetty))
    (let ((process (brltty--connection-process connection)))
      (unless brltty--emacs-process-ok
        (setq brltty--process-connections
              (remove (assq process brltty--process-connections)
                      brltty--process-connections)))
      (when process
        (delete-process process)))))

(defun brltty-display-size (connection)
  "Return the size of the display as the list (WIDTH HEIGHT)."
  (when connection
    (or (brltty--connection-display-width connection)
        (let ((size (brltty--send-packet connection 'getdisplaysize
                                         'getdisplaysize)))
          ;; BrlAPI may report zero size when the display is off.
          ;; We shouldn't return nor cache such a value.
          (when (> (car size) 0)
            (setf (brltty--connection-display-width connection) size))))))

(defun brltty-write (connection text &optional cursor)
  "Display TEXT in BRLTTY accessed through CONNECTION.
TEXT is encoded in the coding given by `brltty-coding' before it is sent.
CURSOR, if non-nil, is a position of the cursor on the display, starting
from 0."
  (when connection
    (let ((display-width (car (brltty-display-size connection))))
      (when display-width
        (let* ((text* (if (> (length text) display-width)
                          (substring text 0 display-width)
                        ;; We must be careful with FORMAT because of formatting
                        ;; of TAB characters
                        (concat text
                                (format
                                 (format "%%-%ds" (- display-width
                                                     (length text)))
                                 ""))))
               (protocol-version (brltty--connection-protocol-version connection))
               (text-coding brltty-coding)
               (encoded-text (with-speechd-coding-protection
                              (encode-coding-string text* text-coding)))
               (arguments (list connection nil 'write
                                (if (< protocol-version 8) 38 102)
                                1 display-width
                                (length encoded-text) encoded-text
                                ;; Cursor position may not be too high,
                                ;; otherwise BRLTTY breaks the connection
                                (if cursor
                                    (1+ (min cursor display-width))
                                  0))))
          (when (>= protocol-version 8)
            (let ((charset (symbol-name text-coding)))
              (setf arguments (append arguments (list (vector (length charset)) (upcase charset))))))
          (apply #'brltty--send-packet arguments))))))

(defun brltty-ignore-keys (connection)
  "Let BrlTTY handle all keys itself."
  (when connection
    (brltty--send-packet connection nil 'ignorekeyranges '(integer64 0 0 0) '(integer64 #xFFFF #xFFFFFF #xFFFFFF))))

(defun brltty-accept-keys (connection &optional keys)
  "Let BrlTTY send all keys to us.
If optional argument KEYS is non-nil, allow to send us only the given keys.
Then KEYS must be a list of key codes represented by integer triplets."
  (when connection
    (let ((key-ranges (if keys
                          (mapcar #'(lambda (key) (cons (cons 'integer64 key) (cons 'integer64 key))) keys)
                        (list (cons '(integer64 0 0 0) '(integer64 #xFFFF #xFFFFFF #xFFFFFF))))))
      (dolist (range key-ranges)
        (brltty--send-packet connection nil 'acceptkeyranges (car range) (cdr range))))))


;;; Announce

(provide 'brltty)


;;; brltty.el ends here
