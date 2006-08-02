;;; brltty.el --- Interface to BrlTTY

;; Copyright (C) 2004, 2005, 2006 Brailcom, o.p.s.

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
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;;; Commentary:

;;; Code:


(require 'cl)

(require 'speechd-common)


;;; User configuration and commands


(defgroup brltty ()
  "BrlTTY interface."
  :group 'speechd-el)
  
(defcustom brltty-default-host "localhost"
  "Default BrlTTY host to connect to."
  :type 'string
  :group 'brltty)

(defcustom brltty-default-port 35751
  "Default BrlTTY port to connect to."
  :type 'integer
  :group 'brltty)

(defcustom brltty-authentication-file "/etc/brlapi.key"
  "File containing the BrlAPI authentication key."
  :type '(file :must-match t)
  :group 'brltty)

(defcustom brltty-coding 'iso-8859-1
  "Coding in which texts should be sent to BrlTTY."
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
  "Maximum number of seconds to wait for a BrlTTY answer."
  :type 'integer
  :group 'brltty)


;;; Internal functions and data


(defconst brltty--emacs-accept-ok
  (condition-case _ (progn (accept-process-output nil 0 0 1) t) (error)))

(defconst brltty--emacs-process-ok (fboundp 'process-put))

(defconst brltty--protocol-version 7)

(defconst brltty--packet-types
  '(;; commands
    (authkey . ?K)
    (gettty . ?t)
    (leavetty . ?L)
    (getdisplaysize . ?s)
    (write . ?w)
    ;; answers
    (ack . ?A)
    (err . ?e)
    (key . ?k)))

(defun brltty--add-answer (connection answer)
  (setf (brltty--connection-answers connection)
        (append (brltty--connection-answers connection) (list answer)))
  answer)

(defun brltty--next-answer (connection)
  (pop (brltty--connection-answers connection)))

(defstruct brltty--connection
  process
  (display-width nil)
  (output "")
  (key-handler nil)
  (answers '())
  (terminal-spec nil))

(unless brltty--emacs-process-ok
  (defvar brltty--process-connections '()))

(defun brltty--terminal-spec ()
  (let ((terminal-spec '()))
    (cond
     ((getenv "WINDOWSPATH")
      (save-match-data
        (dolist (number (split-string (getenv "WINDOWSPATH") ":"))
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
  (let ((process (open-network-stream "brltty" nil
                                      (or host brltty-default-host)
                                      (or port brltty-default-port))))
    (when process
      (set-process-coding-system process 'binary 'binary)
      (if (fboundp 'set-process-query-on-exit-flag)
          (set-process-query-on-exit-flag process nil)
        (process-kill-without-query process))
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
  (error "Error in communication with BrlTTY: %s" error))

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

(defun brltty--read-packet (connection)
  (condition-case err
      (let ((process (brltty--connection-process connection)))
        (flet ((read-enough-output (size)
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
  (destructuring-bind (type . data) (brltty--read-packet connection)
    (case type
     (err
      (error (format "BrlTTY error %d: %s" (brltty--read-integer data) data)))
     (key
      (let ((handler (brltty--connection-key-handler connection)))
        (when handler
          (funcall handler (brltty--read-integer data)))))
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

(defun brltty--read-answer (connection packet-id)
  (let ((process (brltty--connection-process connection))
        (answer '(nothing-yet)))
    (while (and answer (not (eq (car answer) packet-id)))
      ;; The answer may be already present in answers
      (setq answer (brltty--next-answer connection))
      (unless answer
        (brltty--accept-process-output process)
        (setq answer (brltty--next-answer connection))))
    (unless answer
      (error "BrlTTY answer not received"))
    (cdr answer)))

(defun brltty--send-packet (connection answer packet-id &rest data-list)
  (let ((length (reduce #'+ data-list :initial-value 0
                        :key #'(lambda (data)
                                 (cond 
				  ((integerp data) 4)
				  (t (length data))))))
        (process (brltty--connection-process connection)))
    (when process
      (with-speechd-coding-protection
        (condition-case err
            (flet ((send-integer (n)
                     (process-send-string
                      process
                      (apply #'format "%c%c%c%c"
                             (funcall #'reverse
                                      (loop for i from 1 to 4
                                            for x = n then (/ x 256)
                                            for rem = (% x 256)
                                            collect rem))))))
              (send-integer length)
              (send-integer (cdr (assoc packet-id brltty--packet-types)))
              (dolist (data data-list)
                (cond
                 ((integerp data)
                  (send-integer data))
                 ((vectorp data)
                  (dotimes (i (length data))
                    (process-send-string process (format "%c" (aref data i)))))
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
    (unless (equalp terminal-spec orig-terminal-spec)
      (when orig-terminal-spec
        (brltty--send-packet connection 'ack 'leavetty))
      (apply 'brltty--send-packet
             (append (list connection 'ack 'gettty (length terminal-spec))
                     terminal-spec
                     (list [0])))
      (setf (brltty--connection-terminal-spec connection) terminal-spec))))


;;; Public functions and data


(defun brltty-open (&optional host port key-handler)
  "Open and return connection to a BrlTTY server running on HOST and PORT.
If HOST or PORT is nil, `brltty-default-host' or `brltty-default-port' is used
respectively."
  (condition-case err
      (let ((connection (brltty--open-connection host port key-handler)))
        (brltty--send-packet connection 'ack 'authkey
                             brltty--protocol-version
                             (brltty--authentication-key))
        connection)
    (error
     (message "Error on opening BrlTTY connection: %s" err)
     nil)))

(defun brltty-close (connection)
  "Close BrlTTY CONNECTION."
  (when connection
    (brltty--send-packet connection 'ack 'leavetty)
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
        (setf (brltty--connection-display-width connection)
              (brltty--send-packet connection 'getdisplaysize
                                   'getdisplaysize)))))

(defun brltty-write (connection text &optional cursor)
  "Display TEXT in BrlTTY accessed through CONNECTION.
TEXT is encoded in the coding given by `brltty-coding' before it is sent.
CURSOR, if non-nil, is a position of the cursor on the display, starting
from 0."
  (when connection
    (brltty--update-terminal-spec connection)
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
               (encoded-text (with-speechd-coding-protection
                              (encode-coding-string text* brltty-coding))))
          (brltty--send-packet connection nil 'write
                               38
                               1 display-width
                               (length encoded-text) encoded-text
                               ;; Cursor position may not be too high,
                               ;; otherwise BrlTTY breaks the connection
                               (if cursor
                                   (1+ (min cursor display-width))
                                 0)))))))


;;; Announce

(provide 'brltty)


;;; brltty.el ends here
