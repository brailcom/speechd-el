;;; brltty.el --- Interface to BrlTTY

;; Copyright (C) 2004, 2005 Brailcom, o.p.s.

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

;;; Code:


(require 'cl)


;;; User configuration and commands


(defgroup brltty ()
  "BrlTTY interface.")
  
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

(defcustom brltty-tty (car (read-from-string (or (getenv "CONTROLVT") "0")))
  "Number of the Linux console on which brltty.el runs.
The default value is taken from the environment variable CONTROLVT."
  :type 'integer
  :group 'brltty)

(defcustom brltty-key-functions '((1 . (lambda () (message "Hello!"))))
  "Alist of Braille display key codes and corresponding Emacs functions.
If the given key is pressed, the corresponding function is called with no
arguments.
Please note the functions may be called asynchronously any time.  So they
shouldn't modify current environment in any inappropriate way.  Especially, it
is not recommended to assign or call user commands here."
  :type '(alist :key-type (integer :tag "Key code") :value-type function)
  :group 'brltty)

(defcustom brltty-show-unknown-keys t
  "If non-nil, show Braille keys not assigned in `brltty-key-functions'."
  :type 'boolean
  :group 'brltty)

(defcustom brltty-timeout 3
  "Maximum number of seconds to wait for a BrlTTY answer."
  :type 'integer
  :group 'brltty)


;;; Internal functions and data


(defconst brltty--emacs-accept-ok
  (condition-case _ (progn (accept-process-output nil 0 0 1) t) (error)))

(defconst brltty--emacs-process-ok (fboundp 'process-put))

(defconst brltty--protocol-version 6)

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

(defvar brltty--answers '())

(defun brltty--add-answer (answer)
  (setq brltty--answers (append brltty--answers (list answer)))
  answer)

(defun brltty--next-answer ()
  (pop brltty--answers))

(defstruct brltty--connection
  process
  (display-width nil)
  (output ""))

(unless brltty--emacs-process-ok
  (defvar brltty--process-connections '()))

(defun brltty--open-connection (host port)
  (let ((process (open-network-stream "brltty" nil
                                      (or host brltty-default-host)
                                      (or port brltty-default-port))))
    (when process
      (set-process-coding-system process 'raw-text-unix 'raw-text-unix)
      (if (fboundp 'set-process-query-on-exit-flag)
          (set-process-query-on-exit-flag process nil)
        (process-kill-without-query process))
      (let ((connection (make-brltty--connection :process process)))
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
      (let* ((key (brltty--read-integer data))
             (function (cdr (assoc key brltty-key-functions))))
        (cond
         (function
          (funcall function))
         (brltty-show-unknown-keys
          (message "Braille key pressed: %d" key)))))
     (nil
      ;; unknown packet type -- ignore
      )
     (getdisplaysize
      (brltty--add-answer (list type
                                (brltty--read-integer (substring data 0 4))
                                (brltty--read-integer (substring data 4 8)))))
     (t
      (brltty--add-answer (list type data))))))

(defun brltty--read-answer (connection packet-id)
  (let ((connection (brltty--connection-process connection))
        (answer '(nothing-yet)))
    (while (and answer (not (eq (car answer) packet-id)))
      (brltty--accept-process-output connection)
      (setq answer (brltty--next-answer)))
    (unless answer
      (error "BrlTTY answer not received"))
    (cdr answer)))

(defun brltty--send-packet (connection answer packet-id &rest data-list)
  (let ((length (reduce #'+ data-list :initial-value 0
                        :key #'(lambda (data)
                                 (if (integerp data) 4 (length data)))))
        (process (brltty--connection-process connection)))
    (when process
      (condition-case err
          (flet ((send-integer (n reverse)
                   (process-send-string
                    process
                    (apply #'format "%c%c%c%c"
                           (funcall (if reverse #'reverse #'identity)
                                    (loop for i from 1 to 4
                                          for x = n then (/ x 256)
                                          for rem = (% x 256)
                                          collect rem))))))
            (send-integer length t)
            (send-integer (cdr (assoc packet-id brltty--packet-types)) t)
            (dolist (data data-list)
              (if (integerp data)
                  (send-integer (abs data) (>= data 0))
                (process-send-string process data)))
            (when answer
              (brltty--read-answer connection answer)))
        (error
         (brltty--disable-connection connection err))))))

(defun brltty--authentication-key ()
  (with-temp-buffer
    (insert-file-contents brltty-authentication-file)
    (buffer-substring (point-min) (point-max))))


;;; Public functions and data


(defun brltty-open (&optional host port)
  "Open and return connection to a BrlTTY server running on HOST and PORT.
If HOST or PORT is nil, `brltty-default-host' or `brltty-default-port' is used
respectively."
  (condition-case err
      (let ((connection (brltty--open-connection host port)))
        (brltty--send-packet connection 'ack 'authkey
                             (- brltty--protocol-version)
                             (brltty--authentication-key))
        (brltty--send-packet connection 'ack 'gettty brltty-tty 0)
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
      (delete-process process))))

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
    (let* ((display-width (car (brltty-display-size connection)))
           (text* (if (> (length text) display-width)
                      (substring text 0 display-width)
                    (format (format "%%-%ds" display-width) text))))
      (brltty--send-packet connection nil 'write
                           -38
                           1 display-width
                           (encode-coding-string text* brltty-coding)
                           ;; Cursor position may not be too high, otherwise
                           ;; BrlTTY breaks the connection
                           (if cursor (1+ (min cursor display-width)) 0)))))


;;; Announce

(provide 'brltty)


;;; brltty.el ends here
