;;; mmanager.el --- Message manager  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2004, 2005 Brailcom, o.p.s.

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


;;; User customization

(defgroup message-manager ()
  "Message queue management."
  :group 'speechd-el)

(defcustom mmanager-history-size 12
  "Maximum number of message stored in the message history."
  :type 'integer
  :group 'message-manager)

;;; Data structures

(cl-defstruct mmanager--manager
  (queue '())
  (current-message nil)
  (message-blocks '())
  (paused nil)
  (last-group nil)
  display-f
  stop-f
  pause-f
  resume-f
  busy-f
  properties
  (history '())
  (history-cursor nil))

(cl-defstruct mmanager--message
  messages
  priority
  client
  group)

;;; Calls to the driver functions

(defun mmanager--busy (manager)
  (funcall (mmanager--manager-busy-f manager) manager))

(defun mmanager--stop (manager)
  (when (mmanager--busy manager)
    (funcall (mmanager--manager-stop-f manager) manager)))

(defun mmanager--pause (manager)
  (when (and (mmanager--busy manager)
             (not (mmanager--manager-paused manager)))
    (funcall (mmanager--manager-pause-f manager) manager))
  (setf (mmanager--manager-paused manager) t))

(defun mmanager--resume (manager)
  (when (mmanager--manager-paused manager)
    (funcall (mmanager--manager-resume-f manager) manager)
    (setf (mmanager--manager-paused manager) nil)))

(defun mmanager--display (manager message)
  (when message
    (mmanager--add-history manager message)
    (funcall (mmanager--manager-display-f manager) manager message)))

;;; Utility functions

(defun mmanager--make-message (message priority client group)
  (make-mmanager--message :messages (and message (list message))
                          :priority priority :client client :group group))

(defun mmanager--current-priority (manager)
  (let ((current (mmanager--manager-current-message manager)))
    (and current (mmanager--message-priority current))))

(defun mmanager--client-block (manager client)
  (cdr (assoc client (mmanager--manager-message-blocks manager))))

;;; Queue management

(defun mmanager--prune-queue (manager condition)
  (setf (mmanager--manager-queue manager)
        (cl-remove-if condition (mmanager--manager-queue manager))))

(defun mmanager--update-queue (manager message* priority)
  (let* ((queue (mmanager--manager-queue manager))
         (last (car (last queue))))
    (when (and last
               (eq (mmanager--message-priority last) 'progress)
               (not (eq priority 'important)))
      (setf (mmanager--message-priority last) 'message))
    (setf (mmanager--manager-queue manager)
          (if (eq priority 'important)
              (cl-labels ((add (list)
                         (if (and list
                                  (eq (mmanager--message-priority (car list))
                                      'important))
                             (cons (car list) (add (cdr list)))
                           (cons message* (cdr list)))))
                (add queue))
            (append queue (list message*))))))

(defun mmanager--enqueue* (manager message* priority)
  (when (mmanager--message-messages message*)
    (cl-ecase priority
      (important
       (mmanager--pause manager))
      ((message text)
       (mmanager--prune-queue
        manager
        #'(lambda (m) (memq (mmanager--message-priority m)
                            '(text notification))))
       (when (not (memq (mmanager--current-priority manager)
                        '(important message)))
         (mmanager--stop manager)))
      (notification
       (when (and (or (not (mmanager--busy manager))
                      (eq (mmanager--current-priority manager)
                          'notification))
                  (not (mmanager--manager-queue manager)))
         (mmanager--stop manager)))
      (progress
       (mmanager--prune-queue
        manager
        #'(lambda (m) (eq (mmanager--message-priority m) 'progress)))))
    (mmanager--update-queue manager message* priority)
    (mmanager-next manager)))

(defun mmanager--add-history (manager message)
  (let ((history (mmanager--manager-history manager)))
    (when (= (length history) mmanager-history-size)
      (setq history (cdr history)))
    (setf (mmanager--manager-history manager) (append history (list message)))
    (setf (mmanager--manager-history-cursor manager) message)))

;;; Public functions

(defun mmanager-create (display-f stop-f pause-f resume-f busy-f)
  (make-mmanager--manager :display-f display-f :stop-f stop-f
                          :pause-f pause-f :resume-f resume-f :busy-f busy-f))

(defun mmanager-next (manager)
  (let* ((current (mmanager--manager-current-message manager))
         (messages (and current (mmanager--message-messages current))))
    (if messages
        (let ((m (car messages))
              (g (mmanager--message-group current)))
          (when (or (not (mmanager--busy manager))
                    (and g (eq g (mmanager--manager-last-group manager))))
            (mmanager--display manager m)
            (setf (mmanager--manager-last-group manager) g)
            (setf (mmanager--message-messages current) (cdr messages))
            (mmanager-next manager)))
      (let* ((queue (mmanager--manager-queue manager))
             (message (car queue)))
        (if (and message
                 (mmanager--manager-paused manager)
                 (not (eq (mmanager--message-priority message) 'important)))
            (mmanager--resume manager)
          (setf (mmanager--manager-queue manager) (cdr queue))
          (setf (mmanager--manager-current-message manager) message)
          (when message
            (mmanager-next manager)))))))

(defun mmanager-cancel (manager client)
  (mmanager--stop manager)
  (mmanager--prune-queue manager
                         (if client
                             #'(lambda (m)
                                 (string= (mmanager--message-client m) client))
                           #'identity))
  (setf (mmanager--manager-current-message manager) nil)
  (dolist (block% (mmanager--manager-message-blocks manager))
    (setcdr block% 'canceled)))

(defun mmanager-enqueue (manager client message priority &optional group)
  (let ((message-block (and message (mmanager--client-block manager client))))
    (if message-block
        (unless (eq message-block 'canceled)
          (setf (mmanager--message-messages message-block)
                (append (mmanager--message-messages message-block)
                        (list message)))
          (when group
            (setf (mmanager--message-group message-block) group)))
      (mmanager--enqueue*
       manager (mmanager--make-message message priority client group)
       priority))))

(defun mmanager-start-block (manager client priority)
  (unless (mmanager--client-block manager client)
    (push (cons client (mmanager--make-message nil priority client nil))
          (mmanager--manager-message-blocks manager))))

(defun mmanager-finish-block (manager client)
  (let ((message (mmanager--client-block manager client)))
    (when message
      (setf (mmanager--manager-message-blocks manager)
            (cl-remove client (mmanager--manager-message-blocks manager)
                       :key #'car :test #'string=))
      (unless (eq message 'canceled)
        (mmanager--enqueue* manager message
                            (mmanager--message-priority message))))))

(defun mmanager-get (manager property)
  (plist-get (mmanager--manager-properties manager) property))

(defun mmanager-put (manager property value)
  (setf (mmanager--manager-properties manager)
        (plist-put (mmanager--manager-properties manager) property value)))

(defun mmanager-history (manager which)
  (let ((history (mmanager--manager-history manager))
        (cursor (mmanager--manager-history-cursor manager)))
    (cl-ecase which
      (current
       (cl-find cursor history :test #'eq))
      (next
       (let ((next (cl-rest (cl-member cursor history :test #'eq))))
         (when next
           (setf (mmanager--manager-history-cursor manager) (cl-first next)))))
      (previous
       (let ((pos (cl-position cursor history :test #'eq)))
         (when (and pos (> pos 0))
           (setf (mmanager--manager-history-cursor manager)
                 (nth (1- pos) history)))))
      (first
       (setf (mmanager--manager-history-cursor manager) (cl-first history)))
      (last
       (setf (mmanager--manager-history-cursor manager)
             (car (last history)))))))

;;; Announce

(provide 'mmanager)

;;; mmanager.el ends here
