;;; braille.el --- Simple Emacs braille display emulator  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2021 Milan Zamazal <pdm@zamazal.org>
;; Copyright (C) 2004 Brailcom, o.p.s.

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


(defvar braille-buffer-name "*braille-monitor*")


(defvar braille-table
  '((? . ())
    (?! . (2 3 4 6))
    (?\" . (5))
    (?# . (3 4 5 6))
    (?$ . (1 2 4 6))
    (?% . (1 4 6))
    (?& . (1 2 3 4 6))
    (?' . (3))
    (?\( . (1 2 3 5 6))
    (?\) . (2 3 4 5 6))
    (?* . (1 6))
    (?+ . (3 4 6))
    (?, . (6))
    (?- . (3 6))
    (?. . (4 6))
    (?/ . (3 4))
    (?0 . (3 5 6))
    (?1 . (2))
    (?2 . (2 3))
    (?3 . (2 5))
    (?4 . (2 5 6))
    (?5 . (2 6))
    (?6 . (2 3 5))
    (?7 . (2 3 5 6))
    (?8 . (2 3 6))
    (?9 . (3 5))
    (?0 . (1 5 6))
    (?\; . (5 6))
    (?< . (1 2 6))
    (?= . (1 2 3 4 5 6))
    (?> . (3 4 5))
    (?? . (1 4 5 6))
    (?@ . (4 7))
    (?A . (1 7))
    (?B . (1 2 7))
    (?C . (1 4 7))
    (?D . (1 4 5 7))
    (?E . (1 5 7))
    (?F . (1 2 4 7))
    (?G . (1 2 4 5 7))
    (?H . (1 2 5 7))
    (?I . (2 4 7))
    (?J . (2 4 5 7))
    (?K . (1 3 7))
    (?L . (1 2 3 7))
    (?M . (1 3 4 7))
    (?N . (1 3 4 5 7))
    (?O . (1 3 5 7))
    (?P . (1 2 3 4 7))
    (?Q . (1 2 3 4 5 7))
    (?R . (1 2 3 5 7))
    (?S . (2 3 4 7))
    (?T . (2 3 4 5 7))
    (?U . (1 3 6 7))
    (?V . (1 2 3 6 7))
    (?W . (2 4 5 6 7))
    (?X . (1 3 4 6 7))
    (?Y . (1 3 4 5 6 7))
    (?Z . (1 3 5 6 7))
    (?\[ . (2 4 6 7))
    (?\\ . (1 2 5 6 7))
    (?\] . (1 2 4 5 6 7))
    (?^ . (4 5 7))
    (?_ . (4 5 6))
    (?` . (4))
    (?a . (1))
    (?b . (1 2))
    (?c . (1 4))
    (?d . (1 4 5))
    (?e . (1 5))
    (?f . (1 2 4))
    (?g . (1 2 4 5))
    (?h . (1 2 5))
    (?i . (2 4))
    (?j . (2 4 5))
    (?k . (1 3))
    (?l . (1 2 3))
    (?m . (1 3 4))
    (?n . (1 3 4 5))
    (?o . (1 3 5))
    (?p . (1 2 3 4))
    (?q . (1 2 3 4 5))
    (?r . (1 2 3 5))
    (?s . (2 3 4))
    (?t . (2 3 4 5))
    (?u . (1 3 6))
    (?v . (1 2 3 6))
    (?w . (2 4 5 6))
    (?x . (1 3 4 6))
    (?y . (1 3 4 5 6))
    (?z . (1 3 5 6))
    (?{ . (2 4 6))
    (?| . (1 2 5 6))
    (?} . (1 2 4 5 6))
    (?~ . (4 5))
    (t . (4 5 6 7)))
  "Alist mapping characters to braille codes.
Each braille code is represented by a list of numbers idenitifying the dot
positions.
The entry with the symbol t as its car maps to the default code to display
instead of undefined characters.")


(define-derived-mode braille-display-mode fundamental-mode "Braille"
  "Mode for displaying braille display emulator."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (defvar speechd-speak-mode)
  (set (make-local-variable 'speechd-speak-mode) nil))


(defun braille-display (string &optional cursor-position)
  (with-current-buffer (get-buffer-create braille-buffer-name)
    (braille-display-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "\n\n\n\n\n")
      (dolist (c (append string nil))
        (let ((code (cdr (or (assoc c braille-table) (assq t braille-table)))))
          (when cursor-position
            (if (= cursor-position 0)
                (setq code (cons 8 code)
                      cursor-position nil)
              (cl-decf cursor-position)))
          (cl-flet ((display-dot (dot suffix)
                      (end-of-line)
                      (insert (if (member dot code) "o" " ") suffix)
                      (forward-line)))
            (goto-char (point-min))
            (mapc #'(lambda (d) (display-dot d " ")) '(1 2 3 7))
            (goto-char (point-min))
            (mapc #'(lambda (d) (display-dot d "  ")) '(4 5 6 8))))
        (goto-char (point-max))
        (insert (format " %c   " c)))
      (when (equal cursor-position 0)
        (end-of-line -1)
        (insert "  o")))))


;;; Announce

(provide 'braille)


;;; braille.el ends here
