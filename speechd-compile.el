;;; speechd-compile.el --- Maintenance utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2004, 2013, 2021 Milan Zamazal <pdm@zamazal.org>

;; Author: Milan Zamazal <pdm@zamazal.org>

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

(defun speechd-compile ()
  (let ((load-path (cons default-directory load-path)))
    (dolist (file (directory-files "." nil "\\.el$"))
      (unless (member file '("speechd-compile.el"
                             "speechd-el-pkg.el"))
        (load file)
        (byte-compile-file file)))))


;;; Announce

(provide 'speechd-compile)


;;; speechd-compile.el ends here
