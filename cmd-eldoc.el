;;; cmd-eldoc --- eldoc support -*- lexical-binding: t; -*-

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/cmd-mode
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created:  9 October 2016

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Eldoc support for DOS batch scripts.
;; Will be setup automatically if `eldoc-mode' is active, otherwise enable with
;;
;; (add-function :before-until (local 'eldoc-documentation-function)
;;              #'cmd-eldoc-function)
;;
;;; Code:
(eval-when-compile
  (require 'subr-x))

;; case-insensitive hash
(defun cmd-hash-test (s1 s2)
  (eq t (compare-strings s1 nil nil s2 nil nil t)))
(defun cmd-case-fold-hash (s)
  (sxhash (upcase s)))
(define-hash-table-test 'cmd-hash 'cmd-hash-test 'cmd-case-fold-hash)
(defvar cmd-eldoc-hash (make-hash-table :test 'cmd-hash))

;; get name of current function
(defun cmd-eldoc-current-function ()
  (let ((ppss (syntax-ppss))
        (start (or (cdr (bounds-of-thing-at-point 'symbol)) (point))))
    (when (not (nth 4 ppss))
      (save-excursion 
        (save-match-data
          (skip-chars-backward "^=><|)" (line-beginning-position))
          (when (re-search-forward "\\s-*\\([-A-Za-z]+\\)" start t)
            (match-string-no-properties 1)))))))

(defvar cmd-eldoc-buffer "*cmd-eldoc*")
(defvar cmd-eldoc-current-function)
(defun cmd-eldoc-sentinel (_m _p)
  (with-current-buffer cmd-eldoc-buffer
   (goto-char (point-min))
   (forward-line 2)
   (puthash cmd-eldoc-current-function
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))
    cmd-eldoc-hash)))

(defun cmd-eldoc-parse-help (func)
  (with-current-buffer (get-buffer-create cmd-eldoc-buffer)
    (erase-buffer))
  (set-process-sentinel
   (start-process "cmd.exe" cmd-eldoc-buffer "cmd.exe" "/c" "help" func)
   #'cmd-eldoc-sentinel))

(defun cmd-eldoc-lookup-function (func)
  (or (gethash func cmd-eldoc-hash)
      (and (setq cmd-eldoc-current-function func)
           (cmd-eldoc-parse-help func)
           nil)))

;;;###autoload
(defun cmd-eldoc-function ()
  "Display function parameters in minibuffer with eldoc."
  (when-let ((func (cmd-eldoc-current-function)))
    (cmd-eldoc-lookup-function func)))

(provide 'cmd-eldoc)
;;; cmd-eldoc.el ends here
