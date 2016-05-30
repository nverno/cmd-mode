;;; company-cmd.el --- company-mode completion backend for cmd.exe scripts -*- lexical-binding: t -*-

;; Copyright (C) 2016 Noah Peart

;; Author: Noah Peart
;; URL: https://github.com/nverno/cmd-mode
;; Version: 1.0
;; Package-Requires ((company "0.8.0") (cl-lib "0.5.0"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple company-mode completion backend for windows batch/dos scripting modes.
;; Only offers completion for 80-90 commands returned by 'help'.

;;; Code:

(require 'company)
(require 'cl-lib)

(defvar company-cmd-modes '(bat-mode dos-mode cmd-mode ntcmd-mode)
  "Various modes for editing windows batch files.")

(defvar company-cmd-candidates nil
  "List of completion candidates and meta info.")

(defun company-cmd-build-alist ()
  "Build list of commands to offer for completion."
  (let ((raw
         (split-string
          (replace-regexp-in-string
           "\n\\s-+" " "
           (shell-command-to-string
            "help | findstr /b /v /c:\"For more\"")) "\n" t)))
    (setq company-cmd-candidates
          (mapcar #'(lambda (str)
                      (let ((pos (string-match "\\s-" str)))
                        (cons (substring str 0 pos)
                              (replace-regexp-in-string
                               "^\\s-+" " "
                               (substring str (1+ pos) (length str))))))
                  raw)))
  company-cmd-candidates)

(defun company-cmd-prefix ()
  "Prefix for bat completion."
  (and (memq major-mode company-cmd-modes)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-cmd-meta (candidate)
  "Return short documentation string for `CANDIDATE'."
  (cdr (assoc-string candidate company-cmd-candidates t)))

(defun company-cmd-doc (candidate)
  "Return buffer with detailed help for `CANDIDATE'."
  (with-temp-buffer
    (call-process "cmd.exe" nil t nil "/c" (concat " help " candidate))
    (goto-char (point-min))
    (company-doc-buffer
     (buffer-substring-no-properties (line-beginning-position)
                                     (point-max)))))

(defun company-cmd-offer-candidates (arg)
  "Offer completion candidates, if an uppercase character is found,
the candidates are all uppercase."
  (let* ((case-fold-search nil)
        (to-upper (string-match-p "[A-Z]+" arg))
        (completion-ignore-case t)
        (res (all-completions arg (or company-cmd-candidates
                                      (company-cmd-build-alist)))))
    (if to-upper res
      (mapcar #'downcase res))))

;;;###autoload
(defun company-cmd (command &optional arg &rest ignored)
  "Windows batch/dos script backend for company-mode."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-cmd))
    (prefix (company-cmd-prefix))
    (meta (company-cmd-meta arg))
    (candidates (company-cmd-offer-candidates arg))
    (doc-buffer (company-cmd-doc arg))
    (sorted t)))

(provide 'company-cmd)

;;; company-cmd.el ends here
