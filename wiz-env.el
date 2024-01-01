;;; wiz-env.el --- Macros to simplify startup initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 01 Dec 2023
;; Version: 0.0.1
;; Keywords: convenience, lisp
;; Homepage: https://github.com/zonuexe/emacs-wiz
;; Package-Requires: ((emacs "29.1") (exec-path-from-shell "2.1"))
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Optimize GUI Emacs startup overhead by importing environment variables during byte compilation.

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'exec-path-from-shell)

(defun wiz-env--1 (name envs)
  "Construct S-expressions from NAME and ENVS."
  (let* ((value (cdr (assoc name envs)))
         (setenv `(setenv ,name ,value)))
    (cond
     ((null value) nil)
     ((not (string-equal "PATH" name)) setenv)
     ((let ((separated (parse-colon-path value)))
        `(progn
           ,setenv
           (setq exec-path (list ,@separated exec-directory))))))))

(defmacro wiz-env (name)
  "Import NAME environment variable and expand it."
  `(unless window-system
     ,(wiz-env--1 name (exec-path-from-shell-getenvs (list name)))))

(defmacro wiz-envs (&rest names)
  "Import NAMES environment variable and expand it."
  `(unless window-system
     (prog1 (list ,@names)
       ,@(let* ((names-list (mapcar #'eval names))
                (envs (exec-path-from-shell-getenvs names-list)))
           (cl-loop for name in names-list
                    for sexp = (wiz-env--1 name envs)
                    if sexp
                    append (if (eq 'progn (car sexp))
                               (cdr sexp)
                             (list sexp)))))))

(provide 'wiz-env)
;;; wiz-env.el ends here