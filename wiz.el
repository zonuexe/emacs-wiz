;;; wiz.el --- Macros to simplify startup initialization  -*- lexical-binding: t; -*-

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

;; Shorthand macro for feature configuration for init.el.

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defvar wiz--feature-name)
(defvar wiz--hook-name)

(defcustom wiz-feature-hook-name-template "init-%s-setup"
  "Template for setup function name."
  :type 'string
  :group 'wiz)

(defvar wiz-keywords
  `((:load-if-exists
     :transform (lambda (v)
                  (let ((file (eval v)))
                    (list
                     `(when (file-exists-p ,file) (load ,file))))))
    (:load
     :transform (lambda (expr)
                  (list
                   (list 'load (eval expr))))
     :assert-after (lambda (v)
                     (unless (stringp (nth 1 (car v)))
                       (error "(:load file): `file' must be evalute as string %S" (car v)))))
    (:config
     :assert-before (lambda (v)
                      (unless (and (listp v) (eq 'lambda (car v)))
                        (error "(config :proc) `proc' must be lambda expression")))
     :transform (lambda (expr)
                  (list
                   (list 'with-eval-after-load (list 'quote wiz--feature-name) (caddr expr)))))
    (:hook-name
     :assert-before (lambda (name)
                      (unless (symbolp name)
                        (error "(:hook-name %s): `name' must be symbol" name))
                      (unless (boundp name)
                        (error "(:hook-name %s): `name' must be existing hook name" name)))
     :transform (lambda (name)
                  (prog1 nil
                    (setq wiz--hook-name name))))
    (:setup-hook
     :transform (lambda (expr)
                  (let ((setup-hook-name (nth 1 expr))
                        (target-hook-name
                         (or wiz--hook-name
                             (let ((name (symbol-name wiz--feature-name)))
                               (intern (format "%s-hook"
                                               (if (string-match-p "-mode" name)
                                                   name
                                                 (concat name "-mode"))))))))
                    `((add-hook ,(list 'quote target-hook-name)
                                ,(list 'function setup-hook-name))
                       ,expr))))
    (:init
     :transform (lambda (expr)
                  (list
                   `(progn ,(caddr expr)))))))

(defun wiz--assert-feature-spec (feature-name plist)
  "Assert wiz FEATURE-NAME feature spec PLIST."
  (cl-check-type feature-name symbol)
  (cl-loop for (key value) on plist by #'cddr
           for spec = (cdr-safe (assq key wiz-keywords))
           unless spec
           do (error "`%s' is unexpected keyword for wiz" key)))

(defun wiz--feature-process-1 (feature-name plist keyword spec)
  "Process wiz FEATURE-NAME feature SPEC for PLIST of KEYWORD."
  (when-let (value (plist-get plist keyword))
    (let ((assert-before (or (plist-get spec :assert-before) #'always))
          (transform (plist-get spec :transform))
          (assert-after (or (plist-get spec :assert-after) #'always))
          transformed)
      (funcall assert-before value)
      (setq transformed (funcall transform value))
      (funcall assert-after transformed)
      transformed)))

(defun wiz--feature-process (feature-name plist)
  "Process wiz FEATURE-NAME spec by PLIST."
  (let ((wiz--feature-name feature-name)
        wiz--hook-name)
    (cl-loop for (keyword . spec) in wiz-keywords
             for transformed = (wiz--feature-process-1 feature-name plist keyword spec)
             if transformed
             append transformed)))

(defmacro wiz (feature-name &rest plist)
  "Wiz for activate FEATURE-NAME with PLIST."
  (declare (indent defun))
  (wiz--assert-feature-spec feature-name plist)
  (unless (require feature-name nil t)
    (user-error "Wiz: feature `%s' is not a available feature name" feature-name))
  (cons 'prog1 (cons (list 'quote feature-name) (wiz--feature-process feature-name plist))))

(provide 'wiz)
;;; wiz.el ends here