;;; wiz-kwd.el --- wiz keyword impletations          -*- lexical-binding: t; -*-

;; Copyright (C) 2024  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 06 Jan 2024
;; Version: 0.0.4
;; Keywords: lisp
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

;; wiz keyword impletations.

;;; Code:
(require 'pcase)
(eval-when-compile
  (defvar wiz--feature-name))

;; Utilities
(defun wiz-kwd--parse-form (keyword form)
  "Return expressions from FORM in KEYWORD."
  (if (not (eq (caar form) 'lambda))
      form
    (cond
     ((not (eq 1 (length form))) (error "(%S form): Accept only one argument %S" keyword form))
     ((pcase-let ((`(lambda . (() . ,body)) (car form))) body))
     ((error "(%S form): %S i unexpected form" keyword form)))))

;; (wiz-kwd--parse-form :hoge '((lambda () 'foo 'bar)))
;; (wiz-kwd--parse-form :hoge '('foo 'bar))

;; Keywords
(defun wiz-kwd-package-assert-before (expr)
  "Assert EXPR for wiz :package keyword."
  (unless (or (stringp expr) (symbolp expr) (eq expr t) (listp expr))
    (error "(:package form): `form' is invalid")))

(defun wiz-kwd-config-transform (form)
  "Transform FORM for wiz :config keyword."
  (list
   (cons 'with-eval-after-load
         (cons (list 'quote wiz--feature-name)
               (wiz-kwd--parse-form :config form)))))

(defun wiz-kwd-init-transform (form)
  "Transform FORM for wiz :init keyword."
  (list
   (cons 'prog1 (cons nil (wiz-kwd--parse-form :init form)))))

(provide 'wiz-kwd)
;;; wiz-kwd.el ends here
