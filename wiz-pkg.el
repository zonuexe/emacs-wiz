;;; wiz-pkg.el --- Package Manager integrations for wiz   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 01 Jan 2024
;; Version: 0.0.3
;; Keywords: convenience, lisp
;; Homepage: https://github.com/zonuexe/emacs-wiz
;; Package-Requires: ((emacs "29.1"))
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

;; This feature provides a typical workflow for installing a package if it is not installed.
;;
;; It is designed for wiz's :package keyword, but can also be used as a standalone feature.
;;
;; For example:
;;
;;     (wiz-pkg 'package-el 'php-mode) ;; Install from any ELPA using package-install
;;     (wiz-pkg 'nongnu 'php-mode) ;; Install from NonGNU ELPA using package-install
;;
;; The handler should install the package immediately when called.
;; When called from the wiz macro, it installs the package when byte-compiling init.el,
;; so it does nothing at runtime.  Instead, the handler should return an S-expression
;; that is evaluated at runtime.  If it does nothing at runtime, it simply returns NIL.
;;
;; Handlers other than package.el are not implemented.
;; I'm not a strait user, so if anyone wants to use it, please send me Pull Request.
;;

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'package)
(require 'borg nil t)

(defgroup wiz-pkg nil
  "Package Manager integrations for wiz."
  :group 'wiz
  :group 'convenience)

(defcustom wiz-pkg-default-type 'package-el
  "Symbol of default package type."
  :type 'symbol
  :group 'wiz-pkc)

(defcustom wiz-pkg-enable-log nil
  "If non-NIL, log registerd packages."
  :type 'boolean
  :group 'wiz-pkg)

(defvar wiz-pkg-handler-alist
  '((package-el . wiz-pkg-package-el-handler)
    (gnu . wiz-pkg-package-el-handler)
    (nongnu . wiz-pkg-package-el-handler)
    (gnu-devel . wiz-pkg-package-el-handler)
    (nongnu-devel . wiz-pkg-package-el-handler)
    ;; (borg . wiz-pkg-borg-handler)
    ;; (straight . wiz-pkg-straight-handler)
    ;; (el-get . wiz-pkg-el-get-handler)
    (melpa . wiz-pkg-package-el-handler)
    (melpa-stable . wiz-pkg-package-el-handler)))

(defvar wiz-pkg--registerd-packages nil)

(defun wiz-pkg--ensure-string (value)
  "Return a string from VALUE."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   ((error "Unexpected value %s" value))))

(defun wiz-pkg--ensure-symbol (value)
  "Return a symbol from VALUE."
  (cond
   ((symbolp value) value)
   ((stringp value) (intern value))
   ((error "Unexpected value %s" value))))

(defun wiz-pkg-package-el-handler (type package &optional _params)
  "Install PACKAGE with TYPE and PARAMS using `package-install'."
  (let ((pin-archive (if (eq type 'package-el) nil type)))
    (append
     (when pin-archive
       (list
        (let* ((pair `(cons (quote ,package) ,(wiz-pkg--ensure-string pin-archive)))
               (pin `(unless (member ,pair package-pinned-packages)
                       (setopt package-pinned-packages
                               (cons ,pair package-pinned-packages))))
               (log (when wiz-pkg-enable-log
                      `(push ,pair (alist-get 'package-el wiz-pkg--registerd-packages))))
               (sexp (macroexp-progn (list pin log))))
          (prog1 sexp
            (eval sexp)))))
     (prog1 nil
       (unless (package-installed-p package)
         (when pin-archive
           (package-refresh-contents)
           (package-read-all-archive-contents))
         (package-install package))))))

(defun wiz-pkg (type package &optional params)
  "Install PACKAGE with TYPE and PARAMS."
  (let* ((type (wiz-pkg--ensure-symbol type))
         (handler (cdr-safe (assq type wiz-pkg-handler-alist))))
    (message "handler: %S" handler)
    (macroexp-progn
     (funcall handler type package params))))

(provide 'wiz-pkg)
;;; wiz-pkg.el ends here
