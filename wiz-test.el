;;; wiz-test.el --- Test codes for wiz               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Megurine Luka

;; Author: Megurine Luka <megurine@tadsan14.local>
;; Keywords: lisp

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

;; Test code for wiz.

;;; Code:
(require 'wiz)
(require 'wiz-key)
(require 'wiz-env)
(require 'ert)
(require 'macroexp)
(require 'elisp-mode)

(ert-deftest wiz-test-wiz-map ()
  (should (equal '(prog1 'nil) (macroexpand '(wiz-map '() identity))))
  (should (equal '(prog1 '(1 2 3) 1 2 3)
                 (macroexpand '(wiz-map '(1 2 3) identity))))
  (should (equal '(prog1 '(1 2 3) 2 3 4)
                 (macroexpand '(wiz-map '(1 2 3) 1+))))
  (should (equal '(prog1 'nil) (macroexpand '(wiz-map '() #'identity)))))

(ert-deftest wiz-test-wiz-kwd--parse-form ()
  (should (equal '('foo 'bar)
                 (wiz-kwd--parse-form :hoge '((lambda () 'foo 'bar)))))
  (should (equal '('foo 'bar)
                 (wiz-kwd--parse-form :hoge '('foo 'bar)))))

(ert-deftest wiz-test-wiz-env-1 ()
  (should (equal '(progn
                    (setenv "PATH" "/bin:/usr/bin:/path/to/bin")
                    (setq exec-path
                          (list "/bin/" "/usr/bin/" "/path/to/bin/" exec-directory)))
                 (wiz-env--1 "PATH" '(("PATH" . "/bin:/usr/bin:/path/to/bin"))))))

(ert-deftest wiz-test-wiz-pkg ()
  (let ((wiz-pkgs-handler-alist '((dummy . (lambda (type package &optional params)
                                          (list `(prog1 'dummy
                                                   :type ,type :package ,package :params ,params)))))))
    (let (wiz-pkgs-enable-log)
      (should (equal '(prog1 'dummy :type dummy :package nyan-mode :params nil)
                     (macroexpand '(wiz-pkg dummy nyan-mode)))))
    (let ((wiz-pkgs-enable-log t))
      (should (equal '(prog1 'dummy :type dummy :package nyan-mode :params nil)
                     (macroexpand '(wiz-pkg dummy nyan-mode)))))))

(ert-deftest wiz-test-wiz-keys ()
  (should (equal '(prog1 '(nil :map global-map))
                 (macroexpand '(wiz-keys ()))))
  (should (equal '(prog1 '((((kbd "RET") . 'emacs-version)
                            ("TAB" . emacs-version))
                           :map emacs-lisp-mode-map)
                    (define-key emacs-lisp-mode-map (kbd "RET") 'emacs-version)
                    (define-key emacs-lisp-mode-map "	" 'emacs-version))
                 (macroexpand '(wiz-keys (((kbd "RET") . 'emacs-version)
                                       ("TAB" . emacs-version))
                                      :map emacs-lisp-mode-map)))))

(ert-deftest wiz-test-wiz--form-to-alist ()
  (should
   (equal
    '((:package)
      (:load-if-exists "~/repo/emacs/php-mode/lisp/php-mode-autoloads.el")
      (:load)
      (:config
       (lambda nil
         (setopt php-default-major-mode 'php-mode)
         (setopt php-manual-url 'ja)
         (setopt php-mode-coding-style 'psr2)
         (setopt php-mode-template-compatibility nil)
         (setopt php-imenu-generic-expression 'php-imenu-generic-expression-simple)
         (setopt php-project-auto-detect-etags-file t)))
      (:hook-names)
      (:setup-hook
       (defun init-php-mode-setup nil
         (subword-mode 1)
         (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t)
         (setq show-trailing-whitespace t)
         (setq-local ac-disable-faces
                     '(font-lock-comment-face font-lock-string-face php-string))
         (flycheck-mode t)
         (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
         (add-to-list 'flycheck-disabled-checkers 'php-phpcs)
         (php-format-auto-mode 1)
         (setq-local completion-at-point-functions
                     (append
                      (list #'php-complete-complete-function #'tempel-complete)
                      (list
                       (cape-company-to-capf #'company-phpactor))
                      completion-at-point-functions))
         (when
             (eq 0
                 (buffer-size))
           (insert "<?php\n\n"))))
      (:init
       (lambda nil
         (add-to-list 'auto-mode-alist
                      '("\\.stub" . php-mode-maybe)))))
    (wiz--form-to-alist
     (mapcar #'car wiz-keywords)
     '(:load-if-exists "~/repo/emacs/php-mode/lisp/php-mode-autoloads.el"
                       :init
                       (lambda ()
                         (add-to-list 'auto-mode-alist '("\\.stub" . php-mode-maybe)))
                       :config
                       (lambda ()
                         (setopt php-default-major-mode 'php-mode)
                         (setopt php-manual-url 'ja)
                         (setopt php-mode-coding-style 'psr2)
                         (setopt php-mode-template-compatibility nil)
                         (setopt php-imenu-generic-expression 'php-imenu-generic-expression-simple)
                         (setopt php-project-auto-detect-etags-file t))
                       :setup-hook
                       (defun init-php-mode-setup ()
                         (subword-mode 1)
                         (add-hook 'hack-local-variables-hook 'php-ide-turn-on nil t)
                         (setq show-trailing-whitespace t)
                         (setq-local ac-disable-faces '(font-lock-comment-face font-lock-string-face php-string))
                         (flycheck-mode t)
                         (add-to-list 'flycheck-disabled-checkers 'php-phpmd)
                         (add-to-list 'flycheck-disabled-checkers 'php-phpcs)
                         (php-format-auto-mode +1)
                         (setq-local completion-at-point-functions
                                     (append (list #'php-complete-complete-function #'tempel-complete)
                                             (list (cape-company-to-capf #'company-phpactor))
                                             completion-at-point-functions))
                         (when (eq 0 (buffer-size))
                           (insert "<?php\n\n"))))))))

(provide 'wiz-test)
;;; wiz-test.el ends here
