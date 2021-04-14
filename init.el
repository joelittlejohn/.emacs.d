;;; init.el --- Emacs config of Joe Littlejohn
;;; Commentary:

;; An Emacs configuration inspired by the features of emacs-live by
;; Sam Aaron.  A configuration for Clojure, Terraform, bash, and more.
;;
;; My goal here is to create a leaner configuration than emacs-live -
;; one that has similar useful minor modes and configuration for Clojure
;; but starts more quickly and allows me to use the latest cider (&
;; family).

;;; Code:

(delete-selection-mode 1) ; overwrite selected text when you type or paste
(global-auto-revert-mode 1) ; immediately reload a buffer if it changes on disk
(global-hl-line-mode 1) ; show a faint highlight on the current line
(icomplete-mode 1) ; show matching completions as you type in the minibuffer
(menu-bar-mode -1) ; hide the menu bar from emacs windows (File|Edit|Options...)
(toggle-scroll-bar -1) ; hide scrollbars in buffers
(tool-bar-mode -1) ; hide the toolbar (row of icons at the top of the window)
(recentf-mode 1) ; remember recently opened files
(savehist-mode 1) ; save minibuffer histories to suggest recently used files/functions/etc
(setq auto-save-default nil) ; turn off autosave on files
(setq column-number-mode t) ; include current column number in the modeline
(setq-default indent-tabs-mode nil) ; disable tabs for indentation
(setq inhibit-startup-screen t) ; don't show an Emacs start buffer
(setq load-prefer-newer t) ; prefer to load newer .el files over existing .elc (compiled) files
(setq make-backup-files nil) ; stop emacs creating backup copies of the file being edited
(setq warning-minimum-level :emergency) ; reduce warnings logged
(show-paren-mode 1) ; highlight matching pairs of parens
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; fix trailing whitespace errors when saving
(defalias 'yes-or-no-p 'y-or-n-p) ; always allow y an n to mean yes and no
(add-to-list 'load-path "~/.emacs.d/elisp") ; allow custom scripts to be dropped in ~/.emacs.d/elisp

(if (not (display-graphic-p))
    (global-display-line-numbers-mode))

;; INSTALL MELPA STABLE

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 0)
        ("melpa" . 20)))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;;; INSTALL/CONFIGURE PACKAGES

(use-package auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(use-package auto-complete)
(global-auto-complete-mode t)

(use-package browse-kill-ring)

(use-package clojure-mode)

(use-package cider)
(setq cider-prompt-for-symbol nil) ; stop cider asking for confirmation before every C-. navigation
(setq cider-overlays-use-font-lock t)

(use-package clj-refactor)
(setq cljr-magic-require-namespaces
  '(("async" . "clojure.core.async")
    ("component" . "com.stuartsierra.component")
    ("csk" . "camel-snake-kebab.core")
    ("edn" . "clojure.edn")
    ("far" . "taoensso.faraday")
    ("fum" . "flatland.useful.map")
    ("fus" . "flatland.useful.seq")
    ("http" . "clj-http.client")
    ("io" . "clojure.java.io")
    ("json" . "cheshire.core")
    ("jdbc" . "clojure.java.jdbc")
    ("log" . "clojure.tools.logging")
    ("memo" . "clojure.core.memoize")
    ("s" . "clojure.spec.alpha")
    ("set" . "clojure.set")
    ("sgen" . "clojure.spec.gen.alpha")
    ("stest" . "clojure.spec.test.alpha")
    ("str" . "clojure.string")
    ("time" . "clj-time.core")
    ("time-format" . "clj-time.format")
    ("time-coerce" . "clj-time.coerce")
    ("walk" . "clojure.walk")))

(use-package highlight)
(use-package eval-sexp-fu)
(use-package cider-eval-sexp-fu)
(require 'cider-eval-sexp-fu)

(use-package color-theme)
(require 'color-theme)
(require 'cyberpunk)
(color-theme-cyberpunk)

(use-package dockerfile-mode)

(use-package dotenv-mode)

(use-package flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(use-package flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package flycheck-clj-kondo)
(require 'flycheck-clj-kondo)

(use-package git-gutter)
(global-git-gutter-mode +1)

(use-package go-mode)

(use-package goto-last-change)
(global-set-key (kbd "C-c C-/") 'goto-last-change)

(use-package hl-todo)
(global-hl-todo-mode)

(use-package json-mode)
(let ((width 2))
  (setq js-indent-level width)
  (setq json-reformat:indent-width width)
  (setq tab-width width))

(use-package magit)

(use-package markdown-mode)

(use-package nyan-mode)
(nyan-mode)
(nyan-start-animation)

(use-package paredit)

(use-package rainbow-delimiters)

(use-package smex)
(smex-initialize)

(use-package string-inflection)
(require 'string-inflection)
(global-set-key (kbd "C-c C-u") 'string-inflection-upcase)
(global-set-key (kbd "C-c C-k") 'string-inflection-lisp)

(use-package swiper)

(use-package terraform-mode)

(use-package volatile-highlights)
(require 'volatile-highlights)
(volatile-highlights-mode t)

(use-package undo-tree)
(global-undo-tree-mode)

(use-package yaml-mode)

(use-package which-key)
(require 'which-key)
(which-key-mode)

(use-package popwin)
(require 'popwin)
(popwin-mode 1)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:special-display-config
      '(("*Help*"  :height 30)
        ("*Completions*" :noselect t)
        ("*Messages*" :noselect t :height 30)
        ("*Apropos*" :noselect t :height 30)
        ("*compilation*" :noselect t)
        ("*Backtrace*" :height 30)
        ("*Messages*" :height 30)
        ("*Occur*" :noselect t)
        ("*Ido Completions*" :noselect t :height 30)
        ("*magit-commit*" :noselect t :height 40 :width 80 :stick t)
        ("*magit-diff*" :noselect t :height 40 :width 80)
        ("*magit-edit-log*" :noselect t :height 15 :width 80)
        ("\\*ansi-term\\*.*" :regexp t :height 30)
        ("*shell*" :height 30)
        (".*overtone.log" :regexp t :height 30)
        ("*gists*" :height 30)
        ("*sldb.*":regexp t :height 30)
        ("*cider-error*" :height 30 :stick t)
        ("*cider-doc*" :height 30 :stick t)
        ("*cider-src*" :height 30 :stick t)
        ("*cider-result*" :height 30 :stick t)
        ("*cider-macroexpansion*" :height 30 :stick t)
        ("*Kill Ring*" :height 30)
        ("*Compile-Log*" :height 30 :stick t)
        ("*git-gutter:diff*" :height 30 :stick t)))

;;; CONFIGURE MODES

(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))

(add-hook 'cider-repl-mode-hook #'paredit-mode)

(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'clj-refactor-mode)

(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(add-hook 'json-mode-hook #'paredit-mode)
(add-hook 'json-mode-hook #'rainbow-delimiters-mode)

(add-hook 'scala-mode-hook #'rainbow-delimiters-mode)

(add-hook 'terraform-mode-hook #'paredit-mode)
(add-hook 'terraform-mode-hook #'rainbow-delimiters-mode)

;;; ADDITIONAL FUNCTIONS

(defun delete-all-horizontal-space ()
  "Deletes all horizontal space between two symbols."
  (interactive)
  (just-one-space -1)
  (delete-backward-char 1))

(defun paredit-delete-horizontal-space ()
  "Deletes all horizontal space between two sexps in paredit mode."
  (interactive)
  (just-one-space -1)
  (paredit-backward-delete))

;;; ADDITIONAL BINDINGS

(when (eq system-type 'darwin)
  (setq ring-bell-function 'ignore)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(global-set-key (kbd "C-x C-<left>") 'windmove-left)
(global-set-key (kbd "C-x C-<right>") 'windmove-right)
(global-set-key (kbd "C-x C-<up>") 'windmove-up)
(global-set-key (kbd "C-x C-<down>") 'windmove-down)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-\\") 'delete-all-horizontal-space)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'paredit)
(if (display-graphic-p)
    ;; Don't define this binding for terminal Emacs, it will ruin all required key escapes
    (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square))
(define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
(define-key paredit-mode-map (kbd "M-T") 'transpose-sexps)
(define-key paredit-mode-map (kbd "M-\\") 'paredit-delete-horizontal-space)

(require 'cider)
(define-key cider-mode-map (kbd "C-c M-q") 'cider-quit)
(define-key cider-repl-mode-map (kbd "C-c M-q") 'cider-quit)
(define-key cider-mode-map (kbd "C-c M-r") 'sesman-restart)
(define-key cider-repl-mode-map (kbd "C-c M-r") 'sesman-restart)

(provide 'init)


;;; Scala/Metals
;;; The following was taken from https://scalameta.org/metals/docs/editors/emacs.html
;;;

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config (setq lsp-prefer-flymake nil))

(use-package lsp-ui)
(use-package company-lsp)

;;; init.el ends here
