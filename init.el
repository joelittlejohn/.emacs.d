(add-to-list 'load-path "~/.emacs.d/")

;; disable menu and toolbar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; charcoal theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

;; clojure support
(require 'clojure-mode)

;; disable *GNU Emacs* buffer on startup
(setq inhibit-startup-screen t)

;; Use only spaces (no tabs at all).
(setq-default indent-tabs-mode nil)

;; Show column numbers.
(setq-default column-number-mode t)

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)

;; rainbow brackets
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; solarized theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
