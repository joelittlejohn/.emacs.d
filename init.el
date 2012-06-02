(add-to-list 'load-path "~/.emacs.d/")
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

;; charcoal theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)
