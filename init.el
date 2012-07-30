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

;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; autopair
;(require 'autopair)
;(autopair-global-mode) ;; enable autopair in all buffers

;; remove the 'modified buffers exist...' confirmation message
(defun my-save-buffers-kill-emacs (&optional arg)
  "Offer to save each buffer(once only), then kill this Emacs process.
With prefix ARG, silently save all file-visiting buffers, then kill."
  (interactive "P")
  (save-some-buffers arg t)
  (and (or (not (fboundp 'process-list))
       ;; process-list is not defined on MSDOS.
       (let ((processes (process-list))
         active)
         (while processes
           (and (memq (process-status (car processes)) '(run stop open listen))
            (process-query-on-exit-flag (car processes))
            (setq active t))
           (setq processes (cdr processes)))
         (or (not active)
         (progn (list-processes t)
            (yes-or-no-p "Active processes exist; kill them and exit anyway? ")))))
       ;; Query the user for other things, perhaps.
       (run-hook-with-args-until-failure 'kill-emacs-query-functions)
       (or (null confirm-kill-emacs)
       (funcall confirm-kill-emacs "Really exit Emacs? "))
       (kill-emacs)))
(fset 'save-buffers-kill-emacs 'my-save-buffers-kill-emacs)
