(delete-selection-mode 1)
(global-auto-revert-mode 1)
(global-hl-line-mode 1)
(icomplete-mode 1)
(menu-bar-mode -1)
(recentf-mode 1)
(savehist-mode 1)
(setq auto-save-default nil)
(setq column-number-mode t)
(setq inhibit-startup-screen t)
(setq load-prefer-newer t)
(setq make-backup-files nil)
(show-paren-mode 1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'load-path "~/.emacs.d/elisp")

;; INSTALL MELPA STABLE

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("melpa" . 0)))
(package-initialize)

;;; INSTALL/CONFIGURE PACKAGES

(unless (package-installed-p 'color-theme)
  (package-install 'color-theme))

(unless (package-installed-p 'auto-compile)
  (package-install 'auto-compile))
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(unless (package-installed-p 'auto-complete)
  (package-install 'auto-complete))
(global-auto-complete-mode t)

(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

(unless (package-installed-p 'cider)
  (package-install 'cider))
(setq cider-prompt-for-symbol nil)
(setq cider-overlays-use-font-lock t)

(unless (package-installed-p 'highlight)
  (package-install 'highlight))
(unless (package-installed-p 'eval-sexp-fu)
  (package-install 'eval-sexp-fu))
(unless (package-installed-p 'cider-eval-sexp-fu)
  (package-install 'cider-eval-sexp-fu))

(unless (package-installed-p 'clj-refactor)
  (package-install 'clj-refactor))

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(dashboard-setup-startup-hook)

(unless (package-installed-p 'git-gutter)
  (package-install 'git-gutter))
(global-git-gutter-mode +1)

(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))
(let ((width 2))
  (setq js-indent-level width)
  (setq json-reformat:indent-width width)
  (setq tab-width width))

(unless (package-installed-p 'nyan-mode)
  (package-install 'nyan-mode))
(nyan-mode)
(nyan-start-animation)

(unless (package-installed-p 'paredit)
  (package-install 'paredit))

(unless (package-installed-p 'rainbow-delimiters)
  (package-install 'rainbow-delimiters))

(unless (package-installed-p 'flx-ido)
  (package-install 'flx-ido))
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(unless (package-installed-p 'undo-tree)
  (package-install 'undo-tree))
(global-undo-tree-mode)

(unless (package-installed-p 'browse-kill-ring)
  (package-install 'browse-kill-ring))

(unless (package-installed-p 'volatile-highlights)
  (package-install 'volatile-highlights))
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;; ADD ADDITIONAL CUSTOM ELISP

(load "bindings")
(load "cyberpunk")
(color-theme-cyberpunk)

;;; CONFIGURE MODES

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

;;; ADDITIONAL FUNCTIONS

(defun delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))
