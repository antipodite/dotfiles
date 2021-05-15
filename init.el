;; Initialize package system

(setq package-archives
      '(("org"     .       "https://orgmode.org/elpa/")
        ("gnu"     .       "https://elpa.gnu.org/packages/")
        ("melpa"   .       "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents t)

;; Use-package for civilized configuration

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

;; Packages

(use-package nyan-mode
  :config (nyan-mode t))

(use-package kaolin-themes)

(use-package redtick
  :config (redtick-mode t))

(use-package slime
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (slime-setup '(slime-fancy slime-company slime-indentation))
  (setq inferior-lisp-program "sbcl")
  (defun cliki:start-slime ()
    (unless (slime-connected-p)
      (save-excursion (slime))))
  (add-hook 'slime-mode-hook 'cliki:start-slime))

(use-package company)
(use-package slime-company)
(use-package rainbow-delimiters
  :config 
  (add-hook 'slime-mode-hook (lambda ()
                               (rainbow-delimiters-mode t)
                               (prettify-symbols-mode)
                               (setf line-spacing 0.3))))

(use-package magit)
(use-package auctex
  :defer t
  :config (add-hook 'LaTeX-mode-hook
                    (lambda ()
                      (visual-line-mode t)
                      (setf line-spacing 0.3))))

(use-package minions
  :config (minions-mode 1))

(use-package org
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode t)
              (setf line-spacing 0.3))))

;; UI settings
(visual-line-mode t)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(font . "Iosevka Light-11"))
(set-frame-font "Iosevka Light-11") 
(load-theme 'kaolin-valley-light t)
(blink-cursor-mode 0)
(toggle-scroll-bar -1)
(show-paren-mode 1)
(column-number-mode 1)
(setq ring-bell-function 'ignore)
(add-to-list 'default-frame-alist
             '(fullscreen . maximized)
             '(vertical-scroll-bars nil))
(setq inhibit-startup-screen t)
(size-indication-mode t)
(hl-line-mode 1)
(setq scroll-conservatively 1)
(setq mouse-wheel-scroll-amount '(5))
(setq mouse-wheel-progressive-speed nil)
(setq-default indent-tabs-mode nil)

(windmove-default-keybindings)

;; Make emacs store backup files in .emacs
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/saves/" t)))

;; Autorefresh images and PDF documents in viewer
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; Disable scroll bars on new frames
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; Switch focus to minibuffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(global-set-key "\C-co" 'switch-to-minibuffer) ;; Bind to `C-c o'

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c1c459af570241993823db87096bc775506c378aa02c9c6cd9ccaa8247056b96" default))
 '(package-selected-packages
   '(minions auctex magit rainbow-delimiters slime-company company slime redtick kaolin-themes nyan-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
