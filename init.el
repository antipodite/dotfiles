(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
             t)

;; UI settings
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(font . "Iosevka Light-10"))
(set-frame-font "Iosevka Light-10") 
(load-theme 'kaolin-aurora t)
(blink-cursor-mode 0)
(toggle-scroll-bar -1)
(show-paren-mode 1)
(column-number-mode 1)
(setq ring-bell-function 'ignore)
(redtick-mode t)
(global-set-key (kbd "C-u") 'undo)
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(size-indication-mode t)
(hl-line-mode 1)
(setq scroll-conservatively 1)
(setq mouse-wheel-scroll-amount '(5))
(setq mouse-wheel-progressive-speed nil)

;; Control how emacs makes backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/saves/" t)))

;; Editing defaults
(setq-default indent-tabs-mode nil)


;; SLIME common lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(slime-setup '(slime-fancy slime-company))
(setq inferior-lisp-program "sbcl")
(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))
(add-hook 'slime-mode-hook 'cliki:start-slime)
(add-hook 'slime-mode-hook (lambda ()
                             (rainbow-delimiters-mode t)
                             (prettify-symbols-mode)
                             (setf line-spacing 0.3)))

;; AUCtex
;; I want word boundary wrapping when writing prose
(add-hook 'LaTeX-mode-hook (lambda ()
                             (visual-line-mode t)
                             (setf line-spacing 0.4)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("c342ef444e7aca36f4b39a8e2848c4ba793d51c58fdb520b8ed887766ed6d40b" "1de8de5dddd3c5138e134696180868490c4fc86daf9780895d8fc728449805f3" "17a58e509bbb8318abf3558c4b7b44273b4f1b555c5e91d00d4785b7b59d6d28" "9ef81da35ce99a4c7155db7d46e4f8c20a51860d6879cf082e3ed1c5222c17d3" "8ce796252a78d1a69e008c39d7b84a9545022b64609caac98dc7980d76ae34e3" "6a0d7f41968908e25b2f56fa7b4d188e3fc9a158c39ef680b349dccffc42d1c8" "ff4d091b20e9e6cb43954e4eeae1c3b334e28b5923747c7bd5d2720f2a67e272" "03c32698863b38cb07bf7e6a54b6c1de81f752a6c4eab3642749007d5dcf0aef" "ab3bf0dd6507d10dcf1b63769e7bfc180d8332266d3db27cc7b2e8323ff02ae4" "ff6c5a15591b98a58a7a68a969f7143e3e663991c31bf55ff0807f17e223af4b" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (hl-fill-column magit kaolin-themes slime-company company planet-theme creamsody-theme sunburn-theme doom-themes redtick dashboard use-package spaceline circadian racket-mode ## spacemacs-theme paredit-menu circe rainbow-delimiters paredit slime-repl-ansi-color yascroll auctex centaur-tabs treemacs slime nord-theme)))
 '(spaceline-helm-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
