(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/")
             t)

;; UI settings
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
	     '(font . "Iosevka-11"))
(set-default-font "Iosevka-11") 
(load-theme 'sunburn t)
(blink-cursor-mode 0)
;(global-display-line-numbers-mode)
(setq visible-bell 1)
(toggle-scroll-bar -1)
(show-paren-mode 1)
(column-number-mode 1)
(redtick-mode t)
(global-set-key (kbd "C-u") 'undo)
(add-to-list 'default-frame-alist
             '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(size-indication-mode t)

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

;; Enable copy/paste to Emacs terminal windows on OSX
(defun copy-from-osx ()
   (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
   (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
         (process-send-string proc text)
         (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; SLIME common lisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(defun cliki:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))
(add-hook 'slime-mode-hook 'cliki:start-slime)
(add-hook 'slime-mode-hook (lambda () (rainbow-delimiters-mode t)))
(add-hook 'slime-mode-hook (lambda () (prettify-symbols-mode)))

;; AUCtex
;; I want word boundary wrapping when writing prose
(add-hook 'LaTeX-mode-hook (lambda ()
                             (visual-line-mode t)
                             (setf line-spacing 0.4)))

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed. By Kaushal Modi https://emacs.stackexchange.com/a/24461"
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("ff4d091b20e9e6cb43954e4eeae1c3b334e28b5923747c7bd5d2720f2a67e272" "03c32698863b38cb07bf7e6a54b6c1de81f752a6c4eab3642749007d5dcf0aef" "ab3bf0dd6507d10dcf1b63769e7bfc180d8332266d3db27cc7b2e8323ff02ae4" "ff6c5a15591b98a58a7a68a969f7143e3e663991c31bf55ff0807f17e223af4b" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (planet-theme creamsody-theme sunburn-theme doom-themes redtick dashboard use-package spaceline circadian racket-mode ## spacemacs-theme paredit-menu circe rainbow-delimiters paredit slime-repl-ansi-color yascroll auctex centaur-tabs treemacs slime nord-theme)))
 '(spaceline-helm-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
