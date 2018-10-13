;; init --- My emacs init file

;;; Commentary:
;; Glad if you find something useful, but if you want to change
;; functionality, you need to fork this

;;; Code:

;; Add MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Initialize packages
(package-initialize)

;; Set up use-package -command
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up try -package
(use-package try
  :ensure t)

;; Remove startup message
(setq inhibit-startup-message t)

;; Make *scratch* empty
(setq initial-scratch-message "")

;; Set window size
(when window-system (set-frame-size (selected-frame) 140 40))

;; Set theme and customize it
(use-package paganini-theme
  :ensure t
  :config (progn
	    (load-theme 'paganini t)
	    (set-face-background 'mode-line "VioletRed2")))

;; Remove menubar, toolbar and scrollbar
(menu-bar-mode -99)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable linum-mode with custom format
(global-linum-mode t)
(defvar linum-format "%d ")

;; Do not indent with tabs (autoindent)
(setq indent-tabs-mode nil)

;; Set default directory to home
(setq default-directory "~/")

;; Don't save FILENAME~ and #FILENAME# -files at the same directory with the originals
(defun my-backup-file-name (fpath)
  "Return a new file path of a given FPATH.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/backups/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
))
(setq make-backup-file-name-function 'my-backup-file-name)

;; Removes *Completions* from buffers after file has been opened
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Dont show *Buffer list* when opening multiple files at the same time
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time
(add-hook 'window-setup-hook 'delete-other-windows)

;; Replace yes/no with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Set neotree icon theme if it's graphic display
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t)
(use-package all-the-icons-gnus
  :ensure t)
(use-package all-the-icons-ivy
  :ensure t)
(defvar neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Enable .js extension for rjsx
(use-package rjsx-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

;; Set Firefox as default flymd browser
(defun my-flymd-browser-function (url)
  "Open flymd URL."
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(use-package flymd
  :ensure t
  :config (setq flymd-browser-open-function 'my-flymd-browser-function))

;; Remove selection and overwrite
(delete-selection-mode 1)

;; Prettify lambda in python
(defun update-prettify-symbols-alist ()
  "List of symbols for prettify."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))
(use-package pretty-mode
  :ensure t
  :hook ((prettify-symbols-mode . update-prettify-symbols-alist)
	 (python-mode . prettify-symbols-mode)))

;; Smoother scrolling (leaves room at the edges in form of "prescroll")
(use-package smooth-scrolling
  :ensure t
  :config (smooth-scrolling-mode 1))

;; Smart parens
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode t))

;; Set up ido
(defvar ido-enable-flex-matching t)
(defvar ido-everywhere t)
(ido-mode 1)

;; Use ibuffer for C-x C-b
(defalias 'list-buffers 'ibuffer)

;; Smart command repetition
(use-package smartrep
  :ensure t)

;; More string functions (Mainly used with my yasnippets)
(use-package string-inflection
  :ensure t)

;; Import misc packages
(use-package yasnippet
  :ensure t)

(use-package auto-sudoedit
  :ensure t)

(use-package cinspect
  :ensure t)

(use-package jedi-direx
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package cython-mode
  :ensure t)

(use-package flycheck-cython
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package spaceline
  :ensure t
  :config (spaceline-spacemacs-theme))

(use-package org-bullets
  :ensure t
  :hook ((org-mode . org-bullets-mode)))

(use-package 0xc
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(use-package htmlize
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode 1)
  :hook (flycheck-mode . flycheck-pycheckers-setup))

(use-package flycheck-pycheckers
  :ensure t
  :config (setq flycheck-pycheckers-checkers '(pylint)))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Set custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (which-key try use-package)))
 '(smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.)
 )

;; Reloads init.el file with f5 key
(defun reload-initel ()
  "Reloads init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f5>") 'reload-initel)

;; Set neotree custom navigation shortcuts
(use-package neotree
  :ensure t
  :bind (("C-å" . neotree-previous-line)
         ("C-ä" . neotree-next-line)
         ("C-S-n" . neotree-toggle)))

;; Emacs key bindings for evil-nerd-commenter
(use-package evil-nerd-commenter
  :ensure t
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

;; Set up vimish fold
(use-package vimish-fold
  :ensure t
  :bind (("C-<dead-acute>" . vimish-fold-toggle)))
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(defvar vimish-fold-persist-on-saving t)

;; Set up expand-region
(use-package expand-region
  :ensure t
  :bind (("M-h" . er/expand-region)))

;; Set up avy
(use-package avy
  :ensure t
  :bind (("C-c k" . avy-goto-char)
         ("C-c j" . avy-goto-word-1)
         ("C-c l" . avy-goto-line)))

;; Set ace-window keys
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r ?t)))

;; Set mark at current point and go to beginning of the buffer
(defun set-mark-and-import ()
  "Set mark at current point and go to beginning of the buffer."
  (interactive)
  (set-mark-command nil)
  (deactivate-mark)
  (goto-char (point-min))
  (open-line nil))
(global-set-key (kbd "C-c i") 'set-mark-and-import)

;; Sort current paragraph and return to the previous mark
(defun sort-imports-and-return ()
  "Sort current paragraph and return to the previous mark."
  (interactive)
  (mark-paragraph)
  (sort-lines nil (region-beginning) (region-end))
  (pop-to-mark-command)
  (pop-to-mark-command)
  (recenter-top-bottom nil))
(global-set-key (kbd "C-c m") 'sort-imports-and-return)

;; Spotify controlling
(use-package spotify
  :ensure t
  :bind (("M-å" . spotify-playpause)
         ("M-ä" . spotify-next)
         ("M-ö" . spotify-previous))
  :config (spotify-enable-song-notifications))

;; Reversible C-x 1 (for temporary fullscreen of a buffer)
(use-package zygospore
  :ensure t
  :config (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

;; Set up multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-next-like-this)
         ("C-M-<" . mc/skip-to-next-like-this)
         ("C-c r" . mc/mark-all-in-region))
  :config (define-key mc/keymap (kbd "<return>") nil))
(use-package mc-extras
  :ensure t
  :bind (("C-c f" . mc/freeze-fake-cursors)
         ("C-c u" . mc/unfreeze-fake-cursors)
         ("C-c c" . mc/remove-cursors-on-blank-lines)))
(require 'mc-cycle-cursors)

;; Execute sexp on pointer globally and replace it with the return value
(defun replace-eval-last-sexp ()
  "Execute sexp on pointer globally and replace it with the return value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))
(global-set-key (kbd "C-c e") 'replace-eval-last-sexp)

;; Set up ace-jump
(use-package ace-jump-buffer
  :ensure t
  :bind (("C-x b" . ace-jump-buffer)))

;; Set up swiper, more interactive version of isearch
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Set up jedi
(use-package jedi
  :ensure t
  :config (progn
            (jedi:install-server)
            (setq jedi:complete-on-dot t))
  :hook ((python-mode . jedi:setup)))

(use-package counsel
  :ensure t
  :bind (("M-y" . counsel-yank-pop)
	 :map ivy-minibuffer-map ("M-y" . ivy-next-line)))

;;; Stupid flychecker wants this to be present at the end of the file: (provide 'init)
;;; init.el ends here
