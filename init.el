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

;; Do not ever indent with tabs
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Set up use-package -command
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always ensure the package exists
(setq use-package-always-ensure t)

;; Set up try -package
(use-package try)

;; Remove startup message
(setq inhibit-startup-message t)

;; Make *scratch* empty
(setq initial-scratch-message "")

;; Set window size
(when window-system (set-frame-size (selected-frame) 140 40))

;; Disabled due to tons of bugs
;; Show indicator at 80 char point
;; (use-package fill-column-indicator
;;   :config (progn
;; 	    (setq fci-rule-column 80)
;;             (setq fci-rule-width 7)
;;             (setq fci-rule-color "#0c0c0c")
;;             (turn-on-fci-mode)))

(use-package column-enforce-mode
  :config (global-column-enforce-mode t))

;; Set theme and customize it
;; (use-package paganini-theme
;;   :config (progn
;; 	    (load-theme 'paganini t)
;; 	    (set-face-background 'mode-line "VioletRed2")
;; 	    (set-cursor-color "VioletRed2")
;; 	    ))
;;(set-background-color "#282c34")

;; Can't decide between emacs-doom themes and paganini
;; (use-package doom-modeline)
(use-package doom-themes
  :config (progn
	    (load-theme 'doom-Iosvkem t)
	    (set-face-background 'mode-line "VioletRed2")
	    (set-cursor-color "VioletRed2")))

;; Remove menubar, toolbar and scrollbar
(menu-bar-mode -99)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable linum-mode with custom format
(global-linum-mode t)
(defvar linum-format "%d ")

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
(use-package all-the-icons)
(use-package all-the-icons-dired)
(use-package all-the-icons-gnus)
(use-package all-the-icons-ivy)
(defvar neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Enable .js extension for rjsx
(use-package rjsx-mode
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
	    (setq js2-basic-offset 2)
	    (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
	      "Workaround sgml-mode and follow airbnb component style."
	      (save-excursion
		(beginning-of-line)
		(if (looking-at-p "^ +\/?> *$")
		    (delete-char sgml-basic-offset))))
	    ))

;; Set Firefox as default flymd browser
(defun my-flymd-browser-function (url)
  "Open flymd URL."
  (let ((browse-url-browser-function 'browse-url-firefox))
    (browse-url url)))
(use-package flymd
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
  :hook ((prettify-symbols-mode . update-prettify-symbols-alist)
	 (python-mode . prettify-symbols-mode)))

;; Smoother scrolling (leaves room at the edges in form of "prescroll")
(use-package smooth-scrolling
  :config (smooth-scrolling-mode 1))

;; Smart parens
(use-package smartparens
  :config (smartparens-global-mode t))

;; Use ibuffer for C-x C-b
(defalias 'list-buffers 'ibuffer)

;; Smart command repetition
(use-package smartrep)

;; More string functions (Mainly used with my yasnippets)
(use-package string-inflection)

;; Import misc packages
(use-package yasnippet)
(use-package auto-sudoedit)
(use-package cinspect)
(use-package jedi-direx)
(use-package flycheck)
(use-package cython-mode)
(use-package flycheck-cython)
(use-package json-mode)
(use-package spaceline
  :config (progn
	    (spaceline-spacemacs-theme)
	    (setq spaceline-minor-modes-p nil)
	    (setq powerline-height 22)))
(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))
(use-package graphviz-dot-mode)
(use-package 0xc)

(defvar org-html-postamble nil)
(add-hook 'org-mode-hook 'visual-line-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (js .t)))

(use-package htmlize)

(use-package flycheck
  :config (global-flycheck-mode 1)
  :hook (flycheck-mode . flycheck-pycheckers-setup))

(use-package flycheck-pycheckers
  :config (setq flycheck-pycheckers-checkers '(pylint)))

;; Currently disabled due to not being able to change keys
;; (use-package hungry-delete
;;   :config (global-hungry-delete-mode))

(use-package which-key
  :config (which-key-mode))

;; Set custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds (quote ("https://www.archlinux.org/feeds/news/")))
 '(global-highlight-parentheses-mode t)
 '(org-agenda-files (quote ("~/todo.org")))
 '(package-selected-packages
   (quote
    (flycheck-rebar3 company-erlang latex-preview-pane virtualenvwrapper pyenv-mode beacon org-tree-slide haskell-mode eyebrowse jade-mode doom-themes doom-modeline column-enforce-mode graphviz-dot-mode vagrant tss scp atomic-chrome counsel-projectile projectile elfeed better-shell company-tern highlight-parentheses magit zlc which-key try use-package)))
 '(powerline-buffer-size-suffix t)
 '(powerline-height 22)
 '(smartparens-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Reloads init.el file with f5 key
(defun reload-initel ()
  "Reloads init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f5>") 'reload-initel)

;; Set neotree custom navigation shortcuts
(use-package neotree
  :bind (("C-å" . neotree-previous-line)
         ("C-ä" . neotree-next-line)
         ("C-S-n" . neotree-toggle)))

;; Set up swiper, more interactive version of isearch
(use-package swiper
  :bind (("C-s" . swiper)))

;; Set up counsel for completion
(use-package counsel
  :bind (("C-x C-f" . counsel-find-file)
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 :map ivy-minibuffer-map ("M-y" . ivy-next-line)))

;; Emacs key bindings for evil-nerd-commenter
(use-package evil-nerd-commenter
  :bind (("M-;" . evilnc-comment-or-uncomment-lines)))

;; Set up vimish fold
(use-package vimish-fold
  :bind (("C-<dead-acute>" . vimish-fold-toggle)))
(require 'vimish-fold)
(vimish-fold-global-mode 1)
(defvar vimish-fold-persist-on-saving t)

;; Set up expand-region
(use-package expand-region
  :bind (("M-h" . er/expand-region)))

;; Set up avy
(use-package avy
  :bind (("C-c k" . avy-goto-char)
         ("C-c j" . avy-goto-word-1)
         ("C-c l" . avy-goto-line)))

;; Set ace-window keys
(use-package ace-window
  :bind (("C-x o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r ?t)))

;; Set up eyebrowse for multiple switchable frames
(use-package eyebrowse
  :init (setq eyebrowse-keymap-prefix (kbd "C-x w"))
  :config (eyebrowse-mode))

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
  :bind (("M-å" . spotify-playpause)
         ("M-ä" . spotify-next)
         ("M-ö" . spotify-previous))
  :config (spotify-enable-song-notifications))

;; Reversible C-x 1 (for temporary fullscreen of a buffer)
(use-package zygospore
  :config (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

;; Set up multiple-cursors
(use-package multiple-cursors
  :bind (("C-<" . mc/mark-next-like-this)
         ("C-M-<" . mc/skip-to-next-like-this)
         ("C-c r" . mc/mark-all-in-region))
  :config (define-key mc/keymap (kbd "<return>") nil))
(use-package mc-extras
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

;; Set up ivy
(use-package ivy
  :config (progn
	    (setq ivy-use-virtual-buffers t)
	    (global-set-key (kbd "C-c c") #'ivy-resume))
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c v" . ivy-push-view)
	 ("C-c V" . ivy-pop-view)))

;; Set up Magit
(use-package magit)

;; Set up zlc, zsh like autocompletion
(use-package zlc)

;; Set up highlight-parentheses
(use-package highlight-parentheses
  :config (global-highlight-parentheses-mode))

;; Set up hightlight shortcut for whitespaces
(global-set-key (kbd "C-c ^") 'whitespace-mode)

;; Set up better-shell
;; TODO: check if one could use remote's default shell when opening remote shell
(use-package better-shell
  :bind (("C-'" . better-shell-shell)
         ("C-*" . better-shell-remote-open)))

;; Set up elfeed
(use-package elfeed)

;; Set up projectile and counsel-projectile
(use-package projectile
  :config (progn
            (projectile-mode +1)
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
            (defvar projectile-completion-system 'ivy)))
(use-package counsel-projectile
  :config (counsel-projectile-mode +1))
;; Fixes .projectile fails to ignore folder -bug
(setq projectile-indexing-method 'native)

;; Set up atomic-chrome
(use-package atomic-chrome
  :config (atomic-chrome-start-server))

;; Set up scp
(use-package scp)

;; Set up vagrant
(use-package vagrant)

;; Set up Haskell mode
(use-package haskell-mode)

;; Set up company for autocompletion
(use-package company
  :config (progn
            (global-company-mode)
            (setq company-idle-delay 0)
            (setq company-minimum-prefix-length 3)))

;; Set up jade mode
(use-package jade-mode)

;; Set up org-tree-slide
(use-package org-tree-slide)

;; Set up beacon
(defun myblink ()
  "Makes beacon blink at the beginning of the line"
  (interactive)
  (set-mark-command nil)       ;; Set mark
  (deactivate-mark)            ;; Deactivate mark
  (move-beginning-of-line nil) ;; Go to beginning of line
  (beacon-blink)               ;; Blink at the beginning of line
  (set-mark-command 1))        ;; Go to previous mark
(use-package beacon
  :bind ("M-<return>" . #'myblink))

;; Set up pyenv
(use-package pyenv-mode
  :hook (python-mode . pyenv-mode))

;; Set up virtualenvwrapper
(use-package virtualenvwrapper
  :config (progn
	    (venv-initialize-interactive-shells)
	    (venv-initialize-eshell)))

;; Set CSS indentation to 2
(setq css-indent-offset 2)

(use-package org-mode
  :bind (("M-p" . org-toggle-latex-fragment)))

;; Set C-r to replace-string
(global-set-key (kbd "C-r") #'replace-string)

;; Set up company-tern for JavaScript autocompletion
(use-package company-tern
  :config (add-to-list 'company-backends 'company-tern)
  :hook ((rjsx-mode . tern-mode)))

;; Set up company-erlang for Erlang autocompletion
(use-package company-erlang)
(use-package flycheck-rebar3)

;; Set up jedi for Python autocompletion
(use-package jedi
  :config (progn
            (jedi:install-server)
            (setq jedi:complete-on-dot t))
  :hook ((python-mode . jedi:setup)))

;;; Stupid flychecker wants this to be present at the end of the file: (provide 'init)
;;; init.el ends here
