;;; init.el --- Emacs Configuration

;; Copyright (C) 2016-2017 Cezary Stankiewicz

;; Maintainer: Cezary Stankiewicz (concat "c.stankiewicz" "@" "wlv.ac.uk")
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Note: Use the ~pp-macroexpand-last-sexp~ to see macro expansion of the ~use-package~.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add package archives
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")) t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package use-package
  :config
  (setq use-package-always-ensure nil
        use-package-verbose nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable and press C-g to debug
;; (setq debug-on-quit t)

;; Search init file for bugs
(use-package bug-hunter
  :ensure t)

;;; Custom load paths
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
(add-to-list 'load-path (concat user-emacs-directory "fork/"))
(add-to-list 'load-path (concat user-emacs-directory "fork/org-9.0.4/lisp"))
(add-to-list 'load-path (concat user-emacs-directory "fork/org-9.0.4/contrib/lisp") t)
(add-to-list 'load-path (concat user-emacs-directory "fork/org-9.0.4/contrib/lisp"))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/cs-misterioso-theme.el/"))

;; Auto backup files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 16   ; How many of the newest versions to keep
      kept-old-versions 16   ; How many of the old versions to keep
      )

;; Uncoditionally kill current buffer 
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Custom data and information
(use-package cs-local
  :ensure nil)
  ;; :defer t
  ;; ;; :commands (blog-directory blog-href blog-title blog-disqus-shortname)
  ;; :load-path "local/")


;; (defvar blog-directory "~/_local/edu/6/6cs007/work/blog/")
;; (defvar blog-href "http://mi-linux.wlv.ac.uk/~1427790/final-project/docs/homepage/")
;; (defvar blog-title "Final Year Project Blog")
;; (defvar blog-disqus-shortname "cezs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package face-remap+
  :disabled t
  :ensure t)

(use-package facemenu+
  :disabled t
  :ensure t)

(use-package faces+
  :disabled t
  :ensure t)

;; Load theme
(defadvice load-theme (before theme-dont-propagate activate)
  "Unload current theme before enabling the new one."
  (mapc #'disable-theme custom-enabled-themes))

;; (load-theme 'wombat t)
(load-theme 'cs-misterioso t)

;; Show/Hide Menu Bar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1)
  (bind-key "<C-f1>" #'menu-bar-open)
  (global-unset-key (kbd "<C-f10>")))

;; Show/Hide Tool Bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Show/Hide Scroll Bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Enable/Disable the site default settings
(when (boundp inhibit-default-init)
  (setq inhibit-default-init t))

;; Enable/Disable the start-up screen
(when (boundp 'inhibit-startup-screen)
  (setq inhibit-startup-screen t))

;; Distinct parentheses
(when (fboundp 'show-paren-mode)
  (show-paren-mode 1))

;; Show/Hide Tool Bar
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode 0))

;; Line numbers in display margin
(use-package nlinum
  :disabled t
  :ensure t)

;; Show line-number and column-number in a mode line
(when (and (fboundp 'line-number-mode)
           (fboundp 'column-number-mode))
  (line-number-mode 1)
  (column-number-mode 1))

;; Allow downcase and upcase region operations
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Indentation
(when (and (boundp 'tab-width)
           (boundp 'indent-tabs-mode))
  (setq tab-width 4)
  (setq-default indent-tabs-mode nil))

;; Workgroups
(use-package workgroups2
  :ensure t
  :bind (("C-c z"   . wg-prefix-key))
  :config
  (setq wg-emacs-exit-save-behavior nil)
  (setq wg-workgroups-mode-exit-save-behavior nil)
  (setq wg-session-file "~/.emacs.d/workgroups"))

;; Popup window manager
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))
;; (setq display-buffer-alist 'popwin:display-buffer)
;; (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
;; (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config))

;; Change layout of windows
(use-package transpose-frame
  :ensure t
  :bind (("M-s c"   . transpose-frame)
         ("M-s d"   . flip-frame)
         ("M-s e"   . flop-frame)
         ("M-s v" . rotate-frame)
         ("M-s f" . rotate-frame-clockwise)
         ("M-s r" . rotate-frame-anticlockwise)))

;; Window manager with IDE like layouts
(use-package e2wm
  :disabled t)

(use-package e2wm-direx
  :disabled t)

;; Different mode lines
(use-package powerline
  :disabled t
  :config
  (powerline-default-theme))

;; Display batery status
(use-package fancy-battery
  :disabled t
  :pin melpa-stable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Behaviour ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Short yes/no
(when (and (fboundp 'yes-or-no-p)
           (fboundp 'y-or-n-p))
  (fset 'yes-or-no-p #'y-or-n-p))

;; Built-in: Copy/Paste between Emacs and other windows
(use-package term
  :ensure t
  :config
  (setq x-select-enable-clipboard t)) ; Enable Clipboard

;; Built-in: Save current session
(use-package desktop
  :disabled t
  :config
  (desktop-save-mode -1)) ; Always save last session?

;; Deprecated: Using helm now.
;; Built-in: Save commands history
(use-package savehist
  :init (savehist-mode 1)
  :config (setq savehist-save-minibuffer-history 1))

;; Deprecated: Using helm now.
;; Built-in: Create a menu item File -> Open recent
(use-package recentf 
  :disabled t
  :init
  :config
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 6)
  (setq recentf-max-saved-items 6)
  (recentf-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package o-blog
  :disabled t
  :ensure t)

;; Custom: All custom functions and variables that are to be used in 'any' type of buffer
(use-package cs-global
  :ensure nil)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(use-package smex
  :disabled t
  :ensure t
  :config
  (smex-initialize)
  (global-set-key [(meta x)] 'smex)
  (global-set-key [(shift meta x)] 'smex-major-mode-commands))

(use-package ido
  :disabled t
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t))

(use-package ido-ubiquitous
  :disabled t
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package flx-ido
  :disabled t
  :ensure t)

(use-package ido-vertical-mode
  :disabled t
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package helm
  :ensure t
  :after async
  :bind (("M-x" . helm-M-x)
         ("C-x C-b" . helm-mini)))

;; built-in: Edit file system directories
(use-package dired
  :ensure nil
  :config
  ;; Press 'a' to access directory or file, without opening new buffer.
  )

;; Tree like expansion in Dired
(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))

;; Sort files in dired
(use-package dired-sort
  :ensure t
  :after dired)
(use-package dired-sort-menu
  :ensure t
  :after dired)

;; Preview and edit files in dired without leaving
;; open buffer when moving to next file.
(use-package bf-mode
  :ensure t
  :after dired
  ;; :commands (bf-mode)
  ;; :bind (:map bf-mode-map
  ;;             ("<down>" . bf-mode-next)
  ;;             ("<up>" . bf-mode-previous))
  :config
  ;; list up file extensions which should be excepted
  (setq bf-mode-except-exts
        (append '("\\.dump$" "\\.data$" "\\.mp3$" "\\.lnk$")
                bf-mode-except-exts))

  ;; list up file extensions which should be forced browsing
  (setq bf-mode-force-browse-exts
        (append '("\\.txt$" "\\.and.more...")
                bf-mode-force-browse-exts))

  ;; browsable file size maximum
  (setq bf-mode-browsing-size 100) ;; 100 killo bytes

  ;; browsing htmls with w3m (needs emacs-w3m.el and w3m)
  (setq bf-mode-html-with-w3m t)

  ;; browsing archive file (contents listing) verbosely
  (setq bf-mode-archive-list-verbose t)

  ;; browing directory (file listing) verbosely
  (setq bf-mode-directory-list-verbose t)

  ;; start bf-mode immediately after starting dired
  (setq bf-mode-enable-at-starting-dired t)

  ;; quitting dired directly from bf-mode
  (setq bf-mode-directly-quit t))

;; Preview files in dired with Peep-dired minor mode
(use-package peep-dired
  :ensure t
  :init
  (add-hook
   'pre-command-hook
   (lambda ()
     (when (eq major-mode 'peep-dired)
       #'peep-dired-kill-buffers-without-window
       ))))
  ;; :config
  ;; (setq peep-dired-cleanup-eagerly t))

;; Preview files in dired with Ranger mode
(use-package ranger
  :ensure t
  :defer t)

;; Tree directory/code explorer
(use-package direx
  :disabled t
  :after popwin
  :bind ("<C-f2>" . direx:display-item)
  :config
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
      popwin:special-display-config))

;; Tree directory/code explorer
(use-package sr-speedbar
  :ensure t
  :after helm
  :bind (("C-c b t" . sr-speedbar-toggle))
  :config
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-right-side nil))

(use-package all-the-icons-dired
  :ensure t)

(use-package helm-dired-history
  :ensure t)

(use-package helm-dired-recent-dirs
  :ensure t)

(use-package look-mode
  :ensure t)

(use-package look-dired
  :disabled t
  :ensure t)

;; Manage buffers. Replacement for buffer-menu.
(use-package ibuffer
  :ensure t
  :config
  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode))

;; Navigate through multiple terminals
(use-package multi-term
  :ensure t
  :after helm
  :config
  (setq multi-term-program "/bin/zsh"))

;; View and edit remote files
;; Example: C-x C-f /ssh:<name>@<host>:<dir>
(use-package tramp
  :ensure t
  :defer t
  :config
  (setq tramp-default-method "ssh")
  ;; resolve prolonged initialization
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))

;; Built-in
;; Example use:
;;   1. Start multiple emacs daemons
;;     emacs --daemon=<server's name>
;;   2. Start emacsclient
;;     emacsclient --server-file=<server's name> <file's name>
;; Otherwise:
;;   1. Start emacs server automatically if not already running
;;   2. Use emacsclient to edit in current frame
;;      or emacsclient --create-frame or -c and <file's name> to use new frame
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))

;; Built-in: Auto-revert buffers of changed files
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode)
  (setq auto-revert-verbose nil
        ;; Revert Dired buffers
        global-auto-revert-non-file-buffers t))

;; Open files in external programs
(use-package launch
  :ensure t)

;; Edit files as root
(use-package sudo-edit
  :ensure t)

;; Deprecated: Using helm now.
;; Locate files
;; Example use: M-x locate RET <filename>
(use-package locate
  :disabled t)


(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root "~/")) ; change to working directory

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: Using helm-gtags instead.
;; Tags
(use-package ggtags
  :disabled t
  :ensure t
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              ("M-," . pop-tag-mark))
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index))

;; Tags
(use-package rtags
  :ensure t)

;; Tags
(use-package ctags
  :disabled t
  :ensure t)

;; Syntax checking
(use-package flycheck
  :ensure t
  :config
  ;; Enable minor mode in all buffers by default
  (global-flycheck-mode 1))

;; Spelling correction
(use-package flyspell
  :ensure t
  :after ispell
  :init
  (add-hook 'LaTeX-mode-hook #'turn-on-flyspell))

(use-package flyspell-popup
  :ensure t
  :after flyspell
  ;; Use the following binding or default "<M-tab>".
  :bind (:map flyspell-mode-map
              ("<C-tab>" . flyspell-popup-correct))
  ;; ;; Or pop-up automatically.
  ;; :init
  ;; (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)
  ;; :config
  ;; (setq flyspell-popup-correct-delay .5)
  )

(use-package ispell
  :ensure t
  :config
  ;; Use either aspell or hunspell
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; (setq ispell-list-command "--list")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell")
    (setq ispell-extra-args '("-d en_GB")))
   (t (setq ispell-program-name nil))))

;; Snippets
(use-package yasnippet
  :ensure t
  :defer t
  ;; :commands (yas-minor-mode)
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  ;; Use parent major mode's indentation instead
  (setq yas-also-auto-indent-first-line t)
  ;; Reload immediately
  (yas-reload-all))

;; Auto completion backend
(use-package company
  :ensure t
  :config
  ;; (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-c-headers))

;; A backend that emulates ac-source-dictionary
(use-package company-dict
  :ensure t)

;; Company version of ac-html, complete for web,html,emmet,jade,slim modes
(use-package company-web
  :ensure t)

;; Anaconda backend for company-mode
(use-package company-anaconda
  :ensure t)

;; company-mode for Arduino
(use-package company-arduino
  :ensure t)

;; Company-mode auto-completion for AUCTeX
(use-package company-auctex
  :ensure t)

;; Company completion for bibtex keys
(use-package company-bibtex
  :disabled t
  :ensure t)

;; company-mode cabal backend
(use-package company-cabal
  :ensure t)

;; Erlang/distel completion backend for company-mode
(use-package company-distel
  :disabled t
  :ensure t)

;; company-mode backend for eclim
(use-package company-emacs-eclim
  :ensure t)

;; company-mode ghc-mod backend
(use-package company-ghc
  :ensure t)

;; company backend which uses the current ghci process.
(use-package company-ghci
  :ensure t)

;; Company mode backend for C/C++ header files with Irony
(use-package company-irony-c-headers
  :ensure t)

;; Completion backends for unicode math symbols and latex tags
(use-package company-math
  :ensure t)

;; Auto completion backend. Note: mode conflicts with company-mode
(use-package auto-complete
  :ensure t
  :config
  (add-hook 'after-init-hook 'auto-complete-mode)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict"))
  ;; ;; Enable minor mode in all buffers by default
  ;; (ac-config-default))

;; auto-complete source with completion-at-point
(use-package ac-capf
  :ensure t)

;; Auto Completion source by libclang for GNU Emacs
(use-package ac-clang
  :ensure t)

;; Haskell auto-complete source which uses the current haskell process
(use-package ac-haskell-process
  :ensure t)

;; Auto Complete with Helm
(use-package ac-helm
  :ensure t)

;; auto complete source for html tags and attributes
(use-package ac-html
  :ensure t)

;; Auto-complete source for Js2-mode, with navigation
(use-package ac-js2
  :disabled t
  :ensure t)

;; Auto-complete sources for input of mathematical symbols and latex tags
(use-package ac-math
  :ensure t)

;; auto-completion source for php
(use-package ac-php
  :ensure t)

;; gen tags for php
(use-package ac-php-core
  :ensure t)

;; Auto complete etags?
(use-package ac-etags
  :ensure t)

(use-package helm-gtags
  :ensure t
  :bind (:map helm-gtags-mode-map
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-j" . helm-gtags-select)
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("M-t" . helm-gtags-find-tag)
              ("M-r" . helm-gtags-find-rtag)
              ("M-s" . helm-gtags-find-symbol)
              ("M-g M-p" . helm-gtags-parse-file)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history))
  :init
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t
   )
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode))

(use-package irony
  :disabled t
  :defer t)

(use-package awk-it
  :disabled t
  :defer t)

(use-package re-builder+
  :ensure nil
  :defer t
  :load-path "fork/")

(use-package cedet
  :ensure t
  :config
  (global-ede-mode))

;; Running word count with goals (minor mode)
(use-package wc-mode
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cmake-mode
  :ensure t)

(use-package cmake-project
  :ensure t)

(use-package cmake-ide
  :ensure t)

(use-package cmake-font-lock
  :ensure t)

(use-package cuda-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package flymake-yaml
  :ensure t)

(use-package glsl-mode
  :ensure t)

;; Summary: Emacs Major mode for Markdown-formatted text files
(use-package markdown-mode
  :ensure t)

;; Summary: extra functions for markdown-mode
(use-package markdown-mode+
  :ensure t)

;; Summary: On the fly markdown preview
;; Usage: M-x flymd-flyit
(use-package flymd
  :ensure t)

;; Summary: Render markdown using the Github api
;; Usage: use the functions `gh-md-render-region' and `gh-md-render-buffer'
(use-package gh-md
  :ensure t)

;; Summary: markdown realtime preview minor mode.
;; Usage: M-x `markdown-preview-mode'
(use-package markdown-preview-mode
  :ensure t)

;; Summary: Html As Markdown. Transparently edit an html file using markdown
;; Usage: M-x `ham-mode'
(use-package ham-mode
  :ensure t)

;; Syntax highlighting for inlined languages
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.css\\'" . web-mode)))

;; JavaScript: Major mode
(use-package js2-mode
  :disabled t
  :ensure t)

;; JavaScript: Preview
;; 1. Put this directory in your `load-path'
;; 2. Load skewer-mode.el
;; 3. M-x `run-skewer' to attach a browser to Emacs
;; 4. From a `js2-mode' buffer with `skewer-mode' minor mode enabled,
;;    send forms to the browser to evaluate
(use-package skewer-mode
  :ensure t
  :config
  ;; (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package flymake-jslint
  :disabled t
  :after flymake
  :config
  (add-hook 'js-mode-hook 'flymake-jslint-load))

;; Yasnippets for Java
(use-package java-snippets
  :ensure t
  :defer t)

;; Java Development Environment for Emacs
(use-package jdee
  :ensure t
  :defer t)

;; A better Java mode for Emacs
;; http://www.github.com/m0smith/malabar-mode
(use-package malabar-mode
  :disabled t
  :ensure t)

;; A better java development mode
;; https://github.com/mopemope/meghanada-emacs
(use-package meghanada
  :ensure t)

;; Code for dealing with Java imports
(use-package java-imports
  :ensure t)

;; Enhanced tags functionality for Java development
;; 
;; (setq tags-table-list '("/usr/java/jdk1.8.0_91/"))
;; (setq tags-revert-without-query 't)
;; jtags-javadoc-root-alist
;; jtags-display-menu-flag
(use-package jtags
  :ensure t
  :config
  (add-hook 'java-mode-hook 'jtags-mode))

;; Scala
(use-package ensime
  :ensure t)

(use-package scala-mode
  :ensure t)

;; auto-complete source for C headers
(use-package ac-c-headers
  :ensure t
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

;; Syntax coloring for opencl kernels
(use-package opencl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode)))

;; C/C++ Arguments suggestions
(use-package function-args
  :ensure t
  ;; :bind (:map function-args-mode-map ())
  :config
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (set-default 'semantic-case-fold t) ; case-insensitive search
  (fa-config-default))

;; C/C++ Refactoring
(use-package srefactor
  :ensure t
  :config
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point))

;; Prolog
(use-package prolog
  :ensure t
  :mode
  ;; Use .pro file extension for Prolog files.
  ("\\.pro\\'" . prolog-mode)
  :config
  ;; Use SWI-Prolog interpreter instead of default GNU.
  (setq prolog-system 'swi))

;; Charts
(use-package ox-taskjuggler
  :ensure nil
  :load-path "fork/org-9.0.4/contrib/lisp/")

;; Plotting
(use-package gnuplot-mode
  :ensure t)

;; TeX
(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t)
    (setq-default TeX-master nil)))

;; Debugger entered--Lisp error: (void-function defstruct)
(use-package latex-math-preview
  :disabled t
  :ensure t
  :commands LaTeX-preview-setup
  :init
  (progn
    (setq-default preview-scale 1.4
                  preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))))

(use-package reftex
  :ensure t
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t))

  ;; (setq reftex-default-bibliography 
  ;;   '("/Bibliography/main.bib"))

  ;; (setq reftex-bibpath-environment-variables
  ;;   '("/Bibliography/"))

  ;; (setq reftex-bibpath-environment-variables
  ;; '("/Users/clarkdonley/Library/texmf/bibtex/bib"))
  ;; (setq reftex-default-bibliography '("/Bibliography/main.bib"))
  ;; (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

  ;; (setq reftex-default-bibliography
  ;;       (quote
  ;;        ("user.bib" "local.bib" "main.bib")))

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
  (autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
  (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

  ;; Make RefTeX faster
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  (setq reftex-plug-into-AUCTeX t)

  (defun org-mode-reftex-setup ()
    "Make RefTeX work with Org-Mode.
Use 'C-c (' instead of 'C-c [' because the latter is already defined in orgmode to the add-to-agenda command."
    (load-library "reftex") 
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

  (add-hook 'org-mode-hook 'org-mode-reftex-setup)

  ;; RefTeX formats for biblatex (not natbib)
  (setq reftex-cite-format
        '(
          (?\C-m . "\\cite[]{%l}")
          (?t . "\\textcite{%l}")
          (?a . "\\autocite[]{%l}")
          (?p . "\\parencite{%l}")
          (?f . "\\footcite[][]{%l}")
          (?F . "\\fullcite[]{%l}")
          (?x . "[]{%l}")
          (?X . "{%l}")
          ))

  (setq font-latex-match-reference-keywords
        '(("cite" "[{")
          ("cites" "[{}]")
          ("autocite" "[{")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{") 
          ("citetitle" "[{") 
          ("citetitles" "[{") 
          ("headlessfullcite" "[{")))

  (setq reftex-cite-prompt-optional-args nil)
  (setq reftex-cite-cleanup-optional-args t))

(use-package bibtex
  :ensure t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

(use-package cdlatex
  :ensure t)

;; Org mode: Prolog support
(use-package ob-prolog
  :ensure t)

;; Org mode: Standard ML support
(use-package ob-sml
  :ensure t)

;; Convert to man page
(use-package ox-man
  :ensure nil
  :defer t
  :load-path "fork/org-9.0.4/contrib/lisp/")

(use-package ox-bibtex
  :ensure nil
  :defer t
  :load-path "fork/org-9.0.4/contrib/lisp/")

(use-package ox-rss
  :ensure nil
  :load-path "fork/org-9.0.4/contrib/lisp/")

(use-package ox-html
  :ensure nil
  :load-path "fork/org-9.0.4/lisp/")

(use-package ox-publish
  :ensure nil
  :load-path "fork/org-9.0.4/lisp/")

(use-package ox-extra
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package ox-latex
  :config
  (defun my-latex-export-example-blocks (text backend info)
    "Export example blocks as listings env."
    (when (org-export-derived-backend-p backend 'latex)
      (with-temp-buffer
        (insert text)
        ;; replace verbatim env by listings
        (goto-char (point-min))
        (replace-string "\\begin{verbatim}" "\\begin{lstlisting}")
        (replace-string "\\end{verbatim}" "\\end{lstlisting}")
        (buffer-substring-no-properties (point-min) (point-max)))))
  (add-to-list 'org-export-filter-example-block-functions
               'my-latex-export-example-blocks)
  (setq org-latex-caption-above nil)
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "makeglossaries %b"
          "biber %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))
  (add-to-list 'org-latex-classes
               '("org-report"
                 "\\documentclass{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Org mode
(use-package org
  :ensure t
  :after cs-local
  :bind (("M-C" . jump-to-org-agenda)
         ("M-m" . org-smart-capture)
         ("C-c c" . org-capture)
         ("M-M" . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link)
         ("C-. n" . org-velocity-read))
  :config
  (defadvice org-rss-headline
      (around my-rss-headline (headline contents info) activate)
    "only use org-rss-headline for top level headlines"
    (if (< (org-export-get-relative-level headline info) 2)
        ad-do-it
      (setq ad-return-value (org-html-headline headline contents info))))

  (setq org-publish-project-alist
        `(("blog"
           :components ("blog-content" "blog-static" "blog-rss"))
          ("blog-content"
           :base-directory ,cs-base-directory
           :html-extension "html"
           :base-extension "org"
           :publishing-directory ,cs-publishing-directory
           :publishing-function (org-html-publish-to-html)
           :auto-sitemap t
           :sitemap-filename "archive.org"
           :sitemap-title "Archive"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           :makeindex t
           :recursive t
           :section-numbers nil
           :with-toc nil
           :with-latex t
           :html-head-include-default-style nil
           :html-head-include-scripts nil
           :html-head-extra
           ,(concat 
           "<link rel=\"alternate\" type=\"appliation/rss+xml\"
                href=\"" cs-html-link-home "/rss.xml\"
                title=\"RSS feed for" cs-html-link-home "\">
          <link href='http://fonts.googleapis.com/css?family=Roboto&subset=latin' rel='stylesheet' type='text/css'>
          <link href='http://fonts.googleapis.com/css?family=Ubuntu+Mono' rel='stylesheet' type='text/css'>
          <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
          <title>" cs-title "</title>
          <meta http-equiv=\"content-type\" content=\"application/xhtml+xml; charset=UTF-8\" />
          <meta name=\"viewport\" content=\"initial-scale=1,width=device-width,minimum-scale=1\">")
           :html-preamble
           ,(concat 
           "<div class=\"header\">
              <a href=\"" cs-html-link-home "\">Return to Homepage</a>
              </div>
          </div>")
           :html-postamble
           (lambda (info)
             "Do not show disqus for Archive and Recent Posts"
             (cond ((string= (car (plist-get info :title)) "Archive") "")
                   ((string= (car (plist-get info :title)) "Recent Posts")
                    "<div id=\"archive\"><a href=\"archive.html\">Other posts</a></div>")
                   (t
                    (concat 
                    "<div id=\"archive\"><a href=\"archive.html\">Other posts</a></div>
              <div id=\"disqus_thread\"></div>
              <script type=\"text/javascript\">
              /* * * CONFIGURATION VARIABLES: EDIT BEFORE PASTING INTO YOUR WEBPAGE * * */
              var disqus_shortname = '" cs-blog-disqus-shortname "';
              /* * * DON'T EDIT BELOW THIS LINE * * */
              (function() {
                var dsq = document.createElement('script');
                dsq.type = 'text/javascript';
                dsq.async = true;
                dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
                (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
                  })();
              </script>
              <noscript>Please enable JavaScript to view the
                  <a href=\"http://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>
              <a href=\"http://disqus.com\" class=\"dsq-brlink\">comments powered by <span class=\"logo-disqus\">Disqus</span></a>"))))
           :exclude "rss.org\\|archive.org\\|theindex.org")
          ("blog-rss"
           :base-directory ,cs-base-directory
           :base-extension "org"
           :publishing-directory ,cs-publishing-directory
           :publishing-function (org-rss-publish-to-rss)
           :html-link-home ,cs-html-link-home
           :html-link-use-abs-url t
           :exclude ".*"
           :include ("rss.org")
           :with-toc nil
           :section-numbers nil
           :title ,cs-title)
          ("blog-static"
           :base-directory ,(concat cs-publishing-directory "/static")
           :base-extension "png\\|jpg\\|css"
           :publishing-directory ,(concat cs-publishing-directory "/static")
           :recursive t
           :publishing-function org-publish-attachment)))
  ;; (setq org-support-shift-select (quote always))
  (setq flyspell-issue-message-flag nil)
  (defun flyspell-ignore-tex ()
    "Ignore tex commands."
    (interactive)
    (set (make-variable-buffer-local 'ispell-parser) 'tex))
  (add-hook 'org-mode-hook 'flyspell-ignore-tex)

  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.3))
  
  (org-babel-do-load-languages
   'org-babel-load-languages '(
                               (C . t)
                               ;; (shell . t)
                               ;; (coq . t)
                               ;; (ipython . t)
                               ;; (julia . t)
                               ;; (gnuplot . t)
                               (haskell . t)
                               ;; (ocaml . t)
                               ;; (perl . t)
                               ;; (prolog . t)
                               (python . t)
                               ;; (R . t)
                               ;; (sml . t)
                               (emacs-lisp . t)))
  (add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
  :init
  ;; (setq org-export-latex-classes
  ;;            '("article"
  ;;              "\\documentclass{article}"
  ;;              ("\\section{%s}" . "\\section*{%s}")
  ;;              ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;              ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-deadline-warning-days 365)
  ;; (setq org-agenda-todo-list-sublevels t)
  (setq org-agenda-todo-ignore-with-date t)
  ;; (setq org-agenda-todo-ignore-deadlines "all")

  (setq org-agenda-repeating-timestamp-show-all nil)
  ;; (setq org-agenda-skip-deadline-prewarning-if-scheduled 40)
  ;; (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;; (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-buffer)
  (add-hook 'org-mode-hook (lambda () (setq ispell-parser 'tex)))
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (setq org-export-backends '(ascii beamer html icalendar latex man md taskjuggler))
  (setq org-modules '(org-bbdb org-bibtex org-ctags org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m))
  (setq org-support-shift-select (quote always))
  (setq org-latex-to-pdf-process
        '("xelatex -interaction nonstopmode %f"
          "xelatex -interaction nonstopmode %f")) ;; for multiple passes
  (setq org-src-fontify-natively t)
  (setq org-replace-disputed-keys t)
  (setq org-use-tag-inheritance nil)
  (setq org-log-done 'time)
  (setq org-startup-truncated nil)
  (setq org-startup-indented t)
  (setq org-support-shift-select t)
  (setq org-startup-folded nil)
  (setq org-default-notes-file nil)
  (setq org-default-notes-file (concat "../Documents/doc/org/" "Notes.org"))
  (setq org-agenda-files (quote ("~/_local/note/org/Nizer.org")))
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.x?html?\\'" . "firefox %s"))))

(use-package org-special-blocks
  :disabled t
  :ensure t)

;; Custom additions to `org-mode'
(use-package cs-org
  :ensure nil
  :defer t
  :load-path "elisp/cs-org"
  :after org
  :config
  ;; Modify Org menu
  (easy-menu-add-item org-org-menu nil ["--" nil])
  (easy-menu-add-item org-org-menu nil '("Elisp"
                                         ["Tangle and open"
                                          cs-org-babel-tangle-and-open]
                                         ["Tangle and open"
                                          cs-org-babel-tangle-and-open]
                                         ["Delete generated files"
                                          cs-org-delete-generated-files]
                                         ["Delete all non org files"
                                          cs-org-delete-all-except-org-files]
                                         ["Create and install emacs package"
                                          cs-org-file-to-emacs-package])))

;; Simple awk usage and custom sample of minor mode
(use-package cs-awker-mm
  :ensure nil
  :defer t
  :commands cs-awker-mm
  :load-path "elisp/cs-awker-mm")

;; Using various GNU utils, also custom sample of minor mode
(use-package cs-nixer-mm
  :ensure nil
  :defer t
  :commands cs-nixer-mm
  :load-path "elisp/cs-nixer-mm")

(use-package o-blog
  :ensure nil
  :defer t)
;; ;; :commands org-publish-blog
;; :load-path "elisp/o-blog")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs scripting tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; String manipulation library
(use-package s
  :ensure t)

;; API for working with directiories
(use-package f
  :ensure t)

(use-package async
  :ensure t)

;; (use-package guile-scheme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn on, during startup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (dired "./")
;; (direx:find-directory "/")
;; (list-buffers) ;(ibuffer)
;; (workgroups-mode 1)
;; (sr-speedbar-open)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overwrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice replace-string (around turn-off-case-fold-search)
  "Case-insensitive replace."
  (let ((case-fold-search nil))
    ad-do-it))

(ad-activate 'replace-string)

(setq c-basic-offset 4)
(c-set-offset 'substatement-open 0)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(set-face-attribute 'default nil :height 100)

;;; init.el ends here
