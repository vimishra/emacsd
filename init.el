;; This is Vikas Mishra's custom emacs init file. I haven't yet figured out how I want to load it.
;; The general sequence that this will follow is
;; 1. Enble the package manager and configure the package manager to get started as needed.
;; 2. Enable benchmarking tool.
;; 3. Setup sane defaults for emacs
;; 4. Enable oher packages

;; ===============================
;; Enble package management
;; ===============================
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

;; Bootstrap 'use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))
(setq use-package-always-ensure t)

;; ===============================
;; Enable benchmarking
;; ===============================
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
          (lambda () (message "loaded in %s" (emacs-init-time))))

;; Don't litter my .emacs.d
(use-package no-littering               ; Keep .emacs.d clean
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; ===============================
;; Enable sane defaults
;; ===============================
(setq gc-cons-threshold (* 100 1024 1024))

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

;; Custom Elisp scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; ===================================
;; Emacs configuration
;; ===================================
(setq-default
 ;; If the .el file is newer than the .elc file, use the .el file
 load-prefer-newer t
 ;; Don't show the startup message
 inhibit-startup-message t
 ;; Set the fill column to 135
 fill-column 135
 ;; Set a more intuitive title for emacs
 frame-title-format '("" "[%b] - Emacs " emacs-version)
 ;; Do not create lockfile
 create-lockfile nil
 ;; Don't use hard tabs
 indent-tabs-mode nil
 custom-file "~/.emacs.d/custom-file.el"

 ;; Do I want autosave - for the time being no.
 auto-save-default nil
 ;; Allow commands to be run on minibuffer
 enable-recursive-minibuffers t)
 ;; Emacs can automatically create backup files. This tells Emacs to put all backups in
 ;; ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; UI Hacks
;; Don't show me the toolbar
(tool-bar-mode -1)
;; Dont use dialog boxes -prompt in minibuffer.
(setq use-dialog-box nil)
;; Enable global line highlight
(global-hl-line-mode t)

;; X  clipboard interaction
(setq-default
 ;; Makes killing/yanking interact with the clipboard.
 x-select-enable-clipboard t
 ;; To understand why this is done, read `X11 Copy & Paste to/from Emacs' section here:
 ;; https://www.emacswiki.org/emacs/CopyAndPaste.
 x-select-enable-primary t
 ;; Save clipboard strings into kill ring before replacing them. When
 ;; one selects something in another program to paste it into Emacs, but
 ;; kills something in Emacs before actually pasting it, this selection
 ;; is gone unless this variable is non-nil.
 save-interprogram-paste-before-kill t
 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html.
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; Other annoyances fixed
;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
;; Display column number in mode line.
(column-number-mode t)
;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)
;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;; Make the command key behave as 'meta'
(when (eq system-type 'darwin)
  ;; Use my option key as meta and command key as hyper
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super))

;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Disable commands that are not useful
(unbind-key "C-x C-d") ;; list-directory
(unbind-key "C-z") ;; suspend-frame
(unbind-key "M-o") ;; facemenu-mode
(setq require-final-newline t)
(setq enable-local-variables :all)

;; Enable personal key map
;; Define vm-map
(define-prefix-command 'vm-map)
(global-set-key (kbd "C-1") 'vm-map)

;; Other Packages
;; Load Crux Mode
;; Details - https://github.com/bbatsov/crux
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("s-," . crux-find-user-init-file)
         ("C-c o" . crux-open-with)
         ("C-c t" . crux-visit-term-buffer)
         ("s-k" . crux-kill-whole-line)
         ))


;; Programming specific improvements related to parens.
(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Jump to the last change
(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

;; Ivy command completion framework
;; Ivy, Counsel and Swiper
(use-package ivy
  :diminish
  :custom
  (ivy-height 15)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)

  :bind (("C-c C-r" . #'ivy-resume)
         ("C-c s"   . #'swiper-thing-at-point)
         ("C-s"     . #'swiper)))

;; Make ivy stuff in minibuffer look pretty
(use-package ivy-rich
  :custom
  (ivy-virtual-abbreviate 'name)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'name)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev
        ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 20))
            (ivy-rich-switch-buffer-size (:width 7 :align right))
            (ivy-rich-switch-buffer-indicators
             (:width 2 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 8 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))))
  (ivy-rich-mode))

;; Counsel package
(use-package counsel
  :init
  (counsel-mode 1)

  :bind (("C-x C-m" . #'counsel-M-x)
         ("C-c U" . #'counsel-unicode-char)
         ("C-c i" . #'counsel-imenu)
         ("C-x f" . #'counsel-find-file)
         ("C-c y" . #'counsel-yank-pop)
         ("C-c r" . #'counsel-recentf)
         ("C-c v" . #'counsel-switch-buffer-other-window)
         ("C-h h" . #'counsel-command-history)
         ("C-x C-f" . #'counsel-find-file)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :diminish)

(use-package counsel-projectile
  :bind (("C-c f" . #'counsel-projectile)
         ("C-c F" . #'counsel-projectile-switch-project)))
(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package ivy-hydra)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode t))

;; Terminal improvements
(use-package vterm
  :ensure t)
(setq crux-term-buffer-name "vterm")

;; UI Improvements
;; Modeline, Theme and Icons
;; Modeline configuration
;; load color theme
(setq custom-enabled-themes 'sanityinc-tomorrow-night)
(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))
;; Diminish so we don't muddy the modeline
(use-package diminish
  :config (diminish 'eldoc-mode))


(use-package zop-to-char
  :init
  (global-set-key [remap zap-to-char] 'zop-to-char))

;; Flash the line that has the cursor on a context change.
;; Super useful in noticing the cursor in a multi-window environment.
(use-package beacon
  :config
  (beacon-mode 1))


;; ;; Doom Modeline for a better looking modeline
;; ;; More customization options can be determined from https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-lsp t)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  ;; Whether display the buffer encoding.
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t))

;; Fuzzy and Search
(use-package fzf)
(use-package ag
  :ensure t
  :commands (ag ag-regexp ag-project))

(use-package ripgrep)
;; Deadgrep is the best way to search in a repo
(use-package deadgrep
  :bind (("C-c h" . #'deadgrep)))
;; Visual reegular expressions. This is the boss
(use-package visual-regexp
  :bind (("C-c 5" . #'vr/replace)))

;; Programming conveniences
(use-package dumb-jump
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :config
  (global-git-gutter-mode 't))

;; Snippets
(use-package yasnippet
    :config
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
    (yas-global-mode 1))
(use-package yasnippet-snippets)

;; Treemacs
(use-package treemacs
  :config
  (setq treemacs-width 25
        treemacs-follow-mode -1
        treemacs-tag-follow-mode -1
        treemacs-is-never-other-window t
        treemacs-follow-after-init t
        treemacs-icon-open-png   (propertize "⊖ " 'face 'treemacs-directory-face)
        treemacs-icon-closed-png (propertize "⊕ " 'face 'treemacs-directory-face))
  (define-key treemacs-mode-map [mouse-1]
    #'treemacs-single-click-expand-action)
  :bind ( "M-0" . treemacs-select-window)
  )


(use-package company
  :ensure company-box
  :init
  (global-company-mode t)
  (global-set-key (kbd "M-/") 'company-complete)

  ;; (add-hook 'comint-mode-hook 'company-mode)
  :config
    (setq company-tooltip-limit 10)
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0.1)
    (setq company-echo-delay 0.1)
    (setq company-minimum-prefix-length 2)
    (setq company-require-match nil)
    (setq company-selection-wrap-around t)
    (setq company-tooltip-align-annotations t)
    (setq company-show-numbers t)
    ;; (setq company-tooltip-flip-when-above t)
    (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package company-box
  :ensure frame-local
  :hook (company-mode . company-box-mode))

(setq treemacs-autopeek-mode nil)

(defun treemacs-toggle-autopeek ()
  (interactive)
  (if treemacs-autopeek-mode
      (progn
        (setq treemacs-autopeek-mode nil)
        (message "Treemacs autopeek: OFF"))
    (setq treemacs-autopeek-mode t)
    (message "Treemacs autopeek: ON")))

(use-package treemacs-projectile)
(use-package treemacs-magit)

;; LSP
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "s-l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; Use pyright for python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

;; optionally
(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-eldoc-enable-hover t ; Disable eldoc displays in minibuffer
        lsp-ui-imenu-enable t
        lsp-ui-peek-always-show t
        lsp-ui-sideline-ignore-duplicate t
        lsp-headerline-breadcrumb-enable t))

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(lsp-treemacs-sync-mode 1)

;; optionally if you want to use debugger
(use-package dap-mode
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip))
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1)
  )
;; Enable dap for python
(require 'dap-python)

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode)
    :custom
    (which-key-idle-delay 0.3)
    )

;; Language specific customizations
(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

;; Improved Undo
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))


;; Window swtiching
(use-package ace-window
  :config
  ;; Show the window designators in the modeline.
  (ace-window-display-mode)

   ;; Make the number indicators a little larger. I'm getting old.
  (set-face-attribute 'aw-leading-char-face nil :height 4.0 :background "black")

  (defun my-ace-window (args)
    "As ace-window, but hiding the cursor while the action is active."
    (interactive "P")
    (cl-letf
        ((cursor-type nil)
         (cursor-in-non-selected-window nil))
      (ace-window nil)))


  :bind (("C-," . my-ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l) "Designate windows by home row keys, not numbers.")
  (aw-background nil))

;; Enable moving with super
(windmove-default-keybindings 'super)
;; wrap around at edges
(setq windmove-wrap-around t)


(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)



;; -----------------------------------------------------
;; Load my theme and custom.el file
;; -----------------------------------------------------
;; Set my font
(set-frame-font "CaskaydiaCove Nerd Font 15"  nil t)
(load-file custom-file)
(require 'color-theme-sanityinc-tomorrow)

;; (load-theme 'sanityinc-tomorrow-bright)
