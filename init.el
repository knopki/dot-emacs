;;; init.el -- My Emacs Configuration.
;;; Commentary:
;;; This file was tangled (automatically generated) from `README.org'
;;; Code:

;; -*- lexical-binding: t -*-

;; Load 'early-init on old versions
(unless (>= emacs-major-version 27)
  (load-file
   (expand-file-name "early-init.el"
                     (file-name-directory (or load-file-name buffer-file-name)))))

;; Update load paths
;; Optimize: Force =lisp= and =site-lisp= at the head to reduce the startup time.


(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(when (file-directory-p (expand-file-name "site-lisp" user-emacs-directory))
  (advice-add #'package-initialize :after #'add-subdirs-to-load-path))

(update-load-path)

;; Garbage collector hack
;; Set garbage collection threshold to the normal value on setup complete. Run GC
;; on idle. Don't run GC in minibuffer and run on exit.


(defvar knopki/gc-cons-threshold (* 4 1024 1024)
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Return GC cons to normal value after loading
            (setq gc-cons-threshold knopki/gc-cons-threshold)

            ;; Run GC after some idle
            (run-with-idle-timer 5 t #'garbage-collect)

            ;; Don't GC in minibuffer at all
            (add-hook 'minibuffer-setup-hook
                      (lambda () (setq gc-cons-threshold most-positive-fixnum)))

            ;; GC after minibuffer exit
            (add-hook 'minibuffer-exit-hook
                      (lambda () (setq gc-cons-threshold knopki/gc-cons-threshold)))))

;; Initialize 'use-package
;; =use-package= package is the central gear of my configuration.

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly - [[https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751][ref]]


(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)



;; Setup package archives.


(require 'package)
(customize-set-variable 'package-archives
      (append (eval (car (get 'package-archives 'standard-value)))
              '(("org" . "http://orgmode.org/elpa/")
                ("gnu"          . "https://elpa.gnu.org/packages/")
                ("melpa" . "http://melpa.org/packages/"))))



;; Initialize packages.


(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))



;; Setup =use-package=.


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)



;; Update GPG keyring for GNU ELPA.


(use-package gnu-elpa-keyring-update)

;; Benchmarking
;; Enable startup benchmarking if started with =EMACS_BENCHMARK= environment
;; variable is set.


(when (getenv "EMACS_BENCHMARK")
  (use-package benchmark-init
    :defines swiper-font-lock-exclude
    :commands (benchmark-init/activate)
    :hook (after-init . benchmark-init/deactivate)
    :init (benchmark-init/activate)
    :config
    (setq use-package-compute-statistics t)
    (with-eval-after-load 'swiper
      (add-to-list 'swiper-font-lock-exclude 'benchmark-init/tree-mode))))

;; Setup standard file paths
;; The default paths used to store configuration files and persistent data are not
;; consistent across Emacs packages. This package sets out to fix this by changing
;; the values of path variables to put configuration files in
;; no-littering-etc-directory (defaulting to =~/.emacs.d/etc/=) and persistent data
;; files in no-littering-var-directory (defaulting to =~/.emacs.d/var/=), and by
;; using descriptive file names and subdirectories when appropriate.


(use-package no-littering :demand)

;; Emacs variables that defined in C source code

(use-package emacs
  :ensure nil
  :demand
  :init
  ;; UTF-8 as the default coding system.
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))       ; pretty
  (prefer-coding-system 'utf-8)            ; pretty
  (setq locale-coding-system 'utf-8)       ; please
  (unless (eq system-type 'windows-nt)
    (setq selection-coding-system 'utf-8)) ; with sugar on top

  (fset #'display-startup-echo-area-message #'ignore)
  :hook
  ;; Favor hard-wrapping in text modes.
  (text-mode . auto-fill-mode)

  ;; Keep cursor from getting stuck in the read-only prompt
  (minibuffer-setup-hook . cursor-intangible-mode)

  :custom
  (use-file-dialog nil "File dialogs via minibuffer only.")
  (use-dialog-box nil "Dialogs via minibuffer only.")


  (truncate-lines t "Truncate long lines.")
  (truncate-partial-width-windows nil "Truncate lines without magic.")

  ;; Tab and Space. Permanently indent with spaces, never with TABs.
  (tab-width 4 "Sane default.")
  (indent-tabs-mode nil "Tabs are evil.")
  (fill-column 80 "Wrap line at 80.")

  (delete-by-moving-to-trash t "Deleting files go to OS's trash folder.")

  (ffap-machine-p-known 'reject "Don't ping.")

  ;; Menu/Tool/Scroll bars
  (hscroll-step 1 "How many colums scroll when points get too close to the edge.")
  (scroll-step 1 "How many lines scroll when point moves out.")
  (scroll-margin 5 "Number of lines of margin at the top & bottom.")
  (scroll-conservatively 10 "Scroll up to this many lines.")
  (scroll-preserve-screen-position t)
  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
  ;; for tall lines.
  (auto-window-vscroll nil)
  ;; More performant rapid scrolling over unfontified regions. May cause brief
  ;; spells of inaccurate fontification immediately after scrolling.
  (fast-but-imprecise-scrolling t)
  ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
  ;; They're generally unhelpful and only add confusing visual clutter.
  (mode-line-default-help-echo nil)
  (show-help-function nil)

  (visible-cursor nil "Don't make cursor very visible.")
  (visible-bell t "Flash frame to represent a bell.")

  ;; Try really hard to keep the cursor from getting stuck in the read-only prompt
  ;; portion of the minibuffer.
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))

  (x-gtk-use-system-tooltips nil "Don't use GTK+ tooltip.")

  (window-resize-pixelwise t "Don't resize in steps.")
  (frame-resize-pixelwise t "Don't resize in steps.")

  (split-height-threshold nil "Favor vertical splits over horizontal ones.")

  (echo-keystrokes 0.02 "Echo key-sequence in minibuffer, like VIM does.")

  ;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
  ;; doesn't look too great with direnv, however...
  (resize-mini-windows 'grow-only)
  ;; But don't let the minibuffer grow beyond this size
  (max-mini-window-height 0.15)

  (x-underline-at-descent-line t "Underline looks a bit better when drawn lower.")

  (indicate-empty-lines t "Visually indicate empty lines.")
  (indicate-buffer-boundaries 'left "Show buffer boundaries at left fringe.")

  ;; Display the bare minimum at startup. We don't need all that noise.
  ;; The dashboard/empty scratch buffer is good enough.
  (inhibit-default-init t "Don't load default font family.")
  (inhibit-startup-screen t "Don't show startup screen.")
  (inhibit-startup-echo-area-message t "Don't echo messages.")
  (inhibit-splash-screen t "Don't show the splash screen.")
  (initial-scratch-message nil "Disable initial scratch message.")
  (initial-major-mode 'text-mode "It just text by default.")

  (history-length 1000 "Max length of history lists.")
  (history-delete-duplicates t "Delete dups in history."))

;; Files
;; Files, backups, etc.


(use-package files
  :ensure nil
  :defer t
  :preface
  (defun my-backup-enable-predicate (name)
    "Like 'normal-backup-enable-predicate but checks var directory too."
    (if (string-prefix-p no-littering-var-directory name)
        nil
      (normal-backup-enable-predicate name)))

  :config
  ;; Backups
  (setq backup-enable-predicate #'my-backup-enable-predicate)

  ;; Don't kill *Scratch*!
  (with-current-buffer
      (get-buffer "*scratch*")
    (add-hook 'kill-buffer-hook
              (lambda () (error "DENIED! don't kill my *scratch*!"))
              nil t))

  :custom
  (confirm-kill-processes nil "Kill process without confirmation.")
  (require-final-newline t "Add new line at EOF.")
  (confirm-nonexistent-file-or-buffer t "Confirm before visiting a new file or buffer.")

  (delete-old-versions -1 "Prevents any trimming of backup versions.")
  (version-control t "Make numeric backup versions unconditionally.")
  (vc-make-backup-files t "Backups of registered files are made as with other files.")

  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
   "Transforms to apply to buffer file name before auto-save."))

;; Save place
;; Point goes to the last place where it was when you previously visited the same file.


(use-package saveplace
  :ensure nil
  :defer t
  :config
  (save-place-mode 1))

;; Advice
;; Disable warnings from legacy advice system. They aren't useful, and we can't
;; often do anything about them besides changing packages upstream.


(use-package advice
  :ensure nil
  :defer t
  :custom
  (ad-redefinition-action 'accept "Disable warnings."))

;; Apropos
;; Make apropos omnipotent. It's more useful this way.


(use-package apropos
  :ensure nil
  :defer t
  :custom
  (apropos-do-all t "Make apropos omnipotent."))

;; Simple

(use-package simple
  :ensure nil
  :diminish visual-line-mode auto-fill-function
  :hook
  (window-setup . size-indication-mode)
  :config
  ;; Typing yes/no is obnoxious when y/n will do.
  (defalias #'yes-or-no-p #'y-or-n-p)
  :custom
  (column-number-mode t "Display column number in the mode line.")
  (line-number-mode t "Display line number in the mode line.")
  (line-move-visual nil "Keep cursor at end of lines.")
  (track-eol t "Vertical motion starting at EOF keeps to EOL.")
  (set-mark-command-repeat-pop t "Repeating C-SPC after popping mark pops it again.")

  ;; Eliminate duplicates in the kill ring. That is, if you kill the
  ;; same thing twice, you won't have to use M-y twice to get past it
  ;; to older entries in the kill ring.
  (kill-do-not-save-duplicates t "Don't add same string twice.")

  (save-interprogram-paste-before-kill
   t "Save clipboard contents into kill-ring before replacing them."))

;; Whitespaces
;; Delete trailing whitespaces on buffer save.


(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

;; Recent files
;; Exclude some files from =recentf= lists and save list on save and some times on timer.


(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  ;; Save recent list some times
  (run-at-time t (* 5 60) 'recentf-save-list)
  :custom
  (recentf-max-saved-items 200 "Many-many items in recent list.")
  (recentf-exclude
   '("\\.?cache"
     "url"
     "COMMIT_EDITMSG\\'"
     "bookmarks"
     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
     "^/tmp/"
     "^/ssh:"
     "\\.?ido\\.last$"
     "\\.revive$"
     "/TAGS$"
     (lambda (file) (file-in-directory-p file package-user-dir))
     (expand-file-name recentf-save-file)
     no-littering-var-directory
     no-littering-etc-directory) "Excludes from recent list."))

;; Save history

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (enable-recursive-minibuffers t "Allow minibuffer commands while in minibuffer.")
  (savehist-additional-variables
   '(mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history) "Additional variables to save.")
  (savehist-autosave-interval 300 "Save history sometime."))

;; Auto revert
;; Revert buffer of file change on disk.


(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-check-vc-info t "Update version control.")
  (auto-revert-verbose nil "Silent auto revert."))

;; Delete selection

(use-package delsel
  :ensure nil
  :custom
  (delete-selection-mode t "Replace the active region just by typing text."))

;; Unique buffer names

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward "bar/mumble/name"))

;; On-the-fly spell checker
;; =hunspell= is must because of ability to query multiple dictionaries.


(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "hunspell")
  :hook
  (((text-mode outline-mode org-mode) . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :init
  (with-eval-after-load "ispell"
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US,ru_RU")
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,ru_RU"))
  :custom
  (flyspell-issue-message-flag nil "Be silent."))

;; Desktop save and load
;; Restore last autosaved session.


(use-package desktop
  :ensure nil
  :hook
  ;; Must be loaded after 'doom-modeline
  ;; See: https://github.com/seagle0128/doom-modeline/issues/216
  (doom-modeline-mode . desktop-revert)
  :custom
  (desktop-restore-eager 10 "Restore immediately last N buffers.")
  (desktop-lazy-verbose nil "Be silent.")
  :config
  (setq desktop-save-mode t))

;; Evil mode
;; I like VIM keys much more, so =evil-mode= is essential part of my configuration.


(use-package evil
  :diminish undo-tree-mode
  :hook (after-init . evil-mode)
  :custom
  (evil-want-keybinding nil "Don't load evil-keybindings - required by evil-collection")
  (evil-search-wrap t "Search wrap around the buffer.")
  (evil-regexp-search t "Search with regexp.")
  (evil-search-module 'evil-search "Search module to use.")
  (evil-vsplit-window-right t "Like vim's 'splitright'.")
  (evil-split-window-below t "Like vim's 'splitbelow'.")
  (evil-want-C-u-scroll t "Enable C-u scroll.")
  (evil-want-C-i-jump nil "Disable C-i jumps in jump list.")
  :config
  ;; (evil-set-initial-state 'term-mode 'emacs)

  ;; Visually selected text gets replaced by the latest copy action
  ;; Amazing hack lifted from: http://emacs.stackexchange.com/a/15054/12585
  (fset 'evil-visual-update-x-selection 'ignore))

;; Evil collection
;; Vim-like keybindings everywhere in Emacs.


(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; El General
;; More convenient method for binding keys.


(use-package general
  :config
  (general-evil-setup t)

  (general-nmap
    :prefix "SPC"
    "<f1>" 'general-describe-keybindings
    "c"    'calc)

  ;; Move visual block
  (general-vmap
    "J" (concat ":m '>+1" (kbd "RET") "gv=gv")
    "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

;; Reverse-im
;; Use bindings while the non-default system layout is active.


(use-package reverse-im
  :custom
  (reverse-im-modifiers '(control meta super))
  :config
  (reverse-im-activate "russian-computer"))

;; Mouse

(use-package mouse
  :ensure nil
  :defer t
  :custom
  (mouse-yank-at-point t "Yanks at point instead of click."))



;; Mouse wheel settings.


(use-package mwheel
  :ensure nil
  :defer t
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 5)) "Amount of scroll by mouse wheel.")
  (mouse-wheel-progressive-speed nil "Progressive scrolling."))

;; Tooltips
;; Don't display floating tooltips; display their contents in the echo-area.


(use-package tooltip
  :ensure nil
  :defer t
  :custom
  (tooltip-mode nil))

;; Frame

(use-package frame
  :ensure nil
  :hook
  ;; Display dividers between windows
  (window-setup . window-divider-mode)
  :custom
  (blink-cursor-mode nil "Don't blink the cursor.")
  ;; Display dividers between windows
  (window-divider-default-places t "Dividers on the bottom and on the right.")
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1))

;; Doom theme
;; Setup Doom themes (use One Dark), set font face.


(use-package doom-themes
  :custom-face (default ((t (:family "FuraCode Nerd Font Mono" :height 120))))
  :defines doom-themes-treemacs-theme
  :functions doom-themes-hide-modeline
  :init (load-theme 'doom-one t)
  :custom
  (doom-themes-treemacs-theme "doom-colors")
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Enable customized theme (`all-the-icons' must be installed!)
  (doom-themes-treemacs-config)
  (with-eval-after-load 'treemacs
    (remove-hook 'treemacs-mode-hook #'doom-themes-hide-modeline)))

;; Evil goggles
;; Displays a visual hint when editing with evil.


(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :defer 2
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Which key
;; Displays the key bindings following your currently entered incomplete command (a
;; prefix) in a popup.


(use-package which-key
  :diminish which-key-mode
  :defer 2
  :general
  (general-nmap
    :prefix "SPC"
    ;; Show top level key bindings
    "<f2>" 'which-key-show-top-level
    ;; Show major mode key bindings
    "<f3>" 'which-key-show-major-mode
    ;; Show key bindings from any keymap
    "<f4>" 'which-key-show-full-keymap)
  :config
  (which-key-setup-side-window-right-bottom))

;; Better help

(use-package helpful
  :defer 2
  :bind
  (:map help-mode-map
        ("f" . #'helpful-callable)
        ("v" . #'helpful-variable)
        ("k" . #'helpful-key)
        ("F" . #'helpful-at-point)
        ("F" . #'helpful-function)
        ("C" . #'helpful-command))
  :custom
  ;; Ivy support
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Ediff
;; A comprehensive visual interface to diff & patch.


(use-package ediff
  :ensure nil
  :hook
  ;; show org ediffs unfolded
  ((ediff-prepare-buffer . outline-show-all)
   ;; restore window layout when done
   (ediff-quit . winner-undo))

  :custom
  (ediff-window-setup-function 'ediff-setup-windows-multiframe))

;; Eldoc

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook
  (prog-mode . eldoc-mode)
  :custom
  (global-eldoc-mode -1 "Disable global mode."))

;; Tabify
;; Buffer re-tabbing.


(use-package tabify
  :ensure nil
  :commands (tabify untabify)
  :config
  (setq tabify-regexp "^\t* [ \t]+"))

;; Highlight matching parens

(use-package paren
  :ensure nil
  :custom
  (show-paren-mode t "Enable show matching parens."))

;; Automatic parenthesis pairing

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Display line numbers

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; Evil commentary
;; =gc= operator, like =vim-commentary=.


(use-package evil-commentary
  :after evil
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))

;; Evil surround
;; Emulates =vim-surround=.


(use-package evil-surround
  :after evil
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :general
  (:states 'operator
           "s" 'evil-surround-edit
           "S" 'evil-Surround-edit)
  (:states 'visual
           "S" 'evil-surround-region
           "gS" 'evil-Surround-region))

;; C/C++

(use-package cc-vars
  :ensure nil
  :defer t
  :custom
  (c-basic-offset 4 "Default indentation.")
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "k&r"))))

;; Python

(use-package python
  :ensure nil
  :defer t
  :custom
  (python-indent-offset 2))

;; Javascript

(use-package js
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2))

;; Old init file

;; Essential look & feel (doomed)
(require 'init-doom-modeline)
(require 'init-all-the-icons)
(require 'init-dashboard)
(require 'init-hide-mode-line)
(require 'init-solaire-mode)

;; Global modes
(require 'init-undo-tree)
(require 'init-ivy)
(require 'init-avy)
(require 'init-persistent-scratch)

;; Sometimes modes
(require 'init-ibuffer)
(require 'init-org)

;; Programming
(require 'init-flycheck)
(require 'init-projectile)
(require 'init-company)
(require 'init-aggressive-indent)
(require 'init-magit)
(require 'init-diff-hl)
(require 'init-yasnippet)
(require 'init-direnv)
(require 'init-nix)


;; Load manual customizations
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here

;; The end…
;; Add standard module footer.


(provide 'init)
;;; init.el ends here
