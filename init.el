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

;; Garbage collector hack
;; Set garbage collection threshold to the normal value on setup complete. Run GC
;; on idle. Don't run GC in minibuffer and run on exit.


(defvar knopki/gc-cons-threshold (* 30 1024 1024)
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


(use-package gnu-elpa-keyring-update
  :commands (gnu-elpa-keyring-update))

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

;; Icons
;; Dependency of many packages. Display nice icons.


(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (or (eq system-type 'windows-nt)
                    (member "all-the-icons" (font-family-list)))
          (all-the-icons-install-fonts t))
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

;; Evil mode
;; I like VIM keys much more, so =evil-mode= is essential part of my configuration.


(use-package evil
  :hook (after-init . evil-mode)
  :commands (evil-ex-define-cmd)
  :defer .1
  :custom
  (evil-want-keybinding nil "Don't load evil-keybindings - required by evil-collection")
  (evil-motion-state-modes nil "Use 'normal instead of 'motion state.")
  (evil-emacs-state-modes nil "Use 'normal instead of 'emacs state.")
  (evil-search-wrap t "Search wrap around the buffer.")
  (evil-regexp-search t "Search with regexp.")
  (evil-search-module 'evil-search "Search module to use.")
  (evil-vsplit-window-right t "Like vim's 'splitright'.")
  (evil-split-window-below t "Like vim's 'splitbelow'.")
  (evil-want-C-u-scroll t "Enable C-u scroll.")
  (evil-want-C-i-jump nil "Disable C-i jumps in jump list.")
  :config
  ;; Visually selected text gets replaced by the latest copy action
  ;; Amazing hack lifted from: http://emacs.stackexchange.com/a/15054/12585
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Vim-like keybindings everywhere in Emacs.
  (use-package evil-collection
    :custom
    (evil-collection-setup-minibuffer t)
    :config
    (evil-collection-init)))

;; El General
;; More convenient method for binding keys. Setup leader key definers.

;; =SPC= is the leader key with =M-SPC= alternative in some states.

;; =​,= is the major mode leader key with =M-,= alternative in some states. Aliased
;; by =SPC m=.

;; =SPC u= is the universal argument instead of standard =C-u=.


(use-package general
  :commands (general-leader general-major-leader)
  :config
  (general-evil-setup)

  (general-create-definer general-leader
    :keymaps 'override
    :states '(insert motion normal emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer general-major-leader
    :states '(insert motion emacs)
    :prefix ","
    :non-normal-prefix "M-,")
  (general-nmap "SPC m" (general-simulate-key "," :which-key "major mode"))
  (general-leader "u" '(universal-argument :wk "Universal argument")))

;; Reverse-im
;; Use bindings while the non-default system layout is active.


(use-package reverse-im
  :custom
  (reverse-im-modifiers '(control meta super))
  :config
  (reverse-im-activate "russian-computer"))

;; Some global keybindings

;; Common prefixes:
;; | prefix  | meaning          |
;; |---------+------------------|
;; | ~SPC a~ | Applications     |
;; | ~SPC b~ | Buffers          |
;; | ~SPC e~ | Error management |
;; | ~SPC f~ | Files            |
;; | ~SPC j~ | Jump/join/split  |
;; | ~SPC K~ | Macros           |
;; | ~SPC m~ | Major mode       |
;; | ~SPC t~ | Global toggles   |


(general-leader
  "" nil
  "K" '(nil :wk "Macros")
  "R" '(nil :wk "Rectangles")
  "a"  '(nil :wk "Applications")
  "b" '(nil :wk "Buffers")
  "e" '(nil :wk "Error management")
  "f" '(nil :wk "Files")
  "j" '(nil :wk "Jump/join/split")
  "t" '(nil :wk "Global toggles"))



;; Application:
;; | key       | command                 |
;; |-----------+-------------------------|
;; | ~SPC a c~ | Calculator              |
;; | ~SPC a p~ | List Emacs subprocesses |
;; | ~SPC a P~ | List system processes   |


(general-leader
  "ac" '(calc-dispatch :wk "Calculator")
  "ap" '(list-processes :wk "List subprocesses")
  "aP" '(proced :wk "List system processes"))

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
  (backup-by-copying t "Don't clobber symlinks.")

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

;; Recent files
;; Exclude some files from =recentf= lists and save list on save and some times on timer.


(use-package recentf
  :ensure nil
  :commands (recentf-open-files recentf-save-list)
  :hook (after-find-file . recentf-mode)
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
  :hook (pre-command . savehist-mode)
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

;; Persistent Scratch buffer
;; Save *scratch* buffer content.


(use-package persistent-scratch
  :hook (after-find-file . persistent-scratch-autosave-mode)
  :config
  (persistent-scratch-setup-default))

;; Customization

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (no-littering-expand-etc-file-name "custom.el"))
  :config
  (load custom-file nil t))

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
  (menu-bar-mode nil "No menu bar.")
  (tool-bar-mode nil "No tool bar.")
  (scroll-bar-mode nil "No scroll bar.")
  (blink-cursor-mode nil "Don't blink the cursor.")
  ;; Display dividers between windows
  (window-divider-default-places t "Dividers on the bottom and on the right.")
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1))

;; Winner mode
;; Restore old window configurations.


(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :custom
  (winner-boring-buffers '("*Apropos*"
                           "*Buffer List*"
                           "*Compile-Log*"
                           "*Completions*"
                           "*Fuzzy Completions*"
                           "*Help*"
                           "*Ibuffer*"
                           "*cvs*"
                           "*esh command on file*"
                           "*inferior-lisp*")))

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

;; Modeline
;; Use Doom modeline.

;; TODO: set smaller font face after package upgrade


(use-package doom-modeline
  :diminish
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-minor-modes t "Display minor modes.")
  (doom-modeline-unicode-fallback t "Use unicode when no icons.")
  (doom-modeline-buffer-file-name-style 'buffer-name "Just show unique buffer name.")
  (doom-modeline-window-width-limit fill-column "The limit of the window width")
  (doom-modeline-project-detection 'project)
  (doom-modeline-env-load-string "♻"))



;; Hide minor modes to menu.


(use-package minions
  :diminish
  :hook (doom-modeline-mode . minions-mode))



;; Hide modeline when needed.


(use-package hide-mode-line
  :hook
  ;; Hide mode-line for completion list
  ((completion-list-mode completion-in-region-mode) . hide-mode-line-mode))

;; Solaire mode
;; Visually distinguish file-visiting windows from other types of windows.


(use-package solaire-mode
  :hook
  (((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
   (minibuffer-setup . solaire-mode-in-minibuffer)
   (after-load-theme . solaire-mode-swap-bg))
  :custom
  (solaire-mode-remap-fringe nil "Don't colorize fringe.")
  :config
  (solaire-mode-swap-bg))

;; Evil goggles
;; Displays a visual hint when editing with evil.


(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :hook (pre-command . evil-goggles-mode)
  :config
  (evil-goggles-use-diff-faces))

;; Prescient
;; Library which sorts and filters lists of candidates.


(use-package prescient
  :commands prescient-persist-mode
  :after counsel
  :hook (ivy-mode . prescient-persist-mode)
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy)
                           "How to interpret filtering queries."))

;; Ivy
;; A generic completion frontend.


(use-package ivy
  :diminish ivy-mode
  :defer t
  :hook (pre-command . ivy-mode)
  :preface
  (defun my-ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (display-graphic-p)
                   (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
                 ">")
               (propertize " " 'display `(space :align-to 2))
               (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n"))
  :custom
  (ivy-use-selectable-prompt t "Make the prompt line selectable like a candidate.")
  (ivy-use-virtual-buffers t "Add recent files/bookmarks to ivy-switch-buffer.")
  (ivy-height 15 "Number of lines for the minibuffer window.")
  (ivy-fixed-height-minibuffer t "Fix the height of minibuffer during completion.")
  (ivy-count-format "(%d/%d)" "index/count format.")
  (ivy-on-del-error-function nil "Do nothing on backward delete error.")
  (ivy-initial-inputs-alist nil)
  (ivy-format-functions-alist
   '((counsel-describe-face . counsel--faces-format-function)
     (t . my-ivy-format-function-arrow))
   "Functions that transform the list of candidates into string."))

;; Ivy prescient
;; Better sorting and filtering for Ivy.


(use-package ivy-prescient
  :commands ivy-prescient-re-builder
  :after (prescient)
  :hook (ivy-mode . ivy-prescient-mode)
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
  :preface
  (defun ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))
  :custom
  (ivy-prescient-retain-classic-highlighting t "Emulate the Ivy highlights candidates.")
  (ivy-re-builders-alist '((counsel-rg . ivy-prescient-non-fuzzy)
                           (counsel-pt . ivy-prescient-non-fuzzy)
                           (counsel-grep . ivy-prescient-non-fuzzy)
                           (counsel-imenu . ivy-prescient-non-fuzzy)
                           (counsel-projectile-grep . ivy-prescient-non-fuzzy)
                           (counsel-projectile-rg . ivy-prescient-non-fuzzy)
                           (counsel-yank-pop . ivy-prescient-non-fuzzy)
                           (projectile-grep . ivy-prescient-non-fuzzy)
                           (projectile-ripgrep . ivy-prescient-non-fuzzy)
                           (swiper . ivy-prescient-non-fuzzy)
                           (swiper-isearch . ivy-prescient-non-fuzzy)
                           (swiper-all . ivy-prescient-non-fuzzy)
                           (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
                           (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
                           (insert-char . ivy-prescient-non-fuzzy)
                           (counsel-unicode-char . ivy-prescient-non-fuzzy)
                           (t . ivy-prescient-re-builder))
                         "A list of regex building funcs for each collection func.")
  (ivy-prescient-sort-commands
   '(:not swiper swiper-isearch ivy-switch-buffer
          counsel-grep counsel-git-grep counsel-ag counsel-imenu
          counsel-yank-pop counsel-recentf counsel-buffer-or-recentf)
   "Which commands have their candidates sorted by prescient."))

;; Counsel
;; Collection of Ivy-enhanced versions of common Emacs commands.


(use-package counsel
  :diminish counsel-mode
  :after (ivy-prescient)
  :hook (ivy-mode . counsel-mode)
  :custom
  (counsel-find-file-at-point t "" "Add file-at-point to the list of candidates.")
  (counsel-yank-pop-separator
   "\n────────\n" "Separator for kill rings in counsel-yank-pop.")
  (counsel-grep-base-command
   (if (executable-find "rg")
       "rg -S --no-heading --line-number --color never '%s' %s"
     "grep -E -n -e %s %s")
   "Use the faster search tool: ripgrep."))

;; Swiper
;; isearch alternative.


(use-package swiper
  :custom
  (swiper-action-recenter t "Recenter after exiting swiper."))

;; Ivy rich
;; More friendly display transformer for Ivy.
;; TODO: Minimize when PR merged https://github.com/melpa/melpa/pull/6669


(use-package ivy-rich
  :after (:all (ivy counsel-projectile all-the-icons))
  :hook ((counsel-projectile-mode . ivy-rich-mode) ; Must load after `counsel-projectile'
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :preface
  (with-no-warnings
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
               (buffer-file-name (buffer-file-name buffer))
               (major-mode (buffer-local-value 'major-mode buffer))
               (icon (with-current-buffer buffer (all-the-icons-icon-for-buffer))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((path (concat ivy--directory candidate))
               (file (file-name-nondirectory path))
               (icon (cond
                      ((file-directory-p path)
                       (all-the-icons-icon-for-dir path nil ""))
                      ((string-match "^/.*:$" path)
                       (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                      ((not (string-empty-p file))
                       (all-the-icons-icon-for-file file :v-adjust -0.05)))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-project-icon (_candidate)
      "Display project icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

    (defun ivy-rich-mode-icon (_candidate)
      "Display mode icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)))

    (defun ivy-rich-function-icon (_candidate)
      "Display function icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)))

    (defun ivy-rich-variable-icon (_candidate)
      "Display the variable icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)))

    (defun ivy-rich-symbol-icon (_candidate)
      "Display the symbol icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

    (defun ivy-rich-theme-icon (_candidate)
      "Display the theme icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2)))

    (defun ivy-rich-keybinding-icon (_candidate)
      "Display the keybindings icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "keyboard" :height 0.9 :v-adjust -0.15)))

    (defun ivy-rich-library-icon (_candidate)
      "Display the library icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue)))

    (defun ivy-rich-package-icon (_candidate)
      "Display the package icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))

    (defun ivy-rich-font-icon (_candidate)
      "Display the font icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-world-clock-icon (_candidate)
      "Display the world clock icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-tramp-icon (_candidate)
      "Display the tramp icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01)))

    (defun ivy-rich-git-branch-icon (_candidate)
      "Display the git branch icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green)))

    (defun ivy-rich-process-icon (_candidate)
      "Display the process icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "bolt" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-imenu-icon (candidate)
      "Display the imenu icon in `ivy-rich'."
      (when (display-graphic-p)
        (let ((case-fold-search nil))
          (cond
           ((string-match-p "Type Parameters?[:)]" candidate)
            (all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
           ((string-match-p "\\(Variables?\\)\\|\\(Fields?\\)\\|\\(Parameters?\\)[:)]" candidate)
            (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue))
           ((string-match-p "Constants?[:)]" candidate)
            (all-the-icons-faicon "square-o" :height 0.95 :v-adjust -0.15))
           ((string-match-p "Enum\\(erations?\\)?[:)]" candidate)
            (all-the-icons-material "storage" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-orange))
           ((string-match-p "References?[:)]" candidate)
            (all-the-icons-material "collections_bookmark" :height 0.95 :v-adjust -0.2))
           ((string-match-p "\\(Types?\\)\\|\\(Property\\)[:)]" candidate)
            (all-the-icons-faicon "wrench" :height 0.9 :v-adjust -0.05))
           ((string-match-p "\\(Functions?\\)\\|\\(Methods?\\)\\|\\(Constructors?\\)[:)]" candidate)
            (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple))
           ((string-match-p "\\(Class\\)\\|\\(Structs?\\)[:)]" candidate)
            (all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.15 :face 'all-the-icons-orange))
           ((string-match-p "Interfaces?[:)]" candidate)
            (all-the-icons-material "share" :height 0.95 :v-adjust -0.2 :face 'all-the-icons-lblue))
           ((string-match-p "Modules?[:)]" candidate)
            (all-the-icons-material "view_module" :height 0.95 :v-adjust -0.15 :face 'all-the-icons-lblue))
           ((string-match-p "Packages?[:)]" candidate)
            (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver))
           (t (all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.125))))))

    (when (display-graphic-p)
      (defun my-ivy-rich-bookmark-type (candidate)
        (let ((filename (ivy-rich-bookmark-filename candidate)))
          (cond ((null filename)
                 (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
                ((file-remote-p filename)
                 (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01))
                ((not (file-exists-p filename))
                 (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
                ((file-directory-p filename)
                 (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
      (advice-add #'ivy-rich-bookmark-type :override #'my-ivy-rich-bookmark-type)))
  :init
  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))
  :custom
  (ivy-rich-parse-remote-buffer nil "For better performance.")
  (ivy-rich-display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     ivy-switch-buffer-other-window
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-switch-buffer
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-switch-buffer-other-window
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     persp-switch-to-buffer
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-M-x
     (:columns
      ((ivy-rich-function-icon)
       (counsel-M-x-transformer (:width 50))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((ivy-rich-function-icon)
       (counsel-describe-function-transformer (:width 50))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((ivy-rich-variable-icon)
       (counsel-describe-variable-transformer (:width 50))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-set-variable
     (:columns
      ((ivy-rich-variable-icon)
       (counsel-describe-variable-transformer (:width 50))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-apropos
     (:columns
      ((ivy-rich-symbol-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-info-lookup-symbol
     (:columns
      ((ivy-rich-symbol-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-descbinds
     (:columns
      ((ivy-rich-keybinding-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-find-file
     (:columns
      ((ivy-rich-file-icon)
       (ivy-read-file-transformer))
      :delimiter "\t")
     counsel-file-jump
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-dired
     (:columns
      ((ivy-rich-file-icon)
       (ivy-read-file-transformer))
      :delimiter "\t")
     counsel-dired-jump
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-el
     (:columns
      ((ivy-rich-symbol-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-fzf
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-git
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-recentf
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
      :delimiter "\t")
     counsel-buffer-or-recentf
     (:columns
      ((ivy-rich-file-icon)
       (counsel-buffer-or-recentf-transformer (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
      :delimiter "\t")
     counsel-bookmark
     (:columns
      ((ivy-rich-bookmark-type)
       (ivy-rich-bookmark-name (:width 40))
       (ivy-rich-bookmark-info))
      :delimiter "\t")
     counsel-bookmarked-directory
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-package
     (:columns
      ((ivy-rich-package-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-fonts
     (:columns
      ((ivy-rich-font-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-major
     (:columns
      ((ivy-rich-function-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-find-library
     (:columns
      ((ivy-rich-library-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-load-library
     (:columns
      ((ivy-rich-library-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-load-theme
     (:columns
      ((ivy-rich-theme-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-world-clock
     (:columns
      ((ivy-rich-world-clock-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-tramp
     (:columns
      ((ivy-rich-tramp-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-git-checkout
     (:columns
      ((ivy-rich-git-branch-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-list-processes
     (:columns
      ((ivy-rich-process-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-projectile-switch-project
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-projectile-find-file
     (:columns
      ((ivy-rich-file-icon)
       (counsel-projectile-find-file-transformer))
      :delimiter "\t")
     counsel-projectile-find-dir
     (:columns
      ((ivy-rich-project-icon)
       (counsel-projectile-find-dir-transformer))
      :delimiter "\t")
     counsel-minor
     (:columns
      ((ivy-rich-mode-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-imenu
     (:columns
      ((ivy-rich-imenu-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     treemacs-projectile
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t"))))

;; Core

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :hook ((pre-command after-find-file) . company-mode)
  :bind
  (:map company-active-map
        ("M-RET" . company-complete-selection)
        ("M-q"   . company-other-backend))
  :custom
  (company-tooltip-align-annotations t "Align annotation to the right side.")
  (company-minimum-prefix-length 2 "Minimum prefix length for idle completion.")
  (company-idle-delay 0.2 "Idle delay in seconds before completion starts.")
  (company-show-numbers t "Number the candidates (use M-1, M-2 etc to select completions).")
  (company-eclim-auto-save nil "Stop eclim auto save.")
  (company-dabbrev-downcase nil "No downcase when completion.")
  (company-dabbrev-ignore-case nil "Ignore case when collection candidates.")
  (company-selection-wrap-around t "Selecting item <first|>last wraps around.")
  (company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)
   "Disable for some modes.")
  (company-global-modes nil)
  (company-backends '((company-capf company-files)) "Default list of active backends.")
  (company-frontends
   '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
   "List of active frontends.")
  :config
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends #'company-dabbrev-code)))

;; Company Prescient
;; Better sorting and filtering.


(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

;; Company Box
;; A company front-end with icons.


(use-package company-box
  :diminish
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-enable-icon t "Display icons.")
  (company-box-show-single-candidate t "Display when only one candidate.")
  (company-box-max-candidates 50 "Maximum number of candidates.")
  :config
  (with-no-warnings
    ;; Highlight `company-common'
    (defun my-company-box--make-line (candidate)
      (-let* (((candidate annotation len-c len-a backend) candidate)
              (color (company-box--get-color backend))
              ((c-color a-color i-color s-color) (company-box--resolve-colors color))
              (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
              (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                        (substring (propertize candidate 'face 'company-box-candidate)
                                                   (length company-common) nil)))
              (align-string (when annotation
                              (concat " " (and company-tooltip-align-annotations
                                               (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
              (space company-box--space)
              (icon-p company-box-enable-icon)
              (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
              (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                              (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                            (company-box--apply-color icon-string i-color)
                            (company-box--apply-color candidate-string c-color)
                            align-string
                            (company-box--apply-color annotation-string a-color)))
              (len (length line)))
        (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                         'company-box--color s-color)
                             line)
        line))
    (advice-add #'company-box--make-line :override #'my-company-box--make-line)

    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
          (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
          (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.1))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
          (Template . ,(all-the-icons-material "format_align_left" :height 0.85 :v-adjust -0.2)))
        company-box-icons-alist 'company-box-icons-all-the-icons))

;; Which key
;; Displays the key bindings following your currently entered incomplete command (a
;; prefix) in a popup.

;; Global keybindings:
;; | key       | command                    |
;; |-----------+----------------------------|
;; | ~SPC h k~ | Show top level keybindings |

;; Keybindings in =which-key= window.
;; | key | command      |
;; |-----+--------------|
;; | ~q~ | Exit         |
;; | ~j~ | Next page    |
;; | ~k~ | Previous key |
;; | ~?~ | Show help    |


(use-package which-key
  :diminish which-key-mode
  :hook (pre-command . (lambda () (which-key-mode +1)))
  :general
  (general-leader
    "hk" '(which-key-show-top-level :wk "Show top level keybindings"))
  :custom
  (which-key-max-description-length 32)
  (which-key-allow-multiple-replacements t)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-allow-evil-operators t)
  :config
  (which-key-setup-side-window-right-bottom))

;; Better help

(use-package helpful
  :commands (helpful--read-symbol)
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

;; Bookmarks
;; Jump to bookmark.

;; | key       | command          |
;; |-----------+------------------|
;; | ~SPC f b~ | Jump to bookmark |


(use-package bookmark
  :ensure nil
  :defer t
  :general
  (general-leader
    "fb" 'bookmark-jump))

;; Avy
;; Jump to things in Emacs tree-style.


(use-package avy
  :commands (avy-goto-word-1 evil-avy-goto-char-timer evil-avy-goto-word-0)
  :general
  (general-mmap
    "C-'" 'evil-avy-goto-char-timer
    "C-\"" 'evil-avy-goto-word-0))

;; Keybindings

(general-leader
  "TAB"    '(mode-line-other-buffer :wk "Alternate buffer")
  "bd"     '(kill-this-buffer :wk "Kill this buffer")
  "be"     '(erase-buffer :wk "Erase buffer")
  "bn"     '(next-buffer :wk "Next buffer")
  "bN C-i" '(make-indirect-buffer :wk "Create indirect buffer")
  "bNi"    '(clone-indirect-buffer :wk "Indirect buffer from current")
  "bNI"    '(clone-indirect-buffer-other-window
             :wk "Indirect buffer from current in other window")
  "bp"     '(previous-buffer :wk "Previous buffer")
  "bx"     '(kill-buffer-and-window :wk "Kill buffer with window")
  "bw"     '(read-only-mode :wk "Read only buffer"))

;; Kill buffer without window
(eval-after-load 'evil-mode
  (evil-ex-define-cmd "bd[elete]" #'kill-this-buffer))

;; Unique buffer names

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward "bar/mumble/name"))

;; ibuffer
;; Better buffer menu.


(use-package ibuffer
  :ensure nil
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-auto-mode-match?
              all-the-icons-faicon
              my-ibuffer-find-file)
  :commands (ibuffer-find-file
             ibuffer-current-buffer)
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))
                                  "Use for displaying filtering group names.")
  :config
  ;; Replace evil :ls etc
  (with-eval-after-load 'evil
    (evil-define-command evil-show-files ()
      "Shows the file-list."
      :repeat nil
      (ibuffer))
    (evil-ex-define-cmd "buffers" 'evil-show-files))

  ;; Intergrate counsel
  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                 (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                   default-directory))))
        (counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

  ;; Display buffer icons on GUI
  (when (and (display-graphic-p) (require 'all-the-icons nil t))
    ;; For alignment, the size of the name field should be the width of an icon
    (define-ibuffer-column icon (:name "  ")
      (let ((icon (if (and (buffer-file-name)
                           (all-the-icons-auto-mode-match?))
                      (all-the-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                    (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0))
          icon)))

    (setq ibuffer-formats `((mark modified read-only ,(if (>= emacs-major-version 26) 'locked "")
                                  ;; Here you may adjust by replacing :right with :center or :left
                                  ;; According to taste, if you want the icon further from the name
                                  " " (icon 2 2 :left :elide)
                                  ,(propertize " " 'display `(space :align-to 8))
                                  (name 18 18 :left :elide)
                                  " " (size 9 -1 :right)
                                  " " (mode 16 16 :left :elide) " " filename-and-process)
                            (mark " " (name 16 -1) " " filename)))))



;; Group ibuffer's list by project root.


(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :custom
  (ibuffer-projectile-prefix
   (if (display-graphic-p)
       (concat
        (all-the-icons-octicon "file-directory"
                               :face ibuffer-filter-group-name-face
                               :v-adjust -0.05
                               :height 1.25)
        " ")
     "Project: "))
  :config
  (add-to-list 'ibuffer-never-show-predicates "^\\*helpful"))

;; Projectile
;; Manage and navigate projects.


(use-package projectile
  :diminish projectile-mode
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file)
  :hook
  ((after-find-file dired-before-readin minibuffer-setup) . projectile-mode)
  :general
  (general-leader
    "p" '(:keymap projectile-command-map :package projectile))
  :custom
  (projectile-mode-line-prefix "" "Mode line lighter prefix for Projectile.")
  (projectile-sort-order 'recentf "Sort order.")
  (projectile-use-git-grep t "Use git grep in git projects.")
  (projectile-enable-cache t)
  (projectile-completion-system 'ivy "Ivy integration.")
  :config
  ;; Update mode-line at the first time
  (projectile-update-mode-line))

;; Projectile Ivy integration
;; More advanced Ivy integration.


(use-package counsel-projectile
  :defer t
  :after (projectile)
  :hook (after-init . counsel-projectile-mode)
  :custom
  (counsel-projectile-rg-initial-input
   '(projectile-symbol-or-selection-at-point)
   "Initial minibuffer input.")
  :config
  (define-obsolete-function-alias 'counsel-more-chars 'ivy-more-chars "26.3"))

;; Dired
;; Standard file manager.

;; Global keys:
;; | key       | commandard                             |
;; |-----------+----------------------------------------|
;; | ~​SPC a d~ | Start =dired=                          |
;; | ~SPC f j~ | Jump to =dired= buffer                 |
;; | ~SPC j d~ | Jump to =dired= buffer                 |
;; | ~SPC j D~ | Jump to =dired= buffer in other window |

;; Mode keys:
;; | key           | commandard                                             |
;; |---------------+--------------------------------------------------------|
;; | ~q~           | Quit                                                   |
;; | ~j~           | Next line                                              |
;; | ~k~           | Previous line                                          |
;; |               |                                                        |
;; | ~#~           | Flag auto save files                                   |
;; | ~.~           | Flag numerical backupd for deletion                    |
;; | ~~~           | Flag backups for deletion                              |
;; |               |                                                        |
;; | ~A~           | Flag all matches for regexp in all marked files        |
;; | ~B~           | Byte compile marked files                              |
;; | ~C~           | Copy all marked files                                  |
;; | ~D~           | Delete marked files                                    |
;; | ~gG~          | Change group of marked files                           |
;; | ~H~           | Create hardlinks of marked files                       |
;; | ~L~           | Load marked Elisp files                                |
;; | ~M~           | Chmod marked files                                     |
;; | ~O~           | Chown marked files                                     |
;; | ~P~           | Print marked files                                     |
;; | ~Q~           | Find regexp and replace in marked files                |
;; | ~R~           | Rename marked files                                    |
;; | ~S~           | Symlink marked files                                   |
;; | ~T~           | Touch marked files                                     |
;; | ~X~           | Run shell command on marked files                      |
;; | ~Z~           | (Un)compress marked files                              |
;; | ~c~           | Compress marked files to archive                       |
;; | ~!~           | Run shell command on marked files                      |
;; | ~&~           | Run async shell command on marked files                |
;; |               |                                                        |
;; | ~=~           | Dired diff                                             |
;; |               |                                                        |
;; | ~M-C-?~       | Unmark all files                                       |
;; | ~M-C-d~       | Go down in tree                                        |
;; | ~M-C-u~       | Go up in tree                                          |
;; | ~M-C-n~       | Go to the next subdirectory                            |
;; | ~M-C-p~       | Go to the previous subdirectory                        |
;; |               |                                                        |
;; | ~M-{~         | Go to previous marked file                             |
;; | ~M-}~         | To to the next marked file                             |
;; |               |                                                        |
;; | ~%u~          | Rename all marked files to upper case                  |
;; | ~%l~          | Rename all marked files to downcase                    |
;; | ~%d~          | Mark all files that contains regexp for deletion       |
;; | ~%g~          | Mark all files that contains regexp                    |
;; | ~%m~          | Mark all files that contains regexp                    |
;; | ~%r~          | Mark all files with regexp in name                     |
;; | ~%C~          | Copy files whose names contains regexp                 |
;; | ~%H~          | Hardlink files whose names contans regexp              |
;; | ~%R~          | Rename files whose names contains regexp               |
;; | ~%S~          | Symlink files whose names contains regexp              |
;; | ~%&~          | Flag for deletion all 'garbage' files                  |
;; |               |                                                        |
;; | ~**~          | Mark executables                                       |
;; | ~*/~          | Mark directories                                       |
;; | ~*@~          | Mark symlinks                                          |
;; | ~*%~          | Mark all files matching regexp                         |
;; | ~*.~          | Mark all files with extension                          |
;; | ~*c~          | Change old mark to new marks                           |
;; | ~*s~          | Mark subdir files                                      |
;; | ~*m~          | Mark file at point                                     |
;; | ~*u~          | Unmark file at point                                   |
;; | ~*?~ ~*!~ ~U~ | Unmark all files                                       |
;; | ~* <delete>~  | Unmark backward                                        |
;; | ~* C-n~       | Next marked file                                       |
;; | ~* C-p~       | Previous marked file                                   |
;; | ~*t~          | Reverse marks                                          |
;; |               |                                                        |
;; | ~a~           | Visit file/directory under cursor                      |
;; | ~d~           | Flag file for deletion                                 |
;; | ~gf~ ~C-m~    | Visit file/directory on the current line               |
;; | ~gr~          | Revert buffer                                          |
;; | ~i~           | Edit with Wdired or make it read only                  |
;; | ~I~           | Insert this subdirectory into the same dired buffer    |
;; | ~J~           | GO to line describing file FILE in this Dired buffer   |
;; | ~K~           | Kill all marked lines (not files)                      |
;; | ~r~           | Redisplay all marked files                             |
;; | ~m~           | Mark the file at point in the Dired buffer             |
;; | ~t~           | Reverse mark                                           |
;; | ~u~           | Unmark file at point                                   |
;; | ~W~           | Open file in www browser                               |
;; | ~x~           | Delete the files flagged for deletion                  |
;; | ~gy~          | Print file type                                        |
;; | ~Y~           | Copy names of marked files into kill ring              |
;; | ~+~           | Create a directory                                     |
;; |               |                                                        |
;; | ~RET~         | Open file/directory                                    |
;; | ~S-RET~       | Open file/directory in other window                    |
;; | ~gO~          | Find file in other window                              |
;; | ~go~          | View file                                              |
;; |               |                                                        |
;; | ~o~           | Toggle sorting by date                                 |
;; |               |                                                        |
;; | ~gj~ ~]]~ ~>~ | Go to next directory line                              |
;; | ~gk~ ~[[~ ~<~ | Go to previous directory line                          |
;; | ~^~ ~-~       | Run dired on parent directory                          |
;; | ~SPC~         | Move down lines then position at filename              |
;; |               |                                                        |
;; | ~g$~          | (Un)hide the current subdir and move to next dir       |
;; | ~M-$~         | Hide all subdirs, leaving only their header lines      |
;; | ~(~           | Toggle visibility of detailed indo in current buffer   |
;; |               |                                                        |
;; | ~M-s a C-s~   | Search for a string through all marked files           |
;; | ~M-s a M-C-s~ | Search for a regexp through all marked files           |
;; | ~M-s f C-s~   | Search for a string only in file names                 |
;; | ~M-s f M-C-s~ | Search for a regexp only in file names                 |
;; |               |                                                        |
;; | ~g?~          | Summarize basic commands and show recent errors        |
;; | ~<delete>~    | Unmark backward                                        |
;; |               |                                                        |
;; | ~C-t d~       | Display thumbs of all marked files                     |
;; | ~C-t t~       | Tag marked files                                       |
;; | ~C-t r~       | Remove tag for selected files                          |
;; | ~C-t j~       | Jump to thumbnail buffer                               |
;; | ~C-t i~       | Display current image file                             |
;; | ~C-t x~       | Display file at point in external viewer               |
;; | ~C-t a~       | Append thumbs to thumb buffer                          |
;; | ~C-t .~       | Display thumb for current file                         |
;; | ~C-t c~       | Add comment to current of marked files                 |
;; | ~C-t f~       | Use regexp to mark files with matching tag             |
;; | ~C-t C-t~     | Toogle thumbs in front of file names                   |
;; | ~C-t e~       | Edit comment and tags of current or marked image files |
;; |               |                                                        |
;; | ~;d~          | Decrypt marked files                                   |
;; | ~;v~          | Verify marked files                                    |
;; | ~;s~          | Sign marked files                                      |
;; | ~;e~          | Encrypt marked files                                   |


(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-recursive-copy 'always)
  (dired-recursive-delete 'always))

;; Shows icons
(use-package all-the-icons-dired
  :diminish
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (with-no-warnings
    (defun my-all-the-icons-dired--display ()
      "Display the icons of files in a dired buffer."
      (when dired-subdir-alist
        (let ((inhibit-read-only t))
          ;; NOTE: don't display icons it too many items
          (if (<= (count-lines (point-min) (point-max)) 1000)
              (save-excursion
                ;; TRICK: Use TAB to align icons
                (setq-local tab-width 1)

                ;; Insert icons before the filenames
                (goto-char (point-min))
                (while (not (eobp))
                  (when (dired-move-to-filename nil)
                    (insert " ")
                    (let ((file (dired-get-filename 'verbatim t)))
                      (unless (member file '("." ".."))
                        (let ((filename (dired-get-filename nil t)))
                          (if (file-directory-p filename)
                              (insert (all-the-icons-icon-for-dir filename nil ""))
                            (insert (all-the-icons-icon-for-file file :v-adjust -0.05))))
                        ;; Align and keep one space for refeshing after some operations
                        (insert "\t "))))
                  (forward-line 1)))
            (message "Not display icons because of too many items.")))))
    (advice-add #'all-the-icons-dired--display
                :override #'my-all-the-icons-dired--display)))

(use-package dired-x
  :ensure nil
  :general
  (general-leader
    "ad" 'dired
    "fj" 'dired-jump
    "jd" 'dired-jump
    "jD" 'dired-jump-other-window)
  :commands (dired-jump
             dired-jump-other-window
             dired-omit-mode))

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

;; Delete selection

(use-package delsel
  :ensure nil
  :custom
  (delete-selection-mode t "Replace the active region just by typing text."))

;; Whitespaces
;; Delete trailing whitespaces on buffer save.


(use-package whitespace
  :ensure nil
  :diminish
  :commands (whitespace-mode global-whitespace-mode)
  :general
  (general-leader
   "t w"   '(whitespace-mode :wk "Whitespace mode")
   "t C-w" '(global-whitespace-mode :wk "Global whitespace mode")))

;; Move visual block
;; Move selection up and down.

;; | key | mode   | command                   |
;; |-----+--------+---------------------------|
;; | ~J~ | visual | Move selected block up.   |
;; | ~K~ | visual | Move selected block down. |


(general-vmap
  "J" (concat ":m '>+1" (kbd "RET") "gv=gv")
  "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))

;; Macros
;; Macros keybindings.

;; | key         | command                                        |
;; |-------------+------------------------------------------------|
;; | ~SPC K c a~ | Add the value of numeric prefix                |
;; | ~SPC K c c~ | Insert current value of counter then increment |
;; | ~SPC K c C~ | Set value of counter                           |
;; | ~SPC K c f~ | Set the format of counter                      |
;; | ~SPC K e b~ | Bind last macro to key                         |
;; | ~SPC K e e~ | Edit last keyboard macro                       |
;; | ~SPC K e l~ | Edit most recent 300 keystrokes as macro       |
;; | ~SPC K e n~ | Name last macro                                |
;; | ~SPC K e r~ | Store the last macro in register               |
;; | ~SPC K e s~ | Step edit and execute last macro               |
;; | ~SPC K k "~ | Record keyboard input                          |
;; | ~SPC K K "~ | End macro if currently defined or call last    |
;; | ~SPC K r L~ | Display current head of macro ring             |
;; | ~SPC K r d~ | Delete current macro from ring                 |
;; | ~SPC K r l~ | Execute second macro in macro ring             |
;; | ~SPC K r n~ | Move to next macro in macro ring               |
;; | ~SPC K r p~ | Move to prev macro in macro ring               |
;; | ~SPC K r s~ | Swap first two elements in macro ring          |
;; | ~SPC K v "~ | Display last macro"                            |


(general-leader
 "Kca" '(kmacro-add-counter :wk "Add the value of numeric prefix")
 "Kcc" '(kmacro-insert-counter :wk "Insert current value of counter then increment")
 "KcC" '(kmacro-set-counter :wk "Set value of counter")
 "Kcf" '(kmacro-set-format :wk "Set the format of counter")
 "Keb" '(kmacro-bind-to-key :wk "Bind last macro to key")
 "Kee" '(kmacro-edit-macro-repeat :wk "Edit last keyboard macro")
 "Kel" '(kmacro-edit-lossage :wk "Edit most recent 300 keystrokes as macro")
 "Ken" '(kmacro-name-last-macro :wk "Name last macro")
 "Ker" '(kmacro-to-register :wk "Store the last macro in register")
 "Kes" '(kmacro-step-edit-macro :wk "Step edit and execute last macro")
 "Kk"  '(kmacro-start-macro-or-insert-counter :wk "Record keyboard input")
 "KK"  '(kmacro-end-or-call-macro :wk "End macro if currently defined or call last")
 "KrL" '(kmacro-view-ring-2nd :wk "Display current head of macro ring")
 "Krd" '(kmacro-delete-ring-head :wk "Delete current macro from ring")
 "Krl" '(kmacro-call-ring-2nd-repeat :wk "Execute second macro in macro ring")
 "Krn" '(kmacro-cycle-ring-next :wk "Move to next macro in macro ring")
 "Krp" '(kmacro-cycle-ring-previous :wk "Move to prev macro in macro ring")
 "Krs" '(kmacro-swap-ring :wk "Swap first two elements in macro ring")
 "Kv"  '(kmacro-view-macro-repeat :wk "Display last macro"))

;; Rectangles

;; | key       | command                                                    |
;; |-----------+------------------------------------------------------------|
;; | ~SPC R !~ | Blank out the rect                                         |
;; | ~SPC R c~ | Delete all whitespace following column in each line        |
;; | ~SPC R d~ | Delete text in rect                                        |
;; | ~SPC R e~ | Cycles through the rect's corners                          |
;; | ~SPC R i~ | Copy rect into register                                    |
;; | ~SPC R k~ | Delete rect and save it as last killed one                 |
;; | ~SPC R l~ | Move N wide chars to the left                              |
;; | ~SPC R m~ | Toggle the region as rect                                  |
;; | ~SPC R n~ | Move N wide chars down                                     |
;; | ~SPC R N~ | Insert numbers in front of rect                            |
;; | ~SPC R o~ | Blank out rect, shifting text right                        |
;; | ~SPC R p~ | Move N wide chars up                                       |
;; | ~SPC R r~ | Move N wide chars to the right                             |
;; | ~SPC R s~ | Replace rect contents with string on each line             |
;; | ~SPC R t~ | Transpose region                                           |
;; | ~SPC R y~ | Yank the last killed rect with upper left corner at point" |


(general-leader
  "R!" '(clear-rectangle :wk "Blank out the rect")
  "Rc" '(close-rectangle :wk "Delete all whitespace following column in each line")
  "Rd" '(delete-rectangle :wk "Delete text in rect")
  "Re" '(rectangle-exchange-point-and-mark :wk "Cycles through the rect's corners")
  "Ri" '(copy-rectangle-to-register :wk "Copy rect into register")
  "Rk" '(kill-rectangle :wk "Delete rect and save it as last killed one")
  "Rl" '(rectangle-left-char :wk "Move N wide chars to the left")
  "Rm" '(rectangle-mark-mode :wk "Toggle the region as rect")
  "Rn" '(rectangle-next-line :wk "Move N wide chars down")
  "RN" '(rectangle-number-lines :wk "Insert numbers in front of rect")
  "Ro" '(open-rectangle :wk "Blank out rect, shifting text right")
  "Rp" '(rectangle-previous-line :wk "Move N wide chars up")
  "Rr" '(rectangle-right-char :wk "Move N wide chars to the right")
  "Rs" '(string-rectangle :wk "Replace rect contents with string on each line")
  "Rt" '(transpose-regions :wk "Transpose region")
  "Ry" '(yank-rectangle :wk "Yank the last killed rect with upper left corner at point"))

;; On-the-fly spell checker
;; On the fly spell checking. Disabled by default. =hunspell= is must because of ability to query multiple dictionaries.

;; Related keybindings:
;; | key       | mode   | command                                                     |
;; |-----------+--------+-------------------------------------------------------------|
;; | ~SPC t S~ | global | Toggle flyspell on/off                                      |
;; | ~SPC S b~ | global | Spell check whole buffer.                                   |
;; | ~z=~      | normal | Correct previous word (ivy menu, access commands by =M-o=). |
;; | ~[s~      | normal | Jump to the previous incorrect word.                        |
;; | ~]s~      | normal | Jump to the next incorrect word.                            |


(use-package flyspell
  :ensure nil
  :diminish
  :commands
  (flyspell-mode flyspell-prog-mode flyspell-buffer flyspell-correct-wrapper)
  :functions knopki/flyspell-or-flyspell-prog-mode
  :preface
  (defun knopki/flyspell-or-flyspell-prog-mode ()
    "Enable/disable flyspell-mode or flyspell-prog-mode"
    (interactive)
    (if (eq flyspell-mode nil)
        (if (derived-mode-p 'prog-mode) (flyspell-prog-mode) (flyspell-mode))
      (flyspell-mode-off)))
  :general
  (general-leader
    "t S" 'knopki/flyspell-or-flyspell-prog-mode
    "S b" 'flyspell-buffer)
  :if (executable-find "hunspell")
  :init
  (with-eval-after-load "ispell"
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US,ru_RU")
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,ru_RU"))
  :custom
  (flyspell-issue-message-flag nil "Be silent."))



;; Correcting words with flyspell via Ivy.

(use-package flyspell-correct-ivy
  :after (:all (flyspell ivy))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :general
  ;; Redefine evil-mode keybinding
  ;; Also, use M-o to access ivy menu
  (general-nmap "z=" 'flyspell-correct-wrapper))

;; Undo Tree
;; Treat undo history as a branching tree of changes.

;; =undo-tree-mode= and =global-undo-tree-mode= keybindings:
;; | key               | command                               |
;; |-------------------+---------------------------------------|
;; | ~SPC a u~ ~C-x u~ | Run Undo Tree Visualizer              |
;; | ~C-_~ ~C-/~       | Undo tree undo                        |
;; | ~M-_~ ~M-/~       | Undo tree redo                        |
;; | ~C-x r u~         | Save current buffer state to register |
;; | ~C-x r U~         | Load buffer state from register       |

;; =undo-tree-visualizer= keybindings:
;; | key                    | command                                |
;; |------------------------+----------------------------------------|
;; | ~<up>~ ~k~ ~p~ ~C-p~   | Undo changes                           |
;; | ~<down>~ ~j~ ~n~ ~C-n~ | Redo changes                           |
;; | ~<left>~ ~h~ ~b~ ~C-b~ | Switch to previous undo-tree branch    |
;; | ~<right> ~l~ ~f~ ~C-f~ | Switch to next undo-tree branch        |
;; | ~C-<up>~ ~M-{~         | Undo changes up to last branch point   |
;; | ~C-<down>~ ~M-}~       | Redo changes down to next branch point |
;; | ~t~                    | Toggle display of time-stamps          |
;; | ~d~                    | Toggle diff display                    |
;; | ~s~                    | Toggle keyboard selection mode         |
;; | ~q~                    | Quit undo-tree-visualizer              |
;; | ~C-q~                  | Abort undo-tree-visualizer             |
;; | ~<~                    | Scroll left                            |
;; | ~>~                    | Scroll right                           |
;; | ~<pgup>~ ~M-v~         | Scroll up                              |
;; | ~<pgdown>~ ~C-v~       | Scroll down                            |


(use-package undo-tree
  :diminish
  :hook (after-find-file . global-undo-tree-mode)
  :general
  (general-leader
    "au" 'undo-tree-visualize)
  :custom
  (undo-tree-visualizer-timestamps t "Display timestamps.")
  (undo-tree-enable-undo-in-region nil "Do not undo changes only in region.")
  (undo-tree-visualizer-lazy-drawing 100 "Switch too lazy drawing after N nodes.")
  (undo-tree-auto-save-history t "Save history to file."))

;; Core

(use-package org
  :ensure org-plus-contrib
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-indent-mode . (lambda () (diminish 'org-indent-mode)))
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :general
  (general-leader
    "o a" 'org-agenda
    "o c" 'counsel-org-capture
    "o l" 'org-agenda-list
    "o s" 'org-search-view)
  :custom
  (org-modules '(org-checklist org-habit) "Always load modules.")

  (org-directory "~/org" "Directory with org files.")
  (org-log-done 'time "Record time when task moves to the DONE state.")
  (org-log-redeadline 'time "Record time when task deadline changes.")
  (org-log-reschedule 'time "Record time when task rescheduled.")
  (org-log-into-drawer t "Insert changes notes and time into a drawer.")
  (org-catch-invisible-edits 'smart "Check for invisible region before edit.")
  (org-startup-indented t "Turn on indent mode on startup.")
  (org-pretty-entities t "Show entities as UTF8 characters.")
  (org-src-window-setup 'current-window "Show edit buffer in the current window.")
  (org-enforce-todo-dependencies t "Undone tasks will block parent from DONE.")
  (org-enforce-todo-checkbox-dependencies t "Unchecked boxes will block parent from DONE.")
  (org-archive-location
   (concat org-directory "/archive/%s_archive::datetree/")
   "The location where subtree should be archived.")

  ;; Keywords
  (org-todo-keywords
   '((sequence "TODO(t!)" "NEXT(n)" "WIP(i!)" "WAITING(w@/!)"
               "GAVE(g!)" "|" "DONE(d!)" "CANCELLED(c@/!)"))
   "List of keywords sequences and their interpretation.")
  (org-todo-keyword-faces
   '(("TODO"     . org-todo)
     ("NEXT"     . org-warning)
     ("WIP"      . (:foreground "OrangeRed" :weight bold))
     ("WAITING"  . (:foreground "coral" :weight bold))
     ("GAVE"     . (:foreground "LimeGreen" :weight bold))
     ("CANCELED" . org-done)
     ("DONE"     . org-done))
   "Faces for specific keywords.")

  ;; Priorities
  (org-lowest-priority ?D "The lowest priority of items.")
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)
                        (?D . normal))
                      "Faces for specific Priorities.")

  ;; Tags
  (org-tags-exclude-from-inheritance
   '(olga)
   "List of tags that should never be inherited.")

  ;; Agenda
  (org-agenda-files '("~/org") "The files to be used for agenda display.")
  (org-agenda-text-search-extra-files '('agenda-archives) "Extra searched files.")
  (org-agenda-span 14 "Number of days to include in agenda overview.")
  (org-agenda-start-on-weekday nil "Start overview on today.")
  (org-agenda-start-day "-3d" "Show previous days in overview.")
  (org-agenda-skip-deadline-prewarning-if-scheduled
   t "Skip deadline prewarning when already scheduled.")
  (org-agenda-skip-scheduled-if-deadline-is-shown t "Skip scheduling line if deadline.")
  (org-agenda-include-diary t "Include diary entries like calendar.")
  (org-stuck-projects
   '("+projects/-DONE" ("NEXT" "WIP") ("@shop") "\\<IGNORE\\>")
   "How to identify stuck projects.")

  ;; Capture
  (org-default-notes-file
   (expand-file-name "capture.org" org-directory)
   "Default target for storing notes.")
  (org-capture-templates
   '(("t" "My TODO task" entry
      (file "capture.org")
      "* TODO  %?\nSCHEDULED: %t"))
   "Capture templates.")

  ;; Refile
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3))
                      "Targets for refiling.")
  (org-refile-use-cache t "Cache refile targets.")

  ;; Attachments
  (org-attach-archive-delete file 'query "Ask for attachment delete on node delete.")

  ;; Clock
  (org-clock-persist 'history "Persist clock history.")

  ;; org-goto/ivy interplay hack
  (org-goto-interface 'outline-path-completion "For ivy integration.")
  (org-outline-path-complete-in-steps nil "For ivy integration.")

  ;; Babel
  (org-confirm-babel-evaluate nil "Evaluate babel without confirmation.")
  (org-src-fontify-natively t "Fontify code in code blocks.")
  (org-src-tab-acts-natively t "Indent on tab in code blocks like in code.")
  (org-babel-load-languages '((emacs-lisp . t)
                              (org . nil)
                              (python . nil)
                              (sh . nil)))
  :config
  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Autosave (no sure is it worth it)
  (run-with-idle-timer 30 t 'org-save-all-org-buffers)

  ;; Set up hooks for clock persistence.
  (org-clock-persistence-insinuate))

;; Evil
;; Evil support in org-mode.


(use-package evil-org
  :diminish
  :after (:all (org evil))
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . (lambda () (evil-org-set-key-theme)))
  (org-agenda-mode . (lambda ()
                       (evil-org-mode)
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys)))
  :general
  (:keymaps 'org-src-mode-map [remap evil-write] 'org-edit-src-save)
  :custom
  (evil-org-key-theme
   '(navigation insert return textobjects additional shift todo heading calendar)
   "Key themes to enable.")
  (evil-org-retain-visual-state-on-shift t "<> should retain selection in visual mode."))

;; Org Bullets
;; Replace bullets with unicode characters.


(use-package org-bullets
  :diminish
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-ellipsis "…"))

;; Fancy Priorities
;; Displays org priorities as custom strings.


(use-package org-fancy-priorities
  :diminish
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list
   (if (char-displayable-p ?⚡)
       '("⚡" "⬆" "⬇" "☕")
     '("HI" "MID" "LOW" "OPT"))))

;; Table of Contents
;; Generates an up-to-date table of contents in org files without exporting.


(use-package toc-org
  :hook
  (org-mode . toc-org-mode)
  ;; In markdown too.
  (markdown-mode . toc-org-mode))

;; diff-hl
;; Highlights uncommitted changes on the fringe.


(use-package diff-hl
  :defer t
  :hook
  ((prog-mode org-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

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
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

;; Magit
;; Awesome git frontend.


(use-package magit
  :commands (magit-file-delete)
  :general
  (general-leader
    "g" 'magit-status)
  :custom
  (magit-diff-toggle-refine-hunk t "Show word-granularity differences within diff hunks.")
  (magit-prefer-remote-upstream t "Favor remote branches when reading upstream branch.")
  (magit-completing-read-function
   'ivy-completing-read "Called when requested user input."))

;; Make Magit Evil
;; TODO: remove me after merge of https://github.com/emacs-evil/evil-collection/pull/259
(use-package evil-magit
  :after (:all (magit evil)))

;; Magit TODO
;; Show source files' TODOs (and FIXMEs, etc) in Magit status buffer.


(use-package magit-todos
  :commands (ivy-magit-todos)
  ;; TODO: add keybindings
  :hook (magit-mode . magit-todos-mode)
  ;; TODO: remove after evil-collection updated with fix
  :general
  (:keymaps '(magit-todos-section-map magit-todos-item-section-map)
            "jT" nil
            "jl" nil
            "j" nil))

;; Git Time Machine
;; Walk through git revisions of a file.


(use-package git-timemachine
  :commands (git-timemachine)
  ;; TODO: setup keybindings
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer")))))

;; Browse at remote
;; Open github/gitlab/bitbucket page.


(use-package browse-at-remote
  :commands (browse-at-remote browse-at-remote-kill)
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote)))

;; Git related modes

(use-package gitattributes-mode
  :mode ("/\\.gitattributes\\'"
         "/info/attributes\\'"
         "/git/attributes\\'"))
(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'"
         "/\\.git/config\\'"
         "/modules/.*/config\\'"
         "/git/config\\'"
         "/\\.gitmodules\\'"
         "/etc/gitconfig\\'"))
(use-package gitignore-mode
  :mode ("/\\.gitignore\\'"
         "/info/exclude\\'"
         "/git/ignore\\'"))

;; Eldoc

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :defer 2
  :hook
  ((prog-mode eval-expression-minibuffer-setup-hook) . eldoc-mode)
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
  :defer 2
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-mode t "Enable show matching parens."))

;; Automatic parenthesis pairing

(use-package elec-pair
  :ensure nil
  :defer 2
  :hook (prog-mode . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Display line numbers

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

;; Highlight TODOs
;; Highlight TODO and similar keywords in comments and strings.


(use-package hl-todo
  :defer 2
  :hook (prog-mode . hl-todo-mode))

;; Evil commentary
;; =gc= operator, like =vim-commentary=.
;; TODO: maybe switch to evil-nerd-commenter

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

;; Aggressive Indent
;; Minor mode to aggressively keep your code always indented.


(use-package aggressive-indent
  :diminish
  :defer 2
  :hook ((prog-mode . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode prolog-inferior-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Flycheck
;; On-the-fly syntax checker.

;; Global keybindings:
;; | key       | command                         |
;; |-----------+---------------------------------|
;; | ~SPC t s~ | Toggle flycheck mode            |
;; | ~SPC e b~ | Check for errors                |
;; | ~SPC e c~ | Clear errors                    |
;; | ~SPC e h~ | Describe checker                |
;; | ~SPC e l~ | Display list of errors          |
;; | ~SPC e s~ | Set flycheck checker            |
;; | ~SPC e S~ | Set flycheck checker executable |
;; | ~SPC e v~ | Verify flycheck setup           |
;; | ~SPC e y~ | Copy error to kill-ring         |
;; | ~SPC e x~ | Explain error at point          |

;; Flycheck error list mode keybindings:
;; | key                 | command        |
;; |---------------------+----------------|
;; | ~gj~ / ~C-j~ / ~]]~ | Next error     |
;; | ~gk~ / ~C-k~ / ~[[~ | Previous error |
;; | ~gr~                | Check source   |
;; | ~s~                 | Set filter     |
;; | ~S~                 | Reset filter   |
;; | ~x~                 | Explain error  |
;; | ~RET~               | Go to error    |
;; | ~q~                 | Quit window    |


(use-package flycheck
  :hook (after-find-file . global-flycheck-mode)
  :general
  (general-leader
    "ts" '(flycheck-mode :wk "Toggle flycheck mode")
    "eb" '(flycheck-buffer :wk "Check for errors")
    "ec" '(flycheck-clear :wk "Clear errors")
    "eh" '(flycheck-describe-checker :wk "Describe checker")
    "el" '(list-flycheck-errors :wk "Display list of errors")
    "es" '(flycheck-select-checker :wk "Set flycheck checker")
    "eS" '(flycheck-set-checker-executable :wk "Set flycheck checker executable")
    "ev" '(flycheck-verify-setup :wk "Verify flycheck setup")
    "ey" '(flycheck-copy-errors-as-kill :wk "Copy error to kill-ring")
    "ex" '(flycheck-explain-error-at-point :wk "Explain error at point"))
  ;; t s
  :custom
  (flycheck-emacs-lisp-load-path 'inherit "Inherit load paths from Emacs.")
  (flycheck-global-modes
   '(not org-mode text-mode outline-mode fundamental-mode
         shell-mode eshell-mode term-mode vterm-mode)
   "Disable checking in some modes.")
  (flycheck-check-syntax-automatically
   '(idle-change mode-enabled save) "Run checks only on this events.")
  (flycheck-idle-change-delay 4 "Idle delay before run checks."))

;; Snippets
;; =yasnippet= for snippets.


(use-package yasnippet
  :diminish yas-minor-mode
  :defer 2
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file)
  :hook ((text-mode prog-mode conf-mode snippet-mode) . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)



;; Integrate =yasnippet= into =ivy=.


(use-package ivy-yasnippet
  :commands ivy-yasnippet--preview
  :general
  (general-leader "y" 'ivy-yasnippet)
  :config
  (advice-add #'ivy-yasnippet--preview :override #'ignore))

;; direnv
;; Switch project environment on buffer switch. Sometimes switching is slow.


(use-package direnv
  ;; Ensures that external dependencies are available before they are called.
  :hook
  ((find-file
    flycheck-before-syntax-check
    before-hack-local-variables) . direnv-update-environment)
  :custom
  (direnv-always-show-summary nil)
  :config
  (direnv-mode)
  (advice-add 'prog-mode :before #'direnv-update-environment))

;; LSP
;; Language Server Protocol Support for Emacs

;; LSP mode keybindings:
;; | key     | command                     |
;; |---------+-----------------------------|
;; | ~​, s s~ | Start server                |
;; | ~​, s r~ | Restart server              |
;; | ~​, s q~ | Shutdown server             |
;; | ~​, s d~ | Describe session            |
;; | ~​, s D~ | Disconnect                  |
;; |         |                             |
;; | ~​, = =~ | Format buffer               |
;; | ~​, = r~ | Format region               |
;; |         |                             |
;; | ~​, F a~ | Add folder                  |
;; | ~​, F r~ | Remove folder               |
;; | ~​, F b~ | Un-blacklist folder         |
;; |         |                             |
;; | ~​, T l~ | Toggle lenses               |
;; | ~​, T L~ | Toggle log io               |
;; | ~​, T h~ | Toggle highlighting         |
;; | ~​, T s~ | Toggle signatures           |
;; | ~​, T S~ | Toggle sideline             |
;; | ~​, T d~ | Toddle documentation popup  |
;; | ~​, T p~ | Toggle signature help       |
;; | ~​, T f~ | Toggle on type formatting   |
;; | ~​, T T~ | Toggle treemacs integration |
;; |         |                             |
;; | ~​, g g~ | Find definition             |
;; | ~​, g r~ | Find references             |
;; | ~​, g i~ | Find implementations        |
;; | ~​, g d~ | Find declarations           |
;; | ~​, g t~ | Find type definitions       |
;; | ~​, g h~ | Call hierarchy              |
;; | ~​, g a~ | Find symbol in workspace    |
;; | ~​, g M~ | Show navigation menu        |
;; | ~​, g e~ | Show flycheck errors        |
;; |         |                             |
;; | ~​, h h~ | Describe symbol at point    |
;; | ~​, h s~ | Signature help              |
;; | ~​, h g~ | Doc popup                   |
;; |         |                             |
;; | ~​, r r~ | Rename                      |
;; | ~​, r o~ | Organize imports            |
;; |         |                             |
;; | ~​, a a~ | Code actions                |
;; | ~​, a l~ | Lens                        |
;; | ~​, a h~ | Highlight symbol            |
;; |         |                             |
;; | ~​, G g~ | Peek definitions            |
;; | ~​, G r~ | Peek references             |
;; | ~​, G i~ | Peek implementations        |
;; | ~​, G s~ | Peek workspace symbol       |
;; | ~​, G N~ | Peek jump backward          |
;; | ~​, G n~ | Peek jump forward           |


(use-package lsp-mode
  ;; TODO: enable after upgrade
  ;; :hook (lsp-mode . lsp-enable-which-key-integration)
  :commands (lsp lsp-deffered)
  :general
  ;; TODO: merge with =lsp-command-map= after upgrade
  (general-major-leader
    :keymaps 'lsp-mode-map
    ;; sessions
    "s"  '(nil :wk "Sessions")
    "ss" '(lsp :wk "Start server")
    "sr" '(lsp-workspace-restart :wk "Restart server")
    "sq" '(lsp-workspace-shutdown :wk "Shutdown server")
    "sd" '(lsp-describe-session :wk "Describe session")
    "sD" '(lsp-disconnect :wk "Disconnect")

    ;; formatting
    "="  '(nil :wk "Formatting")
    "==" '((lambda ()
              (interactive)
              (cond
               ((derived-mode-p 'python-mode) (python-black-buffer))
               (lsp-format-buffer))) :wk "Format buffer")
    "=r" '(lsp-format-region :wk "Format region")

    ;; folders
    "F"  '(nil :wk "Folders")
    "Fa" '(lsp-workspace-folders-add :wk "Add folder")
    "Fr" '(lsp-workspace-folders-remove :wk "Remove folder")
    "Fb" '(lsp-workspace-blacklist-remove :wk "Un-blacklist folder")

    ;; toggle
    "T"  '(nil :wk "Toggle")
    "Tl" '(lsp-lens-mode :wk "Toggle lenses")
    "TL" '(lsp-toggle-trace-io :wk "Toggle log io")
    "Th" '(lsp-toggle-symbol-highlight :wk "Toggle highlighting")
    "Ts" '(lsp-toggle-signature-auto-activate :wk "Toggle signature")
    "TS" '(lsp-ui-sideline-mode :wk "Toggle sideline")
    "Td" '(lsp-ui-doc-mode :wk "Toggle documentation popup")
    "Tp" '(lsp-signature-mode :wk "Toggle signature help")
    "Tf" '(lsp-toggle-on-type-formatting :wk "Toggle on type formatting")
    "TT" '(lsp-treemacs-sync-mode :wk "Toggle treemacs integration")

    ;; goto
    "g"  '(nil :wk "Go To")
    "gg" '(lsp-find-definition :wk "Find definition")
    "gr" '(lsp-find-references :wk "Find references")
    "gi" '(lsp-find-implementation :wk "Find implementations")
    "gd" '(lsp-find-declaration :wk "Find declarations")
    "gt" '(lsp-find-type-definition :wl "Find type definition")
    "gh" '(lsp-treemacs-call-hierarchy :wk "Call hierarchy")
    "ga" '(xref-find-apropos :wk "Find symbol in workspace")
    "gM" '(lsp-ui-imenu :wk "Show navigation menu")
    "ge" '(lsp-ui-flycheck-list :wk "Show flyckeck errors")

    ;; help
    "h"  '(nil :wk "Help")
    "hh" '(lsp-describe-thing-at-point :wk "Describe symbol at point")
    "hs" '(lsp-signature-activate :wk "Signature help")
    "hg" '(lsp-ui-doc-glance :wk "Doc popup")

    ;; refactor
    "r"  '(nil :wk "Refactoring")
    "rr" '(lsp-rename :wk "Rename")
    "ro" '(lsp-organize-imports :wk "Organize imports")

    ;; actions
    "a"  '(nil :wk "Code actions")
    "aa" '(lsp-execute-code-action :wk "Code actions")
    "al" '(lsp-avy-lens :wk "Lens")
    "ah" '(lsp-document-highlight :wk "Highlight symbol")

    ;; peek
    "G"  '(nil :wk "Peek")
    "Gg" '(lsp-ui-peek-find-definitions :wk "Peek definitions")
    "Gr" '(lsp-ui-peek-find-references :wk "Peek references")
    "Gi" '(lsp-ui-peek-find-implementation :wk "Peek implementations")
    "Gs" '(lsp-ui-peek-find-workspace-symbol :wk "Peek workspace symbol")
    "GN" '(lsp-ui-peek-jump-backward :wk "Jump backward")
    "Gn" '(lsp-ui-peek-jump-forward :wk "Jump forward"))
  :custom
  (read-process-output-max (* 1024 1024) "Performace.")
  (lsp-auto-guess-root t "Detect project root.")
  (lsp-keep-workspace-alive nil "Auto-kill LSP server.")
  (lsp-diagnostic-package :flycheck)

  (lsp-pyls-configuration-sources ["flake8"])
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  (lsp-pyls-plugins-yapf-enabled nil)

  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_mypy.strict" t t))))



;; Nice UI


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :general
  (:keymaps 'lsp-ui-peek-mode-map
            "h" 'lsp-ui-peek--select-prev-file
            "j" 'lsp-ui-peek--select-next
            "k" 'lsp-ui-peek--select-prev
            "l" 'lsp-ui-peek--select-next-file))



;; Company integration.


(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates 'auto))



;; Ivy integration.


(use-package lsp-ivy
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

;; Emacs Lisp

(use-package elisp-mode
  :ensure nil
  :hook
  (emacs-lisp-mode . (lambda ()
                       (set (make-local-variable 'company-backends)
                            '((company-capf
                               company-files
                               company-yasnippet
                               company-dabbrev-code)))
                       company-dabbrev-code
                       (company-mode t))))

;; C/C++

(use-package cc-vars
  :ensure nil
  :defer t
  :custom
  (c-basic-offset 4 "Default indentation.")
  (c-default-style '((java-mode . "java")
                     (awk-mode . "awk")
                     (other . "k&r"))))

;; Nix

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :hook (nix-mode . (lambda () (electric-indent-local-mode -1)))
  :custom
  (nix-indent-function 'smie-indent-line))


(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")


(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))


(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))


(use-package nix-format
  :ensure nix-mode
  :custom
  (nix-nixfmt-bin "nixpkgs-fmt" "Use nixpkgs-fmt instead of nixfmt-bin.")
  :commands (nix-format-buffer))

;; Python

(use-package python
  :ensure nil
  :defer t
  :hook (python-mode . lsp-deferred))



;; Live coding in Python.


(use-package live-py-mode
  :commands (live-py-mode))



;; Format buffer with =black=.


(use-package python-black
  :hook (python-mode . python-black-on-save-mode))

;; Javascript

(use-package js
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2))

;; The end…
;; Add standard module footer.


(provide 'init)
;;; init.el ends here
