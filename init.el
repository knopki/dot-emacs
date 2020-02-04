;; -*- lexical-binding: t -*-
;; This file was tangled (automatically generated) from `README.org'

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

;; Performance hacks
;; Default garbage collection thresholds.


(defvar default-gc-cons-percentage gc-cons-percentage)
(defconst knopki/gc-cons-threshold-bytes
  16777216 ; 16Mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")



;; A big contributor to startup times is garbage collection. We up the garbage
;; collector threshold to temporarily prevent it from running, then reset it
;; later. Not resetting it will cause stuttering/freezes. Also, disable GC when
;; cursor in the any minibuffer and restore on minibuffer exit.


(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 100)

;; Startup hook
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore default values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold knopki/gc-cons-threshold-bytes
                  gc-cons-percentage default-gc-cons-percentage)))

;; Stop GC in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum
                  gc-cons-percentage 100)))

;; GC and back GC threshold to normal on minibuffer exit
(add-hook 'minibuffer-exit-hook
          (lambda ()
            (garbage-collect)
            (setq gc-cons-threshold knopki/gc-cons-threshold-bytes
                  gc-cons-percentage default-gc-cons-percentage)))



;; Run GC every 10s when idle.


(run-with-idle-timer 10 t #'garbage-collect)



;; Collect garbage automatically while unfocusing the frame.
;; =focus-out-hook= is obsolete since 27.1.


(if (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
  (add-hook 'focus-out-hook 'garbage-collect))



;; In noninteractive sessions, prioritize non-byte-compiled source files to prevent
;; the use of stale byte-code. Otherwise, it saves us a little IO time to skip the
;; =mtime= checks on every *.elc file we load.


(setq load-prefer-newer noninteractive)



;; This is consulted on every =require=, =load= and various path/io functions. You
;; get a minor speed up by nooping this.


(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)



;; Don't make a second case-insensitive pass over =auto-mode-alist=. If it has to,
;; it's our (the user's) failure. One case for all!


(setq auto-mode-case-fold nil)



;; Don't ping things that look like domain names.


(setq ffap-machine-p-known 'reject)



;; Remove command line options that aren't relevant to our current OS; that means
;; less to process at startup.


(unless (eq system-type 'darwin) (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux) (setq command-line-x-option-alist nil))



;; Don’t compact font caches during garbage collect.


(setq inhibit-compacting-font-caches t)



;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use fonts
;; that are larger than the system default (which would resize the frame).


(setq frame-inhibit-implied-resize t)



;; Disable bidirectional text rendering for a modest performance boost. No RTL, oops.


(setq-default bidi-display-reordering 'left-to-right)



;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions in
;; non-focused windows.


(setq-default cursor-in-non-selected-windows nil)

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
(setq package-archives
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


(require 'no-littering)

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
  (initial-major-mode 'text-mode "It just text by default."))

;; Tabify
;; Buffer re-tabbing.


(use-package tabify
  :ensure nil
  :commands (tabify untabify)
  :config
  (setq tabify-regexp "^\t* [ \t]+"))

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

;; Menu/Tool/Scroll bars

(unless (>= emacs-major-version 27)       ; Move to early init-file in 27
  (unless (and (display-graphic-p) (eq system-type 'darwin))
    (push '(menu-bar-lines . 0) default-frame-alist))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))

;; Fonts

(defconst knopki/font-default
  "FuraCode Nerd Font Mono"
  "Default font face.")

(when (display-graphic-p)
  ;; Set default font
  (when (member knopki/font-default (font-family-list))
    (set-face-attribute 'default nil
                        :font knopki/font-default
                        :height (cond
                                 ((and (display-graphic-p) (eq system-type 'darwin)) 130)
                                 ((eq system-type 'windows-nt) 110)
                                 (t 120)))))

;; Old init file

;; Setup builtin settings and packages
(require 'init-base)

;; Essential look & feel (doomed)
(require 'init-doom-themes)
(require 'init-doom-modeline)
(require 'init-all-the-icons)
(require 'init-dashboard)
(require 'init-hide-mode-line)
(require 'init-solaire-mode)

;; Create Vi-macs homunculus
(require 'init-evil)
(require 'init-general)
(require 'init-reverse-im)

;; Global modes
(require 'init-which-key)
(require 'init-undo-tree)
(require 'init-ivy)
(require 'init-flycheck)
(require 'init-avy)
(require 'init-persistent-scratch)

;; Sometimes modes
(require 'init-ibuffer)
(require 'init-org)

;; Programming
(require 'init-projectile)
(require 'init-company)
(require 'init-aggressive-indent)
(require 'init-magit)
(require 'init-diff-hl)
(require 'init-yasnippet)
(require 'init-direnv)
(require 'init-nix)


;; Load manual customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
