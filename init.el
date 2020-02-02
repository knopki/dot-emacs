;; -*- lexical-binding: t -*-
;; This file was tangled (automatically generated) from `README.org'

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

(defvar default-gc-cons-percentage gc-cons-percentage)
(defconst knopki/gc-cons-threshold-bytes
  16777216 ; 16Mb
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

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

(run-with-idle-timer 10 t #'garbage-collect)

(if (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
  (add-hook 'focus-out-hook 'garbage-collect))

(setq load-prefer-newer noninteractive)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq auto-mode-case-fold nil)

(setq ffap-machine-p-known 'reject)

(unless (eq system-type 'darwin) (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux) (setq command-line-x-option-alist nil))

(setq inhibit-compacting-font-caches t)

(setq frame-inhibit-implied-resize t)

(setq-default bidi-display-reordering 'left-to-right)

(setq-default cursor-in-non-selected-windows nil)

(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(require 'package)
(setq package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")))

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

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

(use-package gnu-elpa-keyring-update)

(when (getenv "EMACS_BENCHMARK")
  (use-package benchmark-init
    :defines swiper-font-lock-exclude
    :commands (benchmark-init/activate)
    :hook (after-init . benchmark-init/deactivate)
    :init (benchmark-init/activate)
    :config
    (with-eval-after-load 'swiper
      (add-to-list 'swiper-font-lock-exclude 'benchmark-init/tree-mode))))

(require 'no-littering)

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
