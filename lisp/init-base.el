;;; init-base.el --- Better default configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-const))

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
(unless (eq system-type 'windows-nt)
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Typing yes/no is obnoxious when y/n will do
(fset #'yes-or-no-p #'y-or-n-p)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default tab-width        4
              indent-tabs-mode nil
              fill-column 80)

(setq initial-scratch-message nil
      initial-major-mode 'text-mode)

;; Favor hard-wrapping in text modes
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Deleting files go to OS's trash folder
(setq delete-by-moving-to-trash t)


;; Menu/Tool/Scroll bars
(unless (>= emacs-major-version 27)       ; Move to early init-file in 27
  (unless (and (display-graphic-p) (eq system-type 'darwin))
    (push '(menu-bar-lines . 0) default-frame-alist))
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist))


;; Display the bare minimum at startup. We don't need all that noise. The
;; dashboard/empty scratch buffer is good enough.
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(fset #'display-startup-echo-area-message #'ignore)


;; Mouse & Smooth Scroll
(setq hscroll-step 1
      scroll-step 1
      scroll-margin 5
      scroll-conservatively 10
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil)


;; Fonts
(when (display-graphic-p)
  ;; Set default font
  (when (member knopki/font-default (font-family-list))
    (set-face-attribute 'default nil
                        :font knopki/font-default
                        :height (cond
                                 ((and (display-graphic-p) (eq system-type 'darwin)) 130)
                                 ((eq system-type 'windows-nt) 110)
                                 (t 120)))))

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
;; They're generally unhelpful and only add confusing visual clutter.
(setq mode-line-default-help-echo nil
      show-help-function nil)

;; Don't make cursor very visible
(setq visible-cursor nil)

;; Flash frame to represent a bell
(setq visible-bell t)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Don't resize emacs in steps, it looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Favor vertical splits over horizontal ones
(setq split-height-threshold nil)

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)


;;
;; Base modules
;;
(use-package tabify
  :ensure nil
  :defer t
  :config
  ;; for :retab
  (setq tabify-regexp "^\t* [ \t]+"))


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
  (setq confirm-kill-processes nil
        require-final-newline t
        confirm-nonexistent-file-or-buffer t)

  ;; Backups
  (setq backup-enable-predicate #'my-backup-enable-predicate)
  (setq delete-old-versions -1)
  (setq version-control t)
  (setq vc-make-backup-files t)

  ;; Autosaves
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; Don't kill *Scratch*!
  (with-current-buffer
      (get-buffer "*scratch*")
    (add-hook 'kill-buffer-hook
              (lambda () (error "DENIED! don't kill my *scratch*!"))
              nil t)))


(use-package saveplace
  :ensure nil
  :defer t
  :config
  (save-place-mode 1))


(use-package advice
  :ensure nil
  :defer t
  :config
  ;; Disable warnings from legacy advice system.
  ;; They aren't useful, and we can't often do anything about them
  ;; besides changing packages upstream
  (setq ad-redefinition-action 'accept))


(use-package apropos
  :ensure nil
  :defer t
  :config
  ;; Make apropos omnipotent. It's more useful this way.
  (setq apropos-do-all t))


(use-package simple
  :ensure nil
  :hook
  (window-setup . size-indication-mode)
  :init
  (setq column-number-mode t
        line-number-mode t
        line-move-visual nil            ; Keep cursor at end of lines
        track-eol t                     ; Repeating C-SPC after popping mark pops it again
        set-mark-command-repeat-pop t)

  ;; Eliminate duplicates in the kill ring. That is, if you kill the
  ;; same thing twice, you won't have to use M-y twice to get past it
  ;; to older entries in the kill ring.
  (setq kill-do-not-save-duplicates t)

  ;; Save clipboard contents into kill-ring before replacing them
  (setq save-interprogram-paste-before-kill t)

  ;; Don't blink the paren matching the one at point, it's too distracting.
  (setq blink-matching-paren nil))


(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup))


(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (run-at-time t (* 5 60) 'recentf-save-list)
  :config
  (add-to-list 'recentf-exclude (expand-file-name recentf-save-file))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        history-delete-duplicates t
        savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)
        savehist-autosave-interval 300))


(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))


;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook
  ;; show org ediffs unfolded
  ((ediff-prepare-buffer . outline-show-all)
   ;; restore window layout when done
   (ediff-quit . winner-undo))

  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-multiframe))


;; Automatic parenthesis pairing.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package paren
  :ensure nil
  :defer t
  :config
  (setq show-paren-delay 0)
  (show-paren-mode +1))


;; On-the-fly spell checker.
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "hunspell")
  :hook
  (((text-mode outline-mode org-mode) . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :init
  (setq flyspell-issue-message-flag nil)
  (with-eval-after-load "ispell"
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US,ru_RU")
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,ru_RU")))


(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook
  (prog-mode . eldoc-mode)
  :config
  (global-eldoc-mode -1)
  (setq eldoc-idle-delay 0.4))


(use-package cc-vars
  :ensure nil
  :defer t
  :config
  (setq c-basic-offset 4)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))


(use-package mouse
  :ensure nil
  :defer t
  :config
  (setq mouse-yank-at-point t))


(use-package mwheel
  :ensure nil
  :defer t
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5))
        mouse-wheel-progressive-speed nil))


(use-package delsel
  :ensure nil
  :defer t
  :config
  ;; replace the active region just by typing text, just like modern editors
  (delete-selection-mode +1))


(use-package tooltip
  :ensure nil
  :defer t
  :config
  ;; Don't display floating tooltips; display their contents in the echo-area.
  (tooltip-mode -1))


(use-package frame
  :ensure nil
  :hook (window-setup . window-divider-mode)
  :config
  ;; Don't blink the cursor, it's too distracting.
  (blink-cursor-mode -1)
  ;; Display dividers between windows
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1))


(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))


(use-package uniquify
  :ensure nil
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package js
  :ensure nil
  :defer t
  :config
  (setq js-indent-level 2))


(use-package python
  :ensure nil
  :defer t
  :config (setq python-indent-offset 2))


;; Must be loaded after 'doom-modeline
;; See: https://github.com/seagle0128/doom-modeline/issues/216
(use-package desktop
  :ensure nil
  :hook ((doom-modeline-mode . desktop-revert))
  :init
  (setq desktop-restore-eager 10)
  (setq desktop-lazy-verbose nil)
  :config
  (desktop-save-mode 1))


(provide 'init-base)
;;; init-base.el ends here
