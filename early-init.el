;;; early-init.el -- My Emacs Configuration.
;;; Commentary:
;;; This file was tangled (automatically generated) from `README.org'
;;; Code:
;; -*- lexical-binding: t -*-

;; Defer garbage collection further back in the startup process.
;; Reset it later by enabling `gcmh-mode'.
(setq gc-cons-threshold most-positive-fixnum)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. We want init packages manually.
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to prevent
;; the use of stale byte-code. Otherwise, it saves us a little IO time to skip the
;; =mtime= checks on every *.elc file we load.
(setq load-prefer-newer noninteractive)

;; Don't make a second case-insensitive pass over =auto-mode-alist=. If it has to,
;; it's our (the user's) failure. One case for all!
(setq auto-mode-case-fold nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Remove command line options that aren't relevant to our current OS;
;; that means less to process at startup.
(unless (eq system-type 'darwin) (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux) (setq command-line-x-option-alist nil))

;; Don’t compact font caches during garbage collect.
(setq inhibit-compacting-font-caches t)

;; Disable bidirectional text rendering for a modest performance boost. No RTL, oops.
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows nil)


(provide 'early-init)
;;; early-init.el ends here
