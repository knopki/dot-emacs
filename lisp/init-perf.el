;;; init-perf.el --- Performance hacks. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-const))


;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later.
;; Not resetting it will cause stuttering/freezes.
(defvar default-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 100)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file we load.
(setq load-prefer-newer noninteractive)

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Run GC every 10s when idle
(run-with-idle-timer 10 t #'garbage-collect)

;; GC automatically while unfocusing the frame
;; `focus-out-hook' is obsolete since 27.1
(if (boundp 'after-focus-change-function)
    (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
  (add-hook 'focus-out-hook 'garbage-collect))

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


;; Don't make a second case-insensitive pass over `auto-mode-alist'. If it has
;; to, it's our (the user's) failure. One case for all!
(setq auto-mode-case-fold nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Remove command line options that aren't relevant to our current OS; that
;; means less to process at startup.
(unless (eq system-type 'darwin) (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux) (setq command-line-x-option-alist nil))

;; Don’t compact font caches during garbage collect
(setq inhibit-compacting-font-caches t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages
;; (sorry!), but for us left-to-right language speakers/writers, it's a boon.
(setq-default bidi-display-reordering 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)


(provide 'init-perf)
;;; init-perf.el ends here
