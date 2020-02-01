;;; init.el --- Initial module. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "26.3")
  (error "This requires Emacs 26.3 and above!"))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
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

;; Performance hacks - better to load it earlier
(require 'init-perf)
(when (getenv "EMACS_BENCHMARK") (require 'init-benchmark))

;; Initialize use-package
(require 'init-package)

;; Normalize paths
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
(require 'init-yasnippet)


;; Load manual customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
