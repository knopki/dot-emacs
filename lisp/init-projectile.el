;;; init-projectile.el --- Initialize projectile configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package)
  (require 'init-base)
  (require 'init-general))

;; Manage and navigate projects
(use-package projectile
  :diminish projectile-mode
  :hook (after-init . projectile-mode)
  :general
  (general-nmap
    :prefix "SPC"
    "p" '(:keymap projectile-command-map :package projectile))
  :config
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t
        projectile-enable-cache t)

  ;; Ivy integration
  (setq projectile-completion-system 'ivy)

  ;; Update mode-line at the first time
  (projectile-update-mode-line))

;; More advanced Ivy integration
(use-package counsel-projectile
  :after (projectile)
  :hook (after-init . counsel-projectile-mode)
  :config
  (define-obsolete-function-alias 'counsel-more-chars 'ivy-more-chars "26.3")
  (setq counsel-projectile-rg-initial-input '(projectile-symbol-or-selection-at-point)))


(provide 'init-projectile)
;;; init-projectile.el ends here
