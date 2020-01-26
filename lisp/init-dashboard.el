;;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package)
  (require 'init-projectile)
  (require 'init-general))

;; Dashboard
(use-package dashboard
  :diminish (dashboard-mode page-break-lines-mode)
  :functions
  (all-the-icons-faicon
   all-the-icons-material
   open-custom-file
   widget-forward)
  :general
  (general-nmap
    :keymaps 'dashboard-mode-map
    "SPC" nil
    [down-mouse-1] 'widget-button-click
    "h"            'dashboard-previous-section
    "l"            'dashboard-next-section
    "k"            'widget-backward
    "j"            'widget-forward
    "g"            'dashboard-refresh-buffer)
  :init (dashboard-setup-startup-hook)
  :config
  (setq dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-set-footer nil
        dashboard-show-shortcuts nil
        dashboard-items '((recents . 10)
                          (bookmarks . 5)
                          (projects . 10)))

  (setq dashboard-startup-banner 2 ; just text logo
        dashboard-init-info "")

  ;; show Dashboard in frames created with emacsclient -c
  ;; (dashboard-setup-startup-hook)
  (setq initial-buffer-choice
        '(lambda () (get-buffer-create "*dashboard*"))))


(provide 'init-dashboard)
;;; init-dashboard.el ends here
