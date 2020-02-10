;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:




(use-package org-bullets
  :diminish
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-ellipsis "…"))


(use-package org-fancy-priorities
  :diminish
  :hook (org-mode . org-fancy-priorities-mode)
  :init (setq org-fancy-priorities-list
              (if (char-displayable-p ?⚡)
                  '("⚡" "⬆" "⬇" "☕")
                '("HI" "MID" "LOW" "OPT"))))

(use-package org-sticky-header
  :diminish
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full)
  (org-sticky-header-outline-path-separator " / "))


(provide 'init-org)
;;; init-org.el ends here
