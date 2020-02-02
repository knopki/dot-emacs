;;; init-diff-hl.el --- Initialize diff-hl configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package diff-hl
  :defer t
  :after magit
  :hook
  (prog-mode . diff-hl-mode)
  (org-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))


(provide 'init-diff-hl)
;;; init-diff-hl.el ends here
