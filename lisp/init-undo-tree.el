;;; init-undo-tree.el --- Treat undo history as a tree -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package undo-tree
  :diminish
  :defines recentf-exclude
  :hook (after-init . global-undo-tree-mode)
  :general
  (general-nmap :prefix "SPC" "<f5>" 'undo-tree-visualize)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-visualizer-lazy-drawing 1000
        undo-tree-auto-save-history t)
  (dolist (dir undo-tree-history-directory-alist)
    (push (expand-file-name (cdr dir)) recentf-exclude)))


(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
