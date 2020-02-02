;;; init-solaire-mode.el --- Make certain buffers grossly incandescent -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package solaire-mode
  :functions persp-load-state-from-file
  :hook
  ((after-init . solaire-global-mode)
   ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
   (minibuffer-setup . solaire-mode-in-minibuffer)
   (after-load-theme . solaire-mode-swap-bg))
  :config
  (setq solaire-mode-remap-fringe nil)
  (solaire-mode-swap-bg)
  (advice-add #'persp-load-state-from-file
              :after #'solaire-mode-restore-persp-mode-buffers))


(provide 'init-solaire-mode)
;;; init-solaire-mode.el ends here
