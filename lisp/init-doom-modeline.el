;;; init-doom-modeline.el --- Modeline. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format nil))

  ;; Whether display minor modes or not. Non-nil to display in mode-line.
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-mu4e nil))


(use-package nyan-mode
  :diminish nyan-mode
  :custom
  (nyan-bar-length 16)
  :hook (after-init . nyan-mode))


(provide 'init-doom-modeline)
;;; init-doom-modeline.el ends here
