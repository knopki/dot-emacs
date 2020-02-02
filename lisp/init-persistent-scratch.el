;;; init-persistent-scratch.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package persistent-scratch
  :hook (after-init . persistent-scratch-autosave-mode)
  :config
  (persistent-scratch-setup-default))


(provide 'init-persistent-scratch)
;;; init-persistent-scratch.el ends here
