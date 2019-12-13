;;; init-persistent-scratch.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package))


(use-package persistent-scratch
  :hook (after-init . persistent-scratch-autosave-mode)
  :config
  (persistent-scratch-setup-default))


(provide 'init-persistent-scratch)
;;; init-persistent-scratch.el ends here
