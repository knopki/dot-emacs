;;; init-flycheck.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package))


(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
