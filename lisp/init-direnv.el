;;; init-direnv.el --- Initialize reverse-im configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-package))


(use-package direnv
  :config
  (direnv-mode))


(provide 'init-direnv)
;;; init-direnv.el ends here
