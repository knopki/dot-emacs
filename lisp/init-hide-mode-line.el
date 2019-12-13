;;; init-hide-mode-line.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package))

;; Hide mode-line for completion list
(use-package hide-mode-line
  :hook
  (((completion-list-mode completion-in-region-mode) . hide-mode-line-mode)))


(provide 'init-hide-mode-line)
;;; init-hide-mode-line.el ends here
