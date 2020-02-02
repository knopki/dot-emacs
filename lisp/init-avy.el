;;; init-avy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-general))


(use-package avy
             :commands (avy-goto-word-1)
             :general
             (general-mmap "C-'" 'evil-avy-goto-char-timer
                           "C-\"" 'evil-avy-goto-word-0))


(provide 'init-avy)
;;; init-avy.el ends here
