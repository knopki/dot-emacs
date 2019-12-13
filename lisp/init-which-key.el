;;; init-which-key.el --- Which-key, help me!  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package)
  (require 'init-general))


(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :general
  (general-nmap
    :prefix "SPC"
    "<f2>" 'which-key-show-top-level
    "<f3>" 'which-key-show-major-mode
    "<f4>" 'which-key-show-full-keymap)
  :config
  (which-key-setup-side-window-right-bottom))


(provide 'init-which-key)
;;; init-which-key.el ends here
