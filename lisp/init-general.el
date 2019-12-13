;;; init-general.el --- A general is the leader.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package))

(use-package general
  :config
  (general-evil-setup t)

  (general-nmap
    :prefix "SPC"
    "<f1>" 'general-describe-keybindings
    "c"    'calc)

  ;; Move visual block
  (general-vmap
    "J" (concat ":m '>+1" (kbd "RET") "gv=gv")
    "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))


(provide 'init-general)
;;; init-general.el ends here
