;;; init-magit.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :general
  (general-nmap
    :prefix "SPC"
    "g" 'magit-status)
  :config
  (setq magit-completing-read-function 'ivy-completing-read))


(use-package evil-magit
  :after (:all (magit evil)))


(provide 'init-magit)
;;; init-magit.el ends here
