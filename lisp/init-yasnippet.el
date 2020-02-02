;;; init-yasnippet.el --- Initialize yasnippet configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-general))


(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))


;; Integrate yasnippet
(use-package ivy-yasnippet
  :commands ivy-yasnippet--preview
  :general
  (general-nmap
    :prefix "SPC"
    "y" 'ivy-yasnippet)
  :config
  (advice-add #'ivy-yasnippet--preview :override #'ignore))


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
