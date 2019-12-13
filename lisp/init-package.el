;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-const))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

;; Setup package archives
(require 'package)
(setq package-archives '(("org"          . "https://orgmode.org/elpa/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)


(provide 'init-package)
;;; init-package.el ends here
