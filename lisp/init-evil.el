;;; init-evil.el --- The Dark Side.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-const)
  (require 'init-package))

(use-package evil
  :diminish undo-tree-mode
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-search-wrap t)
  (setq evil-regexp-search t)
  (setq evil-search-module 'evil-search)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  ;; (evil-set-initial-state 'term-mode 'emacs)

  ;; Amazing hack lifted from: http://emacs.stackexchange.com/a/15054/12585
  (fset 'evil-visual-update-x-selection 'ignore))


;; vim-like keybindings everywhere in Emacs
(use-package evil-collection
  :after evil
  :custom (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))


;; visual hints while editing
(use-package evil-goggles
  :diminish evil-goggles-mode
  :after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))


;; gc operator, like vim-commentary
(use-package evil-commentary
  :after evil
  :bind (:map evil-normal-state-map
              ("gc" . evil-commentary)))


;; like vim-surround
(use-package evil-surround
  :after evil
  :commands
  (evil-surround-edit
   evil-Surround-edit
   evil-surround-region
   evil-Surround-region)
  :init
  (evil-define-key 'operator global-map "s" 'evil-surround-edit)
  (evil-define-key 'operator global-map "S" 'evil-Surround-edit)
  (evil-define-key 'visual global-map "S" 'evil-surround-region)
  (evil-define-key 'visual global-map "gS" 'evil-Surround-region))


(provide 'init-evil)
;;; init-evil.el ends here
