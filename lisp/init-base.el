;;; init-base.el --- Better default configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup))


(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 200
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (run-at-time t (* 5 60) 'recentf-save-list)
  :config
  (add-to-list 'recentf-exclude (expand-file-name recentf-save-file))
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        history-delete-duplicates t
        savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)
        savehist-autosave-interval 300))


(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info t
        auto-revert-verbose nil))


;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook
  ;; show org ediffs unfolded
  ((ediff-prepare-buffer . outline-show-all)
   ;; restore window layout when done
   (ediff-quit . winner-undo))

  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-multiframe))


;; Automatic parenthesis pairing.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package paren
  :ensure nil
  :defer t
  :config
  (setq show-paren-delay 0)
  (show-paren-mode +1))


;; On-the-fly spell checker.
(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "hunspell")
  :hook
  (((text-mode outline-mode org-mode) . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :init
  (setq flyspell-issue-message-flag nil)
  (with-eval-after-load "ispell"
    (setq ispell-program-name "hunspell")
    (setq ispell-dictionary "en_US,ru_RU")
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_US,ru_RU")))


(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :hook
  (prog-mode . eldoc-mode)
  :config
  (global-eldoc-mode -1)
  (setq eldoc-idle-delay 0.4))


(use-package cc-vars
  :ensure nil
  :defer t
  :config
  (setq c-basic-offset 4)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "k&r"))))


(use-package mouse
  :ensure nil
  :defer t
  :config
  (setq mouse-yank-at-point t))


(use-package mwheel
  :ensure nil
  :defer t
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 5))
        mouse-wheel-progressive-speed nil))


(use-package delsel
  :ensure nil
  :defer t
  :config
  ;; replace the active region just by typing text, just like modern editors
  (delete-selection-mode +1))


(use-package tooltip
  :ensure nil
  :defer t
  :config
  ;; Don't display floating tooltips; display their contents in the echo-area.
  (tooltip-mode -1))


(use-package frame
  :ensure nil
  :hook (window-setup . window-divider-mode)
  :config
  ;; Don't blink the cursor, it's too distracting.
  (blink-cursor-mode -1)
  ;; Display dividers between windows
  (setq window-divider-default-places t
        window-divider-default-bottom-width 1
        window-divider-default-right-width 1))


(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))


(use-package uniquify
  :ensure nil
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package js
  :ensure nil
  :defer t
  :config
  (setq js-indent-level 2))


(use-package python
  :ensure nil
  :defer t
  :config (setq python-indent-offset 2))


;; Must be loaded after 'doom-modeline
;; See: https://github.com/seagle0128/doom-modeline/issues/216
(use-package desktop
  :ensure nil
  :hook ((doom-modeline-mode . desktop-revert))
  :init
  (setq desktop-restore-eager 10)
  (setq desktop-lazy-verbose nil)
  :config
  (desktop-save-mode 1))


(provide 'init-base)
;;; init-base.el ends here
