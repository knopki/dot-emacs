;;; init-company.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        ;; Trigger completion immediately.
        company-idle-delay 0.1
        ;; Number the candidates (use M-1, M-2 etc to select completions).
        company-show-numbers t
        ;; Align annotation to the right side.
        company-tooltip-align-annotations t
        ;; Stop eclim auto save.
        company-eclim-auto-save nil
        ;; No downcase when completion.
        company-dabbrev-downcase nil
        company-selection-wrap-around t)

  ;; TOPIC: How add company-dabbrev to the Company completion popup?
  ;; URL: https://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
  (add-to-list 'company-backends 'company-dabbrev-code)
  (add-to-list 'company-backends 'company-gtags)
  (add-to-list 'company-backends 'company-etags)
  (add-to-list 'company-backends 'company-keywords)

  ;; Enable downcase only when completing the completion.
  (defun jcs--company-complete-selection--advice-around (fn)
    "Advice execute around `company-complete-selection' command."
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add 'company-complete-selection :around #'jcs--company-complete-selection--advice-around)

  (setq company-frontends
        ;; show tooltip even for single candidate
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)))


(use-package company-fuzzy
  :diminish company-fuzzy-mode
  :hook (company-mode . company-fuzzy-mode)
  :config
  (setq company-fuzzy-sorting-backend 'alphabetic)
  (setq company-fuzzy-prefix-ontop t)
  (setq company-fuzzy-show-annotation t)
  (with-eval-after-load 'company
    (global-company-fuzzy-mode t)))


;; Popup documentation for completion candidates
(use-package company-quickhelp
  :if (display-graphic-p)
  :defines company-quickhelp-delay
  :bind (:map company-active-map
              ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook (company-mode . company-quickhelp-mode)
  :config
  (setq company-quickhelp-delay 0.5))


(use-package company-prescient
  :after (prescient)
  :hook (company-mode . company-prescient-mode))


(use-package company-tabnine
  :after company
  :config
  (add-to-list 'company-backends #'company-tabnine))


(provide 'init-company)
;;; init-company.el ends here
