;;; init-doom-themes.el --- Theme. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'init-package))


(use-package doom-themes
  :defines doom-themes-treemacs-theme
  :functions doom-themes-hide-modeline
  :hook (after-load-theme . (lambda ()
                              (set-face-foreground
                               'mode-line
                               (face-foreground 'default))))
  :init
  (load-theme 'doom-one t)
  :config
  ;; FIXME: @see https://github.com/hlissner/emacs-doom-themes/issues/317.
  (set-face-foreground 'mode-line (face-foreground 'default))

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; WORKAROUND: use legacy codes
  (set-face-attribute 'doom-visual-bell nil
                      :inherit 'mode-line
                      :background (face-foreground 'error)
                      :inverse-video nil)

  (defvar doom-themes--bell-p nil)
  (defun doom-themes-visual-bell-fn ()
    "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
    (unless doom-themes--bell-p
      (let ((old-remap (copy-alist face-remapping-alist)))
        (setq doom-themes--bell-p t)
        (setq face-remapping-alist
              (append (delete (assq 'mode-line face-remapping-alist)
                              face-remapping-alist)
                      '((mode-line doom-visual-bell))))
        (force-mode-line-update)
        (run-with-timer
         0.15 nil
         (lambda (remap buf)
           (with-current-buffer
               buf
             (when (assq 'mode-line face-remapping-alist)
               (setq face-remapping-alist remap
                     doom-themes--bell-p nil))
             (force-mode-line-update)))
         old-remap
         (current-buffer)))))

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; Enable customized theme (`all-the-icons' must be installed!)
  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (with-eval-after-load
      'treemacs
    (remove-hook 'treemacs-mode-hook #'doom-themes-hide-modeline)))


(provide 'init-doom-themes)
;;; init-doom-themes.el ends here
