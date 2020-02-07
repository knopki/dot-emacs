;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package org
  :ensure nil
  :diminish org-indent-mode
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . org-indent-mode)
   (org-indent-mode . (lambda () (diminish 'org-indent-mode))))
  :custom-face (org-ellipsis ((t (:foreground nil))))
  :custom
  (org-modules '(org-checklist org-habit))

  (org-directory "~/org")
  (org-log-done 'time)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-catch-invisible-edits 'smart)
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-src-window-setup 'current-window)
  (org-blank-before-new-entry (quote ((heading) (plain-list-item))))
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-archive-location (concat org-directory "/archive/%s_archive::datetree/"))

  ;; Keywords
  (org-todo-keywords
   '((sequence "TODO(t!)" "NEXT(n)" "WIP(i!)" "WAITING(w@/!)"
               "GAVE(g!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces
   '(("TODO"     . org-todo)
     ("NEXT"     . org-warning)
     ("WIP"      . (:foreground "OrangeRed" :weight bold))
     ("WAITING"  . (:foreground "coral" :weight bold))
     ("GAVE"     . (:foreground "LimeGreen" :weight bold))
     ("CANCELED" . org-done)
     ("DONE"     . org-done)))

  ;; Priorities
  (org-lowest-priority ?D)
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)
                        (?D . normal)))

  ;; Tags
  (org-tags-exclude-from-inheritance '(olga))

  ;; Agenda
  (org-agenda-files '("~/org"))
  (org-agenda-text-search-extra-files '(agenda-archives))
  (org-agenda-span 14)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day "-3d")
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-include-diary t)
  (org-stuck-projects
   '("+projects/-DONE" ("NEXT" "WIP") ("@shop") "\\<IGNORE\\>"))

  ;; Capture
  (org-default-notes-file (expand-file-name "capture.org" org-directory))
  (org-capture-templates
   '(("t" "My TODO task" entry
      (file "capture.org")
      "* TODO  %?\nSCHEDULED: %t")))

  ;; Refile
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  (org-refile-use-cache t)

  ;; Attachments
  (org-attach-archive-delete file 'query)

  :general
  (general-nmap
    :prefix "SPC o"
    "a" 'org-agenda
    "c" 'counsel-org-capture
    "l" 'org-agenda-list
    "s" 'org-search-view)
  :config
  ;; Autosave (no sure is it worth it)
  (run-with-idle-timer 30 t 'org-save-all-org-buffers)

  ;; org-goto/ivy interplay hack
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)

  ;; Clock
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (shell . t)
                               (python . t)
                               (js . t)
                               (css . t)))

  ;; (use-package ob-go
  ;;   :init (cl-pushnew '(go . t) load-language-list))

  ;; (use-package ob-ipython
  ;;   :if (executable-find "jupyter")     ; DO NOT remove
  ;;   :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list))


(use-package evil-org
  :diminish
  :ensure t
  :hook ((org-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme))))
  :custom
  (evil-org-key-theme
   '(navigation insert return textobjects additional shift todo heading calendar))
  (evil-org-retain-visual-state-on-shift t)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package org-bullets
  :diminish
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-ellipsis "…"))


(use-package org-fancy-priorities
  :diminish
  :hook (org-mode . org-fancy-priorities-mode)
  :init (setq org-fancy-priorities-list
              (if (char-displayable-p ?⚡)
                  '("⚡" "⬆" "⬇" "☕")
                '("HI" "MID" "LOW" "OPT"))))

(use-package org-sticky-header
  :diminish
  :hook (org-mode . org-sticky-header-mode)
  :custom
  (org-sticky-header-full-path 'full)
  (org-sticky-header-outline-path-separator " / "))


(provide 'init-org)
;;; init-org.el ends here
