;;; init-ivy.el --- Initialize ivy configurations. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish ivy-mode
  :hook (after-init . ivy-mode))

(use-package counsel
  :diminish counsel-mode
  :hook (ivy-mode . counsel-mode)
  :preface
  (defun my-ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer."
    (ivy--format-function-generic
     (lambda (str)
       (concat (if (display-graphic-p)
                   (all-the-icons-octicon "chevron-right" :height 0.8 :v-adjust -0.05)
                 ">")
               (propertize " " 'display `(space :align-to 2))
               (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat (propertize " " 'display `(space :align-to 2)) str))
     cands
     "\n"))
  :config
  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil)

  (with-no-warnings
    (setq ivy-format-functions-alist '((counsel-describe-face . counsel--faces-format-function)
                                       (t . my-ivy-format-function-arrow))))

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' %s")))


;; Better sorting and filtering
(use-package prescient
  :commands prescient-persist-mode
  :after (counsel)
  :hook (ivy-mode . prescient-persist-mode)
  :init
  (setq prescient-filter-method '(literal regexp initialism fuzzy)))


(use-package ivy-prescient
  :commands ivy-prescient-re-builder
  :after (counsel prescient)
  :hook (ivy-mode . ivy-prescient-mode)
  :custom-face
  (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
  :preface
  (defun ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))
  :config
  (setq ivy-prescient-retain-classic-highlighting t
        ivy-re-builders-alist '((counsel-ag . ivy-prescient-non-fuzzy)
                                (counsel-rg . ivy-prescient-non-fuzzy)
                                (counsel-pt . ivy-prescient-non-fuzzy)
                                (counsel-grep . ivy-prescient-non-fuzzy)
                                (counsel-imenu . ivy-prescient-non-fuzzy)
                                (counsel-projectile-grep . ivy-prescient-non-fuzzy)
                                (counsel-projectile-rg . ivy-prescient-non-fuzzy)
                                (counsel-yank-pop . ivy-prescient-non-fuzzy)
                                (projectile-grep . ivy-prescient-non-fuzzy)
                                (projectile-ripgrep . ivy-prescient-non-fuzzy)
                                (swiper . ivy-prescient-non-fuzzy)
                                (swiper-isearch . ivy-prescient-non-fuzzy)
                                (swiper-all . ivy-prescient-non-fuzzy)
                                (insert-char . ivy-prescient-non-fuzzy)
                                (t . ivy-prescient-re-builder))
        ivy-prescient-sort-commands '(:not swiper swiper-isearch ivy-switch-buffer
                                           counsel-grep counsel-ag counsel-yank-pop)))


;; Correcting words with flyspell via Ivy
(use-package flyspell-correct-ivy
  :after (:all (flyspell ivy))
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :general
  ;; Redefine evil-mode keybinding
  ;; Also, use M-o to access ivy menu
  (general-nmap "z=" 'flyspell-correct-wrapper))


;; More friendly display transformer for Ivy
(use-package ivy-rich
  :after (:all (ivy counsel-projectile))
  :preface
  (with-no-warnings
    (defun ivy-rich-bookmark-name (candidate)
      (car (assoc candidate bookmark-alist)))

    (defun ivy-rich-buffer-icon (candidate)
      "Display buffer icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((buffer (get-buffer candidate))
               (buffer-file-name (buffer-file-name buffer))
               (major-mode (buffer-local-value 'major-mode buffer))
               (icon (with-current-buffer buffer (all-the-icons-icon-for-buffer))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-file-icon (candidate)
      "Display file icons in `ivy-rich'."
      (when (display-graphic-p)
        (let* ((path (concat ivy--directory candidate))
               (file (file-name-nondirectory path))
               (icon (cond
                      ((file-directory-p path)
                       (all-the-icons-icon-for-dir path nil ""))
                      ((string-match "^/.*:$" path)
                       (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                      ((not (string-empty-p file))
                       (all-the-icons-icon-for-file file :v-adjust -0.05)))))
          (if (symbolp icon)
              (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
            icon))))

    (defun ivy-rich-project-icon (_candidate)
      "Display project icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

    (defun ivy-rich-mode-icon (_candidate)
      "Display mode icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)))

    (defun ivy-rich-function-icon (_candidate)
      "Display function icons in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)))

    (defun ivy-rich-variable-icon (_candidate)
      "Display the variable icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)))

    (defun ivy-rich-symbol-icon (_candidate)
      "Display the symbol icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

    (defun ivy-rich-theme-icon (_candidate)
      "Display the theme icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2)))

    (defun ivy-rich-keybinding-icon (_candidate)
      "Display the keybindings icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "keyboard" :height 0.9 :v-adjust -0.15)))

    (defun ivy-rich-library-icon (_candidate)
      "Display the library icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue)))

    (defun ivy-rich-package-icon (_candidate)
      "Display the package icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))

    (defun ivy-rich-font-icon (_candidate)
      "Display the font icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-world-clock-icon (_candidate)
      "Display the world clock icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (defun ivy-rich-tramp-icon (_candidate)
      "Display the tramp icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01)))

    (defun ivy-rich-git-branch-icon (_candidate)
      "Display the git branch icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green)))

    (defun ivy-rich-process-icon (_candidate)
      "Display the process icon in `ivy-rich'."
      (when (display-graphic-p)
        (all-the-icons-faicon "bolt" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue)))

    (when (display-graphic-p)
      (defun my-ivy-rich-bookmark-type (candidate)
        (let ((filename (ivy-rich-bookmark-filename candidate)))
          (cond ((null filename)
                 (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
                ((file-remote-p filename)
                 (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01))
                ((not (file-exists-p filename))
                 (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
                ((file-directory-p filename)
                 (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))
      (advice-add #'ivy-rich-bookmark-type :override #'my-ivy-rich-bookmark-type)))
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((ivy-rich-function-icon)
            (counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((ivy-rich-function-icon)
            (counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-set-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-apropos
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate)))
          counsel-info-lookup-symbol
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate)))
          counsel-descbinds
          (:columns
           ((ivy-rich-keybinding-icon)
            (ivy-rich-candidate)))
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer)))
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer)))
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-el
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate)))
          counsel-fzf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
          counsel-buffer-or-recentf
          (:columns
           ((ivy-rich-file-icon)
            (counsel-buffer-or-recentf-transformer (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info)))
          counsel-bookmarked-directory
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-package
          (:columns
           ((ivy-rich-package-icon)
            (ivy-rich-candidate)))
          counsel-fonts
          (:columns
           ((ivy-rich-font-icon)
            (ivy-rich-candidate)))
          counsel-major
          (:columns
           ((ivy-rich-function-icon)
            (ivy-rich-candidate)))
          counsel-find-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate)))
          counsel-load-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate)))
          counsel-load-theme
          (:columns
           ((ivy-rich-theme-icon)
            (ivy-rich-candidate)))
          counsel-world-clock
          (:columns
           ((ivy-rich-world-clock-icon)
            (ivy-rich-candidate)))
          counsel-tramp
          (:columns
           ((ivy-rich-tramp-icon)
            (ivy-rich-candidate)))
          counsel-git-checkout
          (:columns
           ((ivy-rich-git-branch-icon)
            (ivy-rich-candidate)))
          counsel-list-processes
          (:columns
           ((ivy-rich-process-icon)
            (ivy-rich-candidate)))
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer)))
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-project-icon)
            (counsel-projectile-find-dir-transformer)))
          projectile-find-dir
          (:columns
           ((ivy-rich-project-icon)
            (counsel-projectile-find-dir-transformer)))
          counsel-minor
          (:columns
           ((ivy-rich-mode-icon)
            (ivy-rich-candidate)))
          treemacs-projectile
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))))))


(provide 'init-ivy)
;;; init-ivy.el ends here
