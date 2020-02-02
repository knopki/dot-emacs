;;; init-reverse-im.el --- Initialize reverse-im configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-package))


(use-package reverse-im
  :custom
  (reverse-im-modifiers '(control meta super))
  :config
  (reverse-im-activate "russian-computer"))


(provide 'init-reverse-im)
;;; init-reverse-im.el ends here
