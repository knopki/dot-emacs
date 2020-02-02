;;; init-benchmark.el --- Start benchmark if needed. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package benchmark-init
  :defines swiper-font-lock-exclude
  :commands (benchmark-init/activate)
  :hook (after-init . benchmark-init/deactivate)
  :init (benchmark-init/activate)
  :config
  (with-eval-after-load 'swiper
    (add-to-list 'swiper-font-lock-exclude 'benchmark-init/tree-mode)))


(provide 'init-benchmark)
;;; init-benchmark.el ends here
