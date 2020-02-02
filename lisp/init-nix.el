;;; init-nix.el --- Initialize nix-mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :custom
  (nix-indent-function 'smie-indent-line))


(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")


(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))


(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))


(use-package nix-format
  :ensure nix-mode
  :custom
  (nix-nixfmt-bin "nixpkgs-fmt")
  :commands (nix-format-buffer))


(provide 'init-nix)
;;; init-nix.el ends here
