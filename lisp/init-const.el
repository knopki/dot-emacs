;;; init-const.el --- Define constants.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst knopki/gc-cons-threshold-bytes
  16777216 ; 16Mb
  "The default value to use for `gc-cons-threshold'.  If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(defconst knopki/font-default
  "FuraCode Nerd Font Mono"
  "Default font face.")


(provide 'init-const)
;;; init-const.el ends here
