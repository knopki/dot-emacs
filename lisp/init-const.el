;;; init-const.el --- Define constants.	-*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Platforms
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")


;; Emacs versions
(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")


(defconst knopki/gc-cons-threshold-bytes
  16777216 ; 16Mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing, decrease this. If you experience stuttering, increase this.")

(defconst knopki/font-default
  "FuraCode Nerd Font Mono"
  "Default font face.")


(provide 'init-const)
;;; init-const.el ends here
