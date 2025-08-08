;;; early-init.el --- Early initialization for Emacs 30.1 -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Minimal early initialization for optimal startup performance.

;;; Code:

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar grim--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist grim--file-name-handler-alist)
            (garbage-collect)))

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t
        native-comp-async-jobs-number
        (let ((cpu-count (or (num-processors) 4)))
          (cond ((>= cpu-count 16) (- cpu-count 4))
                ((>= cpu-count 12) (- cpu-count 2))
                ((>= cpu-count 8) (max 4 (/ cpu-count 2)))
                (t (min 4 (/ cpu-count 2)))))))

(setq custom-file (make-temp-file "emacs-custom-"))

(setq package-enable-at-startup t
      package-quickstart t)

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (background-color . "#000000")
        (foreground-color . "#ffffff")
        (font . "AporeticSansMono Nerd Font")))

(setq frame-resize-pixelwise t
      inhibit-startup-screen t
      load-prefer-newer t)

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-check-signature 'allow-unsigned)
(setq package-archive-priorities
      '(("gnu-elpa" . 10)
        ("nongnu" . 5)
        ("melpa" . 3)))
(setq package-archive-column-width 12
      package-version-column-width 28
      package-install-upgrade-built-in t
      package-native-compile t)
(setq package-vc-register-as-project nil)

(provide 'early-init)
;;; early-init.el ends here
