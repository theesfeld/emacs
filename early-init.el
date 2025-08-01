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

;; Disable garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable file handlers during startup
(defvar grim--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore settings after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist grim--file-name-handler-alist)
            (garbage-collect)))

;; Native compilation settings (Emacs 30.1)
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t
        native-comp-async-jobs-number (min 4 (/ (num-processors) 2))))

;; Disable custom.el
(setq custom-file (make-temp-file "emacs-custom-"))

;; Package initialization
(setq package-enable-at-startup t
      package-quickstart t)

;; Frame parameters for no flicker
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (background-color . "#000000")
        (foreground-color . "#ffffff")
        (font . "AporeticSansMono Nerd Font")))

;; Miscellaneous
(setq frame-resize-pixelwise t
      inhibit-startup-screen t
      load-prefer-newer t)

(provide 'early-init)
;;; early-init.el ends here