;;; early-init.el --- Early initialization for Emacs 30.1 -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Early initialization for Emacs 30.1, executed before package system
;; and GUI initialization.  Optimized for performance and modern features.

;;; Code:

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent
        native-compile-prune-cache t
        native-comp-speed 3
        native-comp-debug 0
        native-comp-verbose 0
        native-comp-jit-compilation t
        native-comp-warning-on-missing-source t
        native-comp-compiler-options '("-O3")
        ;; Reduce parallel jobs to avoid system overload
        native-comp-async-jobs-number (max 4 (- (num-processors) 2)))

  ;; More selective JIT compilation deny list
  (setq native-comp-jit-compilation-deny-list
        '("/\\.dir-locals\\.el\\'"
          "/\\.\\(git\\|svn\\|hg\\)/.*\\'"
          "/temp\\(?:orary\\)?/.*\\'"
          "/tmp/.*\\'"
          "\\(?:[/\\\\]init\\.el$\\)"
          "\\(?:[/\\\\]early-init\\.el$\\)"
          "\\(?:[/\\\\]custom\\.el$\\)"
          "\\(?:[/\\\\]recentf$\\)"
          "\\(?:[/\\\\]savehist$\\)"
          "\\(?:[/\\\\]saveplace$\\)"
          ".*-autoloads\\.el$"
          ".*loaddefs\\.el$"))

  ;; Centralized ELN cache location
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache
     (expand-file-name "eln-cache/" user-emacs-directory)))

  ;; Emacs 30.1: Control synchronous compilation
  (when (boundp 'native-comp-jit-compilation-blocklist)
    (setq native-comp-jit-compilation-blocklist
          '(eval-buffer eval-region eval-last-sexp)))

  ;; Emacs 30.1: Optimized cache and job management
  (when (boundp 'startup-redirect-eln-cache)
    (setq startup-redirect-eln-cache
          (expand-file-name "eln-cache/" user-emacs-directory)))

  (setq native-comp-priority-cpus (min 4 (max 2 (/ (num-processors) 4))))

  ;; Functions that should never be optimized
  (setq native-comp-never-optimize-functions
        '(eval-buffer eval-region eval-last-sexp
          load-file byte-compile-file)))

;; Disable custom.el by making it disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; use-package is built-in as of Emacs 29
(setq use-package-enable-imenu-support t
      use-package-compute-statistics t    ; Enable statistics
      use-package-verbose nil)            ; Less verbose output

;; Prevent white flash during startup
;; Set dark background immediately - this is the proper way in Emacs 30.1
(setq-default default-frame-alist
              (append default-frame-alist
                      '((background-color . "#000000")
                        (foreground-color . "#ffffff"))))

;; Ensure packages will be initialized after early-init
(setq package-enable-at-startup t)

;; Package system configuration for Emacs 30.1
(setq package-enable-at-startup t)        ; Let package.el handle initialization
(setq package-quickstart t)               ; Use package quickstart cache

;; Package archives optimized for Emacs 30.1
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")          ; Official GNU packages
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")       ; GNU development packages
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")            ; NonGNU ELPA
        ("melpa" . "https://melpa.org/packages/")))              ; Community packages

;; Enable package signature verification for security
(setq package-check-signature 'allow-unsigned) ; Allow unsigned for MELPA compatibility

;; Package priorities - prefer GNU packages
(setq package-archive-priorities
      '(("gnu-elpa" . 10)
        ("gnu-elpa-devel" . 8)
        ("nongnu" . 5)
        ("melpa" . 3)))

;; Emacs 30.1: Package selection and installation policies
(setq package-archive-column-width 12
      package-version-column-width 28
      package-install-upgrade-built-in t
      package-native-compile t)           ; Native compile packages

;; Package-vc settings for installing from source (new in Emacs 29+)
(setq package-vc-register-as-project nil) ; Don't register vc packages as projects

(setq read-process-output-max
      (let ((mem-gb (if (eq system-type 'gnu/linux)
                        (/ (string-to-number
                            (shell-command-to-string
                             "grep MemTotal /proc/meminfo | awk '{print $2}'"))
                           1048576.0)
                      32)))
        (cond ((>= mem-gb 64) (* 32 1024 1024))
              ((>= mem-gb 32) (* 8 1024 1024))
              (t (* 1024 1024)))))

(setq process-adaptive-read-buffering nil)      ; More consistent subprocess I/O
(setq bidi-display-reordering 'left-to-right)   ; Faster for LTR languages
(setq-default bidi-paragraph-direction 'left-to-right)
(setq auto-mode-case-fold nil)                  ; Faster file-mode detection
(setq ffap-machine-p-known 'reject)             ; Don't ping for machine names
(setq command-line-x-option-alist nil)          ; Skip X11 options in terminal

;; Theme configuration hints for later loading
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))

;; Prevent theme loading during startup
(setq inhibit-x-resources t)              ; Don't load X resources

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t     ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen nil
      inhibit-startup-screen nil
      inhibit-x-resources t
      ;;      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu nil
      initial-buffer-choice nil)

;; Font configuration - done early to prevent flashing
;; But keep it minimal to speed up startup
;; Only set font for GUI frames
(when (display-graphic-p)
  (push '(font . "AporeticSansMono Nerd Font") default-frame-alist)
  (push '(font . "AporeticSerifMono Nerd Font") initial-frame-alist))

;; Scratch buffer configuration - must be set early for proper initialization
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

")

;; Frame parameters optimized for fast startup and reduced flicker
(setq default-frame-alist
      `((width . 100)
        (height . 50)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (background-color . "#000000")
        (foreground-color . "#ffffff")
        ,@(when (display-graphic-p)
            '((internal-border-width . 0)
              (left-fringe . 8)
              (right-fringe . 8)
              (alpha-background . 100)      ; Emacs 29+ transparency
              (ns-appearance . dark)        ; macOS dark mode
              (ns-transparent-titlebar . t))))) ; macOS transparent titlebar

;; Initial frame inherits default settings
(setq initial-frame-alist default-frame-alist)

(defun grim-emacs-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))
(add-hook 'after-make-frame-functions #'grim-emacs-no-minibuffer-scroll-bar)

;; Garbage collection and file handler optimizations
(defvar grim--initial-gc-cons-threshold gc-cons-threshold)
(defvar grim--initial-gc-cons-percentage gc-cons-percentage)
(defvar grim--initial-file-name-handler-alist file-name-handler-alist)

(defvar grim--system-gc-threshold
  (let ((mem-gb (if (eq system-type 'gnu/linux)
                    (/ (string-to-number
                        (shell-command-to-string
                         "grep MemTotal /proc/meminfo | awk '{print $2}'"))
                       1048576.0)
                  32)))
    (cond ((>= mem-gb 64) (* 2048 1024 1024))
          ((>= mem-gb 32) (* 512 1024 1024))
          (t (* 256 1024 1024)))))

(defvar grim--system-gc-percentage
  (let ((cpu-count (or (num-processors) 4)))
    (cond ((>= cpu-count 12) 0.15)
          ((>= cpu-count 8) 0.2)
          (t 0.3))))

;; Temporarily disable GC and file handlers during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; Restore after startup with system-specific settings
(defun grim--startup-optimization-restore ()
  "Restore normal GC and file handler settings after startup."
  (setq gc-cons-threshold grim--system-gc-threshold
        gc-cons-percentage grim--system-gc-percentage
        file-name-handler-alist grim--initial-file-name-handler-alist)
  (garbage-collect))

(add-hook 'emacs-startup-hook #'grim--startup-optimization-restore)

;; Frame naming for after-init
(add-hook 'window-setup-hook
          (lambda ()
            (set-frame-name "home"))
          -90) ; Run early in window-setup

;; Miscellaneous early settings
(setq load-prefer-newer t)                ; Load newer files
(setq native-comp-async-report-warnings-errors nil) ; Silence native-comp warnings

;; Emacs 30.1: Configure before/after init hooks
(setq before-init-time (current-time))    ; Track init time

(provide 'early-init)
;;; early-init.el ends here
