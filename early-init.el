;;; early-init.el -*- lexical-binding: t -*-

;; Native compilation settings optimized for Emacs 30.1
(when (native-comp-available-p)
  ;; Enhanced error handling and performance for Emacs 30.1
  (setq native-comp-async-report-warnings-errors 'silent
        native-compile-prune-cache t
        native-comp-speed 3                    ; Balance speed vs compile time
        native-comp-debug 0                    ; Disable debug for performance
        native-comp-verbose 0                  ; Reduce compilation noise
        native-comp-jit-compilation t          ; Enable JIT compilation (30.1)
        native-comp-deferred-compilation t    ; Compile in background
        native-comp-compiler-options '("-O3" "-march=alderlake"))

  ;; Emacs 30.1: Optimized cache and job management
  (when (boundp 'startup-redirect-eln-cache)
    (setq startup-redirect-eln-cache t))

  ;; Increase native compilation job limit for modern systems
  (when (> (num-processors) 4)
    (setq native-comp-async-jobs-number (/ (num-processors) 2)))

  ;; Prioritize frequently used packages for native compilation
  (setq native-comp-bootstrap-allow-list
        '("vertico" "orderless" "consult" "marginalia" "magit"))

  (setq native-comp-bootstrap-deny-list
        '("tramp" "tramp-.*" "docker-tramp")) ; Avoid compiling tramp for stability

  (when (> (num-processors) 8)
    (setq native-comp-async-jobs-number (- (num-processors) 2))))

;; Disable custom.el by making it disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Enable use-package imenu support early
(setq use-package-enable-imenu-support t)

;; Prevent white flash during startup
;; Set dark background immediately - this is the proper way in Emacs 30.1
(setq-default default-frame-alist
              (append default-frame-alist
                      '((background-color . "#000000")
                        (foreground-color . "#ffffff"))))

;; Ensure packages will be initialized after early-init
(setq package-enable-at-startup t)

;; Package archives optimized for Emacs 30.1
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")          ; Official GNU packages
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")       ; GNU development packages
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")            ; NonGNU ELPA
        ("melpa" . "https://melpa.org/packages/")))              ; Community packages

;; Enable package signature verification for security
(setq package-check-signature 'allow-unsigned) ; Allow unsigned for MELPA compatibility

;; Package priorities optimized for GNU compliance and Emacs 30.1
;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 10)        ; Highest priority for official GNU packages
        ("gnu-elpa-devel" . 8)   ; Development GNU packages
        ("nongnu" . 5)           ; NonGNU ELPA packages
        ("melpa" . 3)))          ; MELPA packages (lower priority)

;; Prefer GNU ELPA packages when available
(setq package-archive-selection-policy 'prefer-gnu)

(when (boundp 'package-install-parallel)
  (setq package-install-parallel t))

;; Pre-configure modus-themes settings for when it loads
(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-mixed-fonts t
      modus-themes-disable-other-themes t)

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

(when (find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
  (set-face-attribute 'default nil
                      :font "BerkeleyMonoVariable Nerd Font Mono"
                      :height 140))

;; Set variable-pitch font (optional, for prose or Org-mode)
(when (find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
  (set-face-attribute 'variable-pitch nil
                      :font "BerkeleyMonoVariable Nerd Font Mono"
                      :height 160))

;; Customize font-lock faces
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic
                    :weight 'light)
(set-face-attribute 'font-lock-keyword-face nil
                    :weight 'black)

;; Scratch buffer configuration - must be set early for proper initialization
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

")

(dolist (variable '(initial-frame-alist default-frame-alist))
  (set variable `((width . (text-pixels . 800))
                  (height . (text-pixels . 900))
                  (horizontal-scroll-bars . nil)
                  (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                  (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                  ,@(if x-toolkit-scroll-bars
                        (list
                         '(vertical-scroll-bars . nil)
                         '(scroll-bar-width . 12))
                      (list
                       '(vertical-scroll-bars . right)
                       '(scroll-bar-width . 6))))))

(defun grim-emacs-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))
(add-hook 'after-make-frame-functions #'grim-emacs-no-minibuffer-scroll-bar)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.9)

(defvar grim-emacs--file-name-handler-alist file-name-handler-alist)
(defvar grim-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends '(Git))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist grim-emacs--file-name-handler-alist
                  vc-handled-backends grim-emacs--vc-handled-backends)))

(when (boundp 'gcmh-mode)
  (require 'gcmh nil t)
  (gcmh-mode 1))

(add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold (* 50 1024 1024))))
(add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(add-hook 'after-init-hook (lambda () (garbage-collect)))

;; Frame naming for after-init
(add-hook 'after-init-hook
          (lambda ()
            (set-frame-name "home")))

;;; early-init.el ends here
