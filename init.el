;;; init.el -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Early Initial Settings                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inhibit startup screen.
(setq inhibit-startup-message t)

;; Keep the custom variables out of our main init file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Garbage Collection                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use a higher GC threshold during init, then adjust dynamically
(setq gc-cons-threshold most-positive-fixnum)  ;; Max during startup

(defun my-adjust-gc-threshold ()
  "Set a reasonable GC threshold after startup and adjust dynamically."
  (setq gc-cons-threshold (* 100 1024 1024))  ;; 100 MB default
  (setq gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'my-adjust-gc-threshold)

;; Temporarily increase GC threshold during minibuffer usage
(defun my-increase-gc-during-minibuffer ()
  "Increase GC threshold while in minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-restore-gc-after-minibuffer ()
  "Restore GC threshold after exiting minibuffer."
  (setq gc-cons-threshold (* 100 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-increase-gc-during-minibuffer)
(add-hook 'minibuffer-exit-hook #'my-restore-gc-after-minibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    UI                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove the menu/tool/scroll bars early for minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Dracula theme
 (use-package dracula-theme
   :ensure t
   :config
   (load-theme 'dracula t))

 (set-face-attribute 'default nil :height 120)
 (set-face-attribute 'variable-pitch nil :height 130)

 (set-face-attribute 'font-lock-comment-face nil
                     :slant 'italic
                     :weight 'light
                     :distant-foreground "#5c6370"
                     :foreground "#5c6370"
                     :background "#282c34")

 (set-face-attribute 'font-lock-keyword-face nil
                     :weight 'black)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  MELPA                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Force a refresh and install missing packages
(when (not package-archive-contents)
  (package-refresh-contents))
;; Install all packages marked with :ensure t
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Version Control for Config                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Automatically commit changes to init.el using vc
(defun my-auto-commit-init-el ()
  "Commit changes to init.el after saving."
  (when (and (buffer-file-name)
             (string= (file-name-nondirectory (buffer-file-name)) "init.el"))
    (ignore-errors
      (vc-checkin (list (buffer-file-name)) 'git nil "Auto-commit init.el changes"))))

(add-hook 'after-save-hook #'my-auto-commit-init-el)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Basic Emacs Information                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "TJ"
      user-mail-address "william@theesfeld.net"
      calendar-location-name "New York, NY"
      calendar-time-zone-rule "EST"
      calendar-standard-time-zone-name "EST"
      calendar-daylight-time-zone-name "EDT"
      auth-sources '("~/.authinfo.gpg")
      epg-pinentry-mode 'loopback
      password-cache-expiry nil
      auth-source-cache-expiry nil
      gc-cons-percentage 0.6
      ;; Visual ellipsis for truncated lines:
      truncate-string-ellipsis "…"
      scroll-margin 1
      garbage-collection-messages nil
      plstore-cache-directory "~/.config/emacs/")

(require 'auth-source)
(setenv "TZ" "America/New_York")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Desktop Session Saving                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1)
  :config
  (setq desktop-path (list user-emacs-directory)  ;; Save in ~/.emacs.d/
        desktop-dirname user-emacs-directory
        desktop-base-file-name ".emacs-desktop"
        desktop-save t                     ;; Always save without prompting
        desktop-restore-eager 10           ;; Restore 10 buffers immediately, rest lazily
        desktop-auto-save-timeout 300))    ;; Auto-save every 5 minutes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Shell Environment                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-load-env-file ()
  "Load environment variables from ~/.config/emacs/.env into Emacs."
  (let ((env-file (expand-file-name ".env" user-emacs-directory)))
    (when (file-readable-p env-file)
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (unless (or (string-empty-p line)
                        (string-prefix-p "#" line))
              (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
                (let ((key (match-string 1 line))
                      (value (match-string 2 line)))
                  (setenv key value)
                  (message "Loaded env: %s" key)))))
          (forward-line 1))))
    (unless (file-exists-p env-file)
      (message "Warning: .env file not found at %s" env-file))))

(my-load-env-file)

;; Properly set up PATH and environment variables on macOS/Linux.
(use-package exec-path-from-shell
  :ensure t  ;; Ensure it’s installed
  :config
  (setq exec-path-from-shell-shell-name "/usr/bin/zsh")  ;; Explicitly use Zsh (Arch default path)
  (setq exec-path-from-shell-arguments '("-l"))          ;; -l makes it a login shell, sourcing .zshrc
  (exec-path-from-shell-initialize)                      ;; Run unconditionally
  (add-to-list 'exec-path "~/.local/bin/" t)  ; Add to end of exec-path
  (setenv "PATH" (concat (getenv "PATH") ":~/.local/bin/"))  ; Update PATH env var too
  (message "exec-path-from-shell ran with shell: %s" exec-path-from-shell-shell-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Global Emacs Settings                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; save to home directory by default
(setq-default default-directory '~)

;; (much) bigger kill ring
(setq-default kill-ring-max 5000)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; save pointer history
(use-package saveplace
  :ensure nil  ;; Emacs built-in
  :init
  (save-place-mode)
  :config
  (setq save-place-file (expand-file-name ".saveplace" user-emacs-directory)))

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; visual wrap prefix mode
(add-hook 'text-mode-hook #'visual-wrap-prefix-mode)



;; Display line numbers in all buffers
(global-display-line-numbers-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Pixel-precise scrolling
(pixel-scroll-precision-mode 1)
(setq scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; better diff coloring
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;; Enable auto-revert of changed files
(global-auto-revert-mode 1)

;; Move to trash on deletes
(setq delete-by-moving-to-trash t)

;; Resize windows automatically when splitting
(setq window-combination-resize t)

;; Show time but hide load average
(display-time-mode 1)
(setq display-time-load-average nil)

;; Indentation
(setq-default indent-tabs-mode nil)  ;; Use spaces instead of tabs
(setq tab-width 2)
(setq standard-indent 2)

;; Tidy up whitespace on save
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Savehist
(setq savehist-file "~/.config/emacs/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

;; Keep an eye on undone changes
(setq undo-limit 800000)

;; Show isearch count
(setq isearch-lazy-count t
      lazy-count-prefix-format nil
      lazy-count-suffix-format "   (%s/%s)")

;; Replacing kill-buffer key
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Backup and Auto-Save                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keep backups in a dedicated directory with timestamps
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq backup-by-copying t    ;; Don't clobber symlinks
      version-control t      ;; Use versioned backups
      kept-new-versions 10   ;; Keep 10 new versions
      kept-old-versions 5    ;; Keep 5 old versions
      delete-old-versions t  ;; Auto-delete excess backups
      vc-make-backup-files t ;; Backup even under version control
      backup-by-copying-when-linked t) ;; Handle hard links safely

;; Timestamped backup files
(setq make-backup-file-name-function
      (lambda (file)
        (concat (file-name-concat "~/.config/emacs/backups" (file-name-nondirectory file))
                "."
                (format-time-string "%Y%m%dT%H%M%S")
                "~")))

;; Save auto-save files in a dedicated directory
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t))
      auto-save-default t
      auto-save-timeout 30      ;; Auto-save after 30 seconds of idle
      auto-save-interval 200)   ;; Auto-save after 200 keystrokes

;; No TRAMP backups
(with-eval-after-load 'tramp
  (setq tramp-backup-directory-alist nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    vundo                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vundo
  :ensure t
  :bind
  ("C-x u" . vundo)
    :config
    (setq vundo-glyph-alist vundo-unicode-symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Visual Enhancements                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Indent bars
(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-prefer-character t)
  (indent-bars-treesit-scope '((python function_definition class_definition
                                       for_statement if_statement with_statement
                                       while_statement)))
  (indent-bars-color '(highlight :face-bg t :blend 0.15))
  (indent-bars-starting-column 0)
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth '(:blend 0.5))
  (indent-bars-display-on-blank-lines t)
  :hook
  (prog-mode . indent-bars-mode))

;; modeline - commented out for now as i kind of like default
;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 15)
;;   (doom-modeline-bar-width 6)
;;   (doom-modeline-minor-modes t)
;;   (doom-modeline-major-mode-icon t)
;;   (doom-modeline-buffer-state-icon t)
;;   (doom-modeline-buffer-modification-icon t))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Mode Line Cleanup                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package delight
  :ensure t
  :config
  (delight '((global-hl-line-mode nil "hl-line")
             (save-place-mode nil "saveplace")
             (global-auto-revert-mode nil "autorevert")
  ;           (corfu-mode " 💡" "corfu")
             (flyspell-mode " ✍" "flyspell")
             (which-key-mode nil "which-key")
             (yas-minor-mode nil "yasnippet")
             (smartparens-mode nil "smartparens"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Completion Setup                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Corfu: in-buffer completion
;; (use-package corfu
;;   :straight (:host github :repo "minad/corfu")
;;   :custom
;;   (corfu-auto t)
;;   (corfu-cycle t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 1.2)
;;   :init
;;   (global-corfu-mode)
;;   :config
;;   (defun my-disable-corfu-in-erc ()
;;     "Disable Corfu in erc-mode derived buffers."
;;     (corfu-mode -1))

;;   (add-hook 'erc-mode-hook #'my-disable-corfu-in-erc))

;; built-in completion-preview
;; Enable Completion Preview mode in code buffers
(add-hook 'prog-mode-hook #'completion-preview-mode)
;; also in text buffers
(add-hook 'text-mode-hook #'completion-preview-mode)
;; and in \\[shell] and friends
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))

(with-eval-after-load 'completion-preview
  ;; Primary bindings
  (keymap-set completion-preview-active-mode-map "<tab>" #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "<down>" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "<up>" #'completion-preview-prev-candidate)
  ;; Fallbacks for terminals or muscle memory
  (keymap-set completion-preview-active-mode-map "TAB" #'completion-preview-insert)  ; Same as <tab>, for robustness
  (keymap-set completion-preview-active-mode-map "C-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "C-p" #'completion-preview-prev-candidate)
  ;; Customizations
  (setq completion-preview-minimum-symbol-length 2)
  (push 'org-self-insert-command completion-preview-commands)
  (push 'paredit-backward-delete completion-preview-commands))

;; Better completion styles
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Terminal-friendly Corfu
;; (straight-use-package
;;  '(corfu-terminal
;;    :type git
;;    :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

;; Turn off automatic corfu in Eshell, but enable on demand
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (setq-local corfu-auto nil)
;;             (corfu-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  IBUFFER                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ibuffer-saved-filter-groups
      `(("home"
         ("Emacs" (filename . ,(concat "\\`" (regexp-quote (expand-file-name user-emacs-directory)) ".*")))
         ("Prog" (derived-mode . prog-mode))
         ("Org" (or (file-extension . "org")
                    (derived-mode . org-mode)
                    (derived-mode . org-agenda-mode)))
         ("PDF" (derived-mode . pdf-tools-mode))
         ("Mail" (or (derived-mode . rmail-mode)
                     (derived-mode . message-mode)))
         ("Gnus" (or (derived-mode . gnus-mode)
                     (saved . "gnus")))
         ("Net" (or (derived-mode . eww-mode)
                    (derived-mode . elfeed-mode)))
         ("IRC" (derived-mode . erc-mode))
         ("Dired" (derived-mode . dired-mode))
         ("Proc" (process))
         ("Stars" (starred-name)))))

(add-hook 'ibuffer-mode-hook
 (lambda ()
  (ibuffer-switch-to-saved-filter-groups "home")))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
(setq nerd-icons-ibuffer-icon t)
(setq nerd-icons-ibuffer-color-icon t)
(setq nerd-icons-ibuffer-icon-size 1.0)
(setq  nerd-icons-ibuffer-human-readable-size t)
nerd-icons-ibuffer-formats
(setq inhibit-compacting-font-caches t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Vertico + Consult                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Persist minibuffer history over Emacs restarts
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;; Consult
(use-package consult
  :ensure t
  :bind (("C-c M-x"   . consult-mode-command)
         ("C-c h"     . consult-history)
         ("C-c k"     . consult-kmacro)
         ("C-c m"     . consult-man)
         ("C-c i"     . consult-info)
         ("C-x M-:"   . consult-complex-command)
         ("C-x b"     . consult-buffer)  ; Default consult-buffer
;         ("C-x C-b"   . consult-buffer)
         ("C-x 4 b"   . consult-buffer-other-window)
         ("C-x 5 b"   . consult-buffer-other-frame)
         ("C-x t b"   . consult-buffer-other-tab)
         ("C-x r b"   . consult-bookmark)
         ("C-x p b"   . consult-project-buffer)
         ("M-#"       . consult-register-load)
         ("M-'"       . consult-register-store)
         ("C-M-#"     . consult-register)
         ("M-y"       . consult-yank-pop)
         ("M-g e"     . consult-compile-error)
         ("M-g g"     . consult-goto-line)
         ("M-g M-g"   . consult-goto-line)
         ("M-g o"     . consult-outline)
         ("M-g m"     . consult-mark)
         ("M-g k"     . consult-global-mark)
         ("M-g i"     . consult-imenu)
         ("M-g I"     . consult-imenu-multi)
         ("M-s d"     . consult-find)
         ("M-s c"     . consult-locate)
         ("M-s g"     . consult-grep)
         ("M-s G"     . consult-git-grep)
         ("M-s r"     . consult-ripgrep)
         ("M-s l"     . consult-line)
         ("M-s L"     . consult-line-multi)
         ("M-s k"     . consult-keep-lines)
         ("M-s u"     . consult-focus-lines)
         ("M-s e"     . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"       . consult-isearch-history)
         ("M-s e"     . consult-isearch-history)
         ("M-s l"     . consult-line))
  :config
  ;; Enhance consult-buffer with common settings
  (consult-customize
   consult-buffer
   :sort t  ; Sort by recency (default behavior, just explicit)
   :history 'buffer-name-history))  ; Use buffeer history

;; Embark
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))

;; Marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;; Nerd Icons support for completions
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after marginalia
  :ensure t
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Editing Helpers                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-format Emacs Lisp files on save
(defun my-elisp-format-buffer ()
  "Format the current Emacs Lisp buffer if syntactically valid."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (check-parens)  ; Returns t if balanced
        (progn
          (indent-region (point-min) (point-max))
          (whitespace-cleanup))
      (message "Skipping format: Unbalanced parentheses"))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my-elisp-format-buffer nil t)))

(global-set-key (kbd "C-c r") #'replace-regexp-as-diff)

(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

;; show uncommitted changes in the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode +1)
  ;; disable on slow TRAMP connections with diff-hl-disable-on-remote to t
  )

(use-package dash
  :ensure t)

(use-package helm
  :ensure t)

(use-package which-key
  :ensure nil
  :config
  (setq which-key-idle-delay 0.1)
  (which-key-mode))

(use-package avy
  :bind (("M-j" . avy-goto-char-timer))
  :init
  (avy-setup-default)
  :config
  (defun avy-action-exchange (pt)
    "Exchange sexp at PT with the one at point."
    (set-mark pt)
    (transpose-sexps 0))
  (add-to-list 'avy-dispatch-alist '(?e . avy-action-exchange))

  (defun avy-action-embark (pt)
    "Invoke Embark at PT."
    (save-excursion
      (goto-char pt)
      (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package avy-zap
  :bind (("M-z" . avy-zap-up-to-char-dwim)
         ("M-Z" . avy-zap-to-char-dwim))
  :config
  (setq avy-zap-forward-only t)
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Popup Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package popup
  :config
  ;; If you want the popup library to compute columns more optimally:
  (setq popup-use-optimized-column-computation t)

  ;; Example: limit maximum width of a popup-tip
  (setq popup-tip-max-width 80))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   FlySpell                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  ;; (prog-mode . flyspell-prog-mode) is too noisy, too many false positives
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (org-mode  . flyspell-mode))
  :init
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--camel-case"))
  :custom
  (flyspell-default-dictionary "american"))

(setq ispell-program-name "aspell"
      ispell-silently-savep t)

(use-package flyspell-popup
  :ensure t
  :after flyspell
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode)
  :config
  (setq flyspell-popup-auto-correct-delay 2)  ;; Delay before popup
  (define-key flyspell-mode-map (kbd "C-c TAB") #'flyspell-popup-correct))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;; (use-package flyspell-lazy
;;   :after flyspell
;;   :init
;;   (flyspell-lazy-mode 1)
;;   (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Project                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package project
  :ensure nil  ; Built-in, no external fetch needed
  :config
  ;; Custom function to recognize specific directories as projects
  (defun my-project-find-root (dir)
    "Identify project roots in ~/Code and ~/.config/."
    (let ((roots '("~/Code" "~/.config")))
      (when (member (file-truename (expand-file-name dir))
                    (mapcar #'file-truename (mapcar #'expand-file-name roots)))
        (cons 'transient dir))))
  (add-hook 'project-find-functions #'my-project-find-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Eglot (LSP) Setup                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :hook ((prog-mode . (lambda ()
                        (unless (string-match-p "^\\*.*\\*$" (buffer-name))
                          (eglot-ensure)))))
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
              (eglot-inlay-hints-mode))))

;; consult-lsp
(use-package consult-lsp
  :ensure t
  :after (eglot consult)
  :bind (:map eglot-mode-map
              ("C-c l a" . consult-lsp-code-actions)
              ("C-c l d" . consult-lsp-diagnostics)
              ("C-c l s" . consult-lsp-symbols)
              ("C-c l f" . consult-lsp-file-symbols)
              ("C-c l i" . consult-lsp-implementation)
              ("C-c l r" . consult-lsp-references)
              ("C-c l D" . consult-lsp-definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Org Mode Setup                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)
(require 'org-agenda)

;; Basic Org Settings
(setq org-directory "~/.org/")
(setq org-agenda-files (list (expand-file-name "agenda.org" org-directory)
                             (expand-file-name "todos.org" org-directory)
                             (expand-file-name "url.org" org-directory)))  ; Include url.org for agenda
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-id-link-to-org-use-id t)  ; Enable IDs for linking
(setq org-startup-folded 'overview)
(setq org-log-done 'time)
(setq org-tag-alist '(("@personal" . ?p) ("@work" . ?w)))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;;(add-hook 'org-mode-hook #'visual-wrap-prefix-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
;; Enable word wrap and disable line numbers in Org-mode
(add-hook 'org-mode-hook (lambda ()
                           (visual-line-mode 1)          ; Enable word wrapping
                           (display-line-numbers-mode 0) ; Disable line numbers
                           ))
(add-hook 'org-mode-hook #'org-indent-mode)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook (lambda () (org-toggle-pretty-entities)))
(setq org-startup-with-inline-images t)

(setq org-ellipsis " ▾")
(setq org-return-follows-link t)
(add-hook 'org-mode-hook (lambda () (setq-local tab-always-indent t)))

(define-key org-mode-map (kbd "TAB") 'org-cycle)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;;(when (boundp 'treesit-language-source-alist)
;;  (add-to-list 'major-mode-remap-alist '(org-mode . org-ts-mode)))

;; Automatically kill file when aborting capture
(defun my-org-capture-delete-file-after-kill (&rest _)
  "If current buffer has a file, delete it when capture is killed."
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
    (delete-file (buffer-file-name))
    (message "Deleted aborted capture file: %s" (buffer-file-name))))
(advice-add 'org-capture-kill :after #'my-org-capture-delete-file-after-kill)

(use-package adaptive-wrap
  :ensure t
  :hook (org-mode . adaptive-wrap-prefix-mode))

;; Org-auto-tangle
(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode))

;; Org-download
(use-package org-download
  :ensure t
  :config
  (require 'org-download)
  (add-hook 'dired-mode-hook #'org-download-enable)
  (setq org-download-image-dir (expand-file-name "images" org-directory))
  :bind (:map org-mode-map
              ("C-c n O" . org-download-clipboard)))

;; Org-protocol
(require 'org-protocol)

;; Org-capture Templates (preserving your originals + new ones)
(setq org-capture-templates
      '(("L" "Link Capture" entry
         (file (lambda () (expand-file-name "url.org" org-directory)))
         "* %:description :web:%^g\n:PROPERTIES:\n:URL: %:link\n:ADDED: %U\n:END:\n"
         :immediate-finish t)
        ("P" "Selection Capture" entry
         (file (lambda () (expand-file-name "url.org" org-directory)))
         "* %:description :web:%^g\n:PROPERTIES:\n:URL: %:link\n:ADDED: %U\n:END:\n#+BEGIN_QUOTE\n%:initial\n#+END_QUOTE")
        ("I" "Image Capture" entry
         (file (lambda () (expand-file-name "url.org" org-directory)))
         "* %:description :web:%^g\n:PROPERTIES:\n:URL: %:link\n:ADDED: %U\n:END:\n%(org-download-clipboard)"
         :immediate-finish t)
        ("n" "Note Capture" entry
         (file (lambda () (expand-file-name "url.org" org-directory)))
         "* %:description :web:%^g\n:PROPERTIES:\n:ADDED: %U\n:END:\n%:initial")
        ("t" "TODO Capture" entry
         (file (lambda () (expand-file-name "url.org" org-directory)))
         "* TODO %:description :web:%^g\n:PROPERTIES:\n:ADDED: %U\n:END:\n%:initial")
        ;; New templates for agenda/journal
        ("m" "Meeting" entry
         (file+headline (lambda () (expand-file-name "agenda.org" org-directory)) "Meetings")
         "* MEETING %^{Title} %^g\nSCHEDULED: %^{Date and Time}T\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?"
         :empty-lines 1)
        ("s" "Scheduled Todo" entry
         (file+headline (lambda () (expand-file-name "todos.org" org-directory)) "Tasks")
         "* TODO %^{Title} %^g\nSCHEDULED: %^{Date}T\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?"
         :empty-lines 1)
        ("u" "Unscheduled Todo" entry
         (file+headline (lambda () (expand-file-name "todos.org" org-directory)) "Tasks")
         "* TODO %^{Title} %^g\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?"
         :empty-lines 1)
        ("j" "Journal Entry" entry
         (function org-journal-new-entry)
         "* %<%H:%M> %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n"
         :empty-lines 1)))

;; Add IDs for stable linking
(add-hook 'org-capture-prepare-finalize-hook #'org-id-get-create)

;; Org-timeblock
(use-package org-timeblock
  :ensure t
  :vc (:url "https://github.com/ichernyshovvv/org-timeblock")
  :bind (("C-c w" . org-timeblock)))

;; Org-agenda Configuration
(setq org-agenda-start-on-weekday 1)  ; Start on Monday
(setq org-agenda-span 'week)
(setq org-agenda-include-diary t)
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down tag-up)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))
(setq org-agenda-log-mode-items '(closed)
      org-agenda-start-with-log-mode t)

(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Org-super-agenda
(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :init
  (org-super-agenda-mode 1)
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-super-agenda-groups
        '((:name "Personal Scheduled"
                 :and (:tag ("personal") :scheduled t))
          (:name "Personal Unscheduled"
                 :and (:tag ("personal") :not (:scheduled t)))
          (:name "Work Scheduled"
                 :and (:tag ("work") :scheduled t))
          (:name "Work Unscheduled"
                 :and (:tag ("work") :not (:scheduled t)))
          (:name "General Scheduled"
                 :and (:not (:tag ("personal" "work")) :scheduled t))
          (:name "General Unscheduled"
                 :and (:not (:tag ("personal" "work")) :not (:scheduled t)))
          (:name "Overdue" :deadline past)
          (:name "Completed Today" :and (:todo "DONE" :scheduled today))))
  (setq org-agenda-custom-commands
        '(("A" "Super Agenda"
           ((agenda "" ((org-agenda-span 'week)))
            (alltodo ""))
           ((org-super-agenda-groups org-super-agenda-groups))))))

;; Org-journal Setup
(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir (expand-file-name "journal/" org-directory))
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-W%W.org")  ; e.g., 2025-W08.org
  (setq org-journal-date-format "%A, %Y-%m-%d")
  (setq org-journal-time-prefix "** ")
  :config
  (defun org-journal-new-entry ()
    "Open or create today's journal entry in the weekly file."
    (interactive)
    (org-journal-new-entry t)
    (goto-char (point-max))))

;; Org Keybindings
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c C-t") #'org-todo-list)


;; Modern Org Look
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table-vertical 2
        org-modern-table-horizontal 2
        org-modern-star ["●" "○" "✸" "✿"]
        org-modern-list '((43 . "•") (45 . "–") (42 . "•"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Org Keybindings and Utils                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybindings
(global-set-key (kbd "C-c C-s") 'org-schedule)          ;; Schedule item under cursor
(global-set-key (kbd "C-c C-d") 'org-deadline)          ;; Set deadline for item
;;(global-set-key (kbd "C-c t")   'org-toggle-checkbox)   ;; Toggle checkbox state
(global-set-key (kbd "C-c n t") 'org-time-stamp)        ;; Insert timestamp
(global-set-key (kbd "C-c n a") 'org-agenda-list)       ;; Show agenda list (all scheduled)
(global-set-key (kbd "C-c n j") 'org-journal-new-entry)  ;; Quick journal entry (rebinds your C-c j)
(global-set-key (kbd "C-c n s") 'org-search-view)       ;; Search all agenda files

;; Bindings in Org-mode only
(define-key org-mode-map (kbd "C-c C-x C-r") 'org-clock-report)  ;; Clock report in buffer
(define-key org-mode-map (kbd "C-c C-x C-i") 'org-clock-in)      ;; Clock in to task
(define-key org-mode-map (kbd "C-c C-x C-o") 'org-clock-out)     ;; Clock out of task

;; Utility Functions
(defun my-org-refile-to-todos ()
  "Refile current heading to todos.org under 'Tasks'."
  (interactive)
  (org-refile nil nil (list "Tasks" (expand-file-name "todos.org" org-directory) nil nil)))
(global-set-key (kbd "C-c C-x r") 'my-org-refile-to-todos)

(defun my-org-agenda-today ()
  "Show agenda for today only."
  (interactive)
  (org-agenda nil "a")
  (org-agenda-day-view))
(global-set-key (kbd "C-c n d") 'my-org-agenda-today)

(defun my-org-capture-note-quick ()
  "Quickly capture a note without switching buffers."
  (interactive)
  (org-capture nil "n"))
(global-set-key (kbd "C-c n n") 'my-org-capture-note-quick)

;; Enhance org-agenda with a custom jump to current task
(defun my-org-agenda-goto-current-clock ()
  "Jump to the currently clocked task in agenda."
  (interactive)
  (org-agenda nil "a")
  (org-agenda-goto (org-clock-is-active)))
(global-set-key (kbd "C-c n c") 'my-org-agenda-goto-current-clock)

;; Configuration tweaks for these bindings
(setq org-clock-persist 'history)  ;; Save clock history across sessions
(org-clock-persistence-insinuate)  ;; Enable clock persistence

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Magit/Forge                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prefer remote upstream for magit
(setq magit-prefer-remote-upstream t)

(use-package magit :defer t)
(use-package forge :defer t)

(keymap-global-set "C-c G"  'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                      eshell                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my-eshell-buffer nil
  "Stores the buffer name of our persistent eshell.")

(defun toggle-eshell ()
  "Toggle a persistent eshell buffer."
  (interactive)
  (if (and my-eshell-buffer
           (buffer-live-p my-eshell-buffer)
           (eq my-eshell-buffer (current-buffer)))
      ;; If we're in the eshell buffer, hide it
      (progn
        (switch-to-prev-buffer)
        (bury-buffer my-eshell-buffer))
    ;; Otherwise, show or create eshell
    (if (and my-eshell-buffer
             (buffer-live-p my-eshell-buffer))
        (switch-to-buffer my-eshell-buffer)
      (setq my-eshell-buffer (eshell)))))

;; Bind the toggle function instead of plain eshell
(global-set-key (kbd "C-c t") 'toggle-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Grep Ignorance                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package grep
  :config
  (setq grep-find-ignored-directories
        (append '(".angular" ".git" ".hg" ".idea" ".project" ".settings"
                  ".svn" "3rdparty" "bootstrap*" "pyenv" "target")
                grep-find-ignored-directories))
  (setq grep-find-ignored-files
        (append '("*.blob" "*.class" "*.gz" "*.jar" "*.pack" "*.xd"
                  ".factorypath" "TAGS" "dependency-reduced-pom.xml"
                  "projectile.cache" "workbench.xmi")
                grep-find-ignored-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Copilot for AI Suggestions                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el.git")
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB"   . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word))
  :config
  (setq copilot-idle-delay 1.0
        copilot-log-max 10000
        copilot-max-char 250000)
  ;; Indentation alist
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-hook 'prog-mode-hook 'copilot-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           AIdermacs (Anthropic)                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aidermacs
  :vc (:url "https://github.com/MatthewZMD/aidermacs")
  :config
  (setq aidermacs-default-model "anthropic/claude-3-7-sonnet-20250219")
  (global-set-key (kbd "C-c A") 'aidermacs-transient-menu)
  ; See the Configuration section below
  (setq aidermacs-auto-commits t)
  (setq aidermacs-use-architect-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Misc Packages                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recentf
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 25
        recentf-exclude '("/tmp/" "/ssh:")))

;; SSH Deploy
(use-package ssh-deploy
  :demand
  :after hydra
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode)
  (ssh-deploy-add-menu)
  (ssh-deploy-hydra "C-c C-z"))

;; Emacs-everywhere
(use-package emacs-everywhere
  :ensure t
  :config
  (defun grim/emacs-everywhere-wayland-app-info ()
    "Return a dummy app info struct for Wayland."
    (make-emacs-everywhere-app
     :id "wayland"
     :class "wayland-app"
     :title "Unknown"
     :geometry '(0 0 800 600)))
  (setq emacs-everywhere-app-info-function #'grim/emacs-everywhere-wayland-app-info
        emacs-everywhere-copy-command '("wl-copy")
        emacs-everywhere-paste-command '("wl-paste")))

;; Basic screenshot function (Wayland example)
(defun grim/screenshot (&optional type)
  "Export current frame as screenshot to clipboard using TYPE format (png/svg/pdf/postscript)."
  (interactive (list
                (intern (completing-read
                         "Screenshot type: "
                         '(png svg pdf postscript)))))
  (let* ((extension (pcase type
                      ('png        ".png")
                      ('svg        ".svg")
                      ('pdf        ".pdf")
                      ('postscript ".ps")
                      (_ (error "Unsupported screenshot type: %s" type))))
         (filename (make-temp-file "Emacs-" nil extension))
         (data     (x-export-frames nil type)))
    (with-temp-file filename
      (insert data))
    (when (executable-find "wl-copy")
      (with-temp-buffer
        (insert-file-contents filename)
        (call-process-region (point-min) (point-max)
                             "wl-copy" nil nil nil "-t"
                             (format "image/%s" (substring extension 1)))))
    (set-register ?s filename)
    (message "Screenshot copied to clipboard and saved to %s" filename)))

(global-set-key (kbd "s-s") #'grim/screenshot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     eww                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eww
  :commands (eww-browse-url)
  :init
  (defun my-open-with-mpv (url &rest args)
    "Open URL with mpv."
    (start-process "mpv" nil "mpv" url))
  ;; Define browse-url handlers for EWW and PDFs
  (setq browse-url-handlers
        '(("[^/]*\\.\\(mp4\\|mkv\\|webm\\|avi\\|mov\\|flv\\|wmv\\|mpg\\|mpeg\\)\\([?#].*\\)?$" . my-open-with-mpv)
          ("^https?://\\(www\\.youtube\\.com\\|youtu\\.be\\|vimeo\\.com\\)/" . my-open-with-mpv)
          ("\\.pdf\\'" . my-open-remote-pdf-in-emacs)
          ("^https?://" . eww-browse-url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                mpv & emacs.tv                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mpv
  :ensure t
  :defer t)

(use-package emacstv
  :defer t
  :vc (:url "https://github.com/emacstv/emacstv.github.io"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     pdf                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :ensure t  ;; Using straight.el instead of package.el
  :mode ("\\.pdf\\'" . pdf-view-mode)  ;; Automatically use pdf-view-mode for PDFs
  :init
  ;; Define the remote PDF function before pdf-tools loads
  (defun my-open-remote-pdf-in-emacs (url &rest args)
    "Download a PDF from URL and open it in Emacs with pdf-view-mode."
    (interactive)
    (let ((temp-file (make-temp-file "emacs-pdf-" nil ".pdf")))
      (url-copy-file url temp-file t)
      (find-file temp-file)
      (delete-file temp-file)))
  :config
  ;; Install pdf-tools and configure it
  (pdf-tools-install :no-query)  ;; Install without prompting
  (setq pdf-view-display-size 'fit-page  ;; Fit page by default
        pdf-view-continuous t            ;; Continuous scrolling
        pdf-view-use-scaling t)          ;; Better rendering on HiDPI
  (add-to-list 'pdf-view-incompatible-modes 'display-line-numbers-mode)
  ;; Explicitly disable line numbers in pdf-view-mode
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ERC (IRC Client)                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package erc
  :defer t
  :config
  (defun my-erc-set-fill-column ()
    "Set ERC fill column based on display type."
    (setq-local erc-fill-column
                (if (display-graphic-p)
                    (window-width)  ; GUI: Use full window width
                  (min 80 (window-width)))))  ; Terminal: Cap at 80
  (add-hook 'erc-mode-hook #'my-erc-set-fill-column)
  (setq erc-track-remove-disconnected-buffers t
        erc-hide-list '("PART" "QUIT" "JOIN")
        erc-interpret-mirc-color t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t
        erc-track-shorten-start 8
        erc-kill-buffer-on-part t
        erc-auto-query 'bury
        erc-prompt (lambda () (concat (propertize "ERC> " 'face '(:foreground "cyan" :weight bold))
                                     (buffer-name)))
        erc-timestamp-format "[%H:%M] "
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-track-position-in-mode-line t
        erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE")
        erc-track-switch-direction 'newest
        erc-track-visibility 'visible
        erc-track-showcount t
        erc-format-query-as-channel-p t
        erc-fill-function 'erc-fill-variable
        erc-fill-prefix "  "
        erc-fill-static-center 20
        erc-log-channels-directory "~/.config/emacs/irc-logs/"
        erc-save-buffer-on-part t
        erc-log-write-after-insert t)
  ;; Enable ERC modules, including 'networks' but excluding 'nickbar'
  (setq erc-modules '(networks notifications))  ; Add 'networks' explicitly
  (erc-update-modules)

  (defun my-erc-update-notifications-keywords (&rest _)
    "Update notification keywords with current nick."
    (when erc-session-user
      (setq erc-notifications-keywords (list erc-session-user))))
  (add-hook 'erc-nick-changed-functions #'my-erc-update-notifications-keywords)
  ;; Initialize with a default or leave it nil until connected
  (setq erc-notifications-keywords nil)

  (erc-timestamp-mode 1)
  (erc-track-mode 1)
  (erc-autojoin-mode 1)
  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
  (add-hook 'erc-mode-hook (lambda ()
                             (require 'erc-pcomplete)
                             (pcomplete-erc-setup)
                             (erc-completion-mode 1)))
  (require 'erc-button)
  (erc-button-mode 1)
  (setq erc-button-url-open-function 'eww-browse-url)
  (set-face-attribute 'erc-nick-default-face nil :foreground "#bd93f9")
  (set-face-attribute 'erc-timestamp-face nil :foreground "#6272a4")
  (set-face-attribute 'erc-my-nick-face nil :foreground "#ff79c6" :weight 'bold)
  :bind (:map erc-mode-map
              ("C-c e" . erc-button-browse-url)
              ("C-c l" . erc-view-log-mode))
  :init
  (defun my-erc-connect ()
    "Retrieve IRC credentials from authinfo.gpg and connect to the IRC server"
    (interactive)
    (let* ((host "samhain.su")
           (port "7000")
           (auth-entry (car (auth-source-search :host host
                                                :port port
                                                :require '(:user :secret)
                                                :max 1)))
           (username (plist-get auth-entry :user))
           (password (if (functionp (plist-get auth-entry :secret))
                         (funcall (plist-get auth-entry :secret))
                       (plist-get auth-entry :secret))))
      (unless (and username password)
        (error "Could not retrieve IRC credentials from authinfo.gpg"))
      (erc-tls :server host
               :port (string-to-number port)
               :nick username
               :password password
               :full-name "blackdream")))
  :bind (:map global-map
              ("C-c E" . my-erc-connect)))

(use-package erc-hl-nicks
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'hl-nicks)
  (erc-update-modules))

(use-package erc-image
  :ensure t
  :after erc
  :config
  (setq erc-image-inline-rescale 300)
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Elfeed + Dashboard                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aio
  :ensure t)  ; Fetch from MELPA

(use-package elfeed-tube
  :vc (:url "https://github.com/karthink/elfeed-tube")
  :after elfeed
  :config
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
              ("v" . elfeed-tube-mpv)  ;; Play video in mpv
              ("f" . elfeed-tube-mpv-follow-mode)))  ;; Follow mode for live control

(use-package elfeed
  :ensure t
  :defer t
  :hook ((elfeed-search-mode-hook . (lambda ()
                                      (elfeed-update)  ;; Run update on opening
                                      (my-elfeed-start-auto-update)))  ;; Start timer
         (elfeed-show-mode . (lambda () (setq shr-external-browser 'eww-browse-url))))
  :bind (:map elfeed-search-mode-map
              ("q" . my-elfeed-quit-and-stop-timer)  ;; Custom quit to stop timer
              :map elfeed-show-mode-map
              ("RET" . shr-browse-url)
              ("v"   . elfeed-tube-mpv))
  :init
  ;; Timer variable for auto-update
  (defvar my-elfeed-update-timer nil
    "Timer for Elfeed auto-updates.")
  ;; Function to start auto-update
  (defun my-elfeed-start-auto-update ()
    "Start auto-updating Elfeed every 30 minutes if search buffer is visible."
    (interactive)
    (unless my-elfeed-update-timer
      (setq my-elfeed-update-timer
            (run-at-time t 1800  ;; 1800 seconds = 30 minutes
                         (lambda ()
                           (when (get-buffer-window "*elfeed-search*" t)
                             (elfeed-update)
                             (my-elfeed-update-title)))))))
  ;; Function to stop auto-update
  (defun my-elfeed-stop-auto-update ()
    "Stop Elfeed auto-update timer."
    (interactive)
    (when my-elfeed-update-timer
      (cancel-timer my-elfeed-update-timer)
      (setq my-elfeed-update-timer nil)))
  ;; Custom quit function
  (defun my-elfeed-quit-and-stop-timer ()
    "Quit Elfeed and stop the auto-update timer."
    (interactive)
    (elfeed-search-quit-window)
    (my-elfeed-stop-auto-update))
  ;; Function to update buffer title with new articles
  (defun my-elfeed-update-title ()
    "Update Elfeed search buffer title with new article count."
    (interactive)
    (with-current-buffer "*elfeed-search*"
      (let ((new-count (length (elfeed-search-filter "unread"))))
        (rename-buffer (format "*elfeed-search* (%d new)" new-count)))))
  :config
  (setq shr-external-browser 'eww-browse-url)

(setq elfeed-feeds
      '("https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Fsachachua.com%2Fblog%2Ffeed%2Findex.xml&key=1&hash=48ac81675b0797fda5d8f4f189846563a5ed14d9&max=1000&links=preserve&exc="
        "https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Fddosecrets.com%2Farticle%2Flexipolleaks&key=1&hash=b9258f920d5b200034edd73868c42b1e68284695&max=1000&links=preserve&exc="
        "https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Fhnrss.org%2Fnewest&key=1&hash=62a3abd97ca026dbb64c82151d396f32e4c6a4fb&max=1000&links=preserve&exc="
        "https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Ffeeds.bbci.co.uk%2Fnews%2Frss.xml&key=1&hash=78370b961d44c8bff594b1b41a513762d6f34560&max=1000&links=preserve&exc="
        "https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Fplanet.emacslife.com%2Fatom.xml&key=1&hash=f609b22f1328308f3361f85a9b50c9eda53bfb6d&max=1000&links=preserve&exc=1")))

(use-package elfeed-dashboard
  :config
  (setq elfeed-dashboard-file (concat org-directory "elfeed-dashboard.org"))
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Tree-sitter-based Modes                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs 29 and above has built-in tree-sitter support via `treesit`.
;; This section remaps major modes to their new tree-sitter variants.
;; You may need to install language grammars with `M-x treesit-install-language-grammar`
;; or set them up manually. Once installed, these new modes provide
;; improved syntax highlighting and parsing features for many languages.

;; folding
(when (boundp 'treesit-language-source-alist)  ;; Emacs 29+ check
  ;; Optional package for automatically installing & configuring grammars:
  (use-package treesit-auto
    :ensure t
    :config
      (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode)))

;; If you prefer manual control over which modes switch to Tree-sitter:
(setq major-mode-remap
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (c-or-c++-mode   . c-or-c++-ts-mode)
        (python-mode     . python-ts-mode)
        (js-mode         . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode       . json-ts-mode)
        (css-mode        . css-ts-mode)
        (sh-mode         . bash-ts-mode)
        (org-mode        . org-ts-mode)
        (rust-mode       . rust-ts-mode)
        ;; Add others as grammars become available
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                So-long-mode                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `so-long-mode` is built into Emacs 29+. It automatically detects
;; extremely long lines (think minified code or large log files)
;; and switches Emacs into a more performant mode, preventing
;; slow rendering or freezing.
;;
;; Usage:
;;  - Enable globally:  (global-so-long-mode 1)
;;  - Once enabled, Emacs will automatically enter `so-long-mode`
;;    for buffers with extremely long lines. This typically disables
;;    certain expensive UI features and speeds up editing.
;;  - If you do NOT want it global, you can hook it into specific modes
;;    or only enable it manually with M-x so-long-mode.

(global-so-long-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 dired                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired-preview
  :ensure t
  :init
  (dired-preview-global-mode 1))

(use-package dired-git-info
  :ensure t
  :config
  (setq dgi-auto-hide-details-p nil)
  (with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Electric-quote-mode                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built into Emacs 29+, `electric-quote-mode` automatically transforms
;; certain ASCII quotes, dashes, and ellipses into their typographically
;; preferred characters. For example, typing "..." becomes …,
;; typing " - " can become – (en dash), and so forth.
;;
;; Usage:
;;  - Enable globally:    (electric-quote-mode 1)
;;  - Or just in text:    (add-hook 'text-mode-hook #'electric-quote-mode)
;;  - Customize behavior: M-x customize-group RET electric-quote RET
;;    (e.g., set `electric-quote-context-sensitive` to t)
;;
;; This is especially handy for prose, Org-mode notes, etc.
;; If you prefer raw ASCII punctuation (e.g., in code), disable or
;; keep it confined to your writing modes.

(setq electric-quote-context-sensitive t)
(electric-quote-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           calibre / nov.el                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package calibredb
  :ensure t
  :defer t
  :config
  (setq calibredb-format-nerd-icons t)
  (setq calibredb-format-character-icons t)
  (setq calibredb-root-dir "~/.calibre-library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/.calibre-library" (name . "Calibre")))))

(use-package esxml
  :ensure t)

(use-package nov
  :ensure t
  :after esxml
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename))
  (setq nov-verbose t))  ;; Temporary for debugging

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Treemacs Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :init
  ;; Ensure that when you switch or open a project, Treemacs is displayed.
  :config
  ;; Open Treemacs automatically when Emacs starts up, if you prefer:
  ;; (treemacs)

  ;; Follow the current file in Treemacs automatically
  (treemacs-follow-mode t)

  ;; Optionally show git status colors asynchronously
  (treemacs-git-mode 'deferred)

  ;; You can tweak other Treemacs settings here:
  (setq treemacs-width 35
        treemacs-collapse-dirs 3
        treemacs-file-event-delay 2000
        treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-recenter-after-project-jump 'always))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :config
  ;; If you want a keybinding to quickly open Treemacs:
  (global-set-key (kbd "C-c T") #'treemacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Flymake Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flymake
  :ensure nil  ; Built-in
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  ;; Enable Elisp backends
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile nil t)
              (add-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc nil t))))

;; Optionally, if you want linting in all prog modes even outside Eglot:
(add-hook 'prog-mode-hook #'flymake-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        snippets (Yasnippet)                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :init
  ;; Enable globally at startup
  (yas-global-mode 1)
  :custom
  ;; Explicitly set snippet directories
  (yas-snippet-dirs
   (list (expand-file-name "snippets/" user-emacs-directory)  ;; Personal snippets
         (expand-file-name "yasnippet-snippets/snippets/" user-emacs-directory)))  ;; Pre-built snippets
  ;; Optional: Show snippet suggestions in completion frameworks like Corfu
  (yas-prompt-functions '(yas-completing-prompt yas-ido-prompt yas-no-prompt))
  :config
  ;; Reload snippets after configuration to ensure they're picked up
  (yas-reload-all)
  ;; Optional: Bind a key to insert snippets manually
  :bind (:map yas-minor-mode-map
              ("C-c y" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t
  :config
  ;; Ensure snippets are loaded when this package is loaded
  (yas-reload-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                elisp coding                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-key]      . helpful-key))

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               email/mu4e                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package mu4e
;;   :ensure nil
;;   :ensure-system-package mu
;;   :load-path "/usr/share/emacs/site-lisp/mu4e"
;;   :bind (("C-c M" . mu4e)
;;          :map mu4e-view-mode-map
;;          ("n"         . next-line)
;;          ("p"         . previous-line)
;;          ("<tab>"     . org-next-link)
;;          ("<backtab>" . org-previous-link)
;;          ("<RET>"     . mu4e~view-browse-url-from-binding))
;;   :hook (mu4e-compose-mode
;;          . (lambda ()
;;              (flyspell-mode)
;;              (auto-fill-mode -1)
;;              (display-line-numbers-mode -1)))
;;   :custom
;;   (mail-user-agent 'mu4e-user-agent)
;;   (mu4e-get-mail-command "mbsync -c ~/.mbsyncrc -a")
;;   (mu4e-update-interval 600)
;;   (mu4e-split-view nil)
;;   (mu4e-confirm-quit nil)
;;   (mu4e-use-fancy-chars t)
;;   (mu4e-view-show-images t)
;;   (mu4e-view-prefer-html t)
;;   (mu4e-view-show-addresses t)
;;   (mu4e-hide-index-messages t)
;;   (mu4e-attachment-dir "~/Downloads")
;;   (mu4e-compose-dont-reply-to-self t)
;;   (mu4e-change-filenames-when-moving t)
;;   (mu4e-sent-messages-behavior 'delete)
;;   (mu4e-index-update-error-warning nil)
;;   (mu4e-html2text-command "w3m -dump -I utf-8 -O utf-8 -T text/html"))

;; (use-package mu4e-headers
;;   :ensure nil
;;   :hook (mu4e-headers-mode . (lambda () (eldoc-mode -1)))
;;   :custom
;;   (mu4e-headers-auto-update t)
;;   (mu4e-headers-fields `((:human-date . 12)
;;                          (:flags      .  6)
;;                          (:from       . 22)
;;                          (:subject    . ,(- (window-body-width) 50))))
;;   :config
;;   (setq mu4e-headers-attach-mark '("a" . "📎")))

;; (use-package message
;;   :ensure nil
;;   :custom
;;   (message-kill-buffer-on-exit t)
;;   (message-send-mail-function 'smtpmail-send-it))

;; (use-package smtpmail
;;   :ensure nil
;;   :custom
;;   (smtpmail-smtp-service 587)
;;   (smtpmail-smtp-server "smtp.office365.com")
;;   (setq user-mail-address "my@email.com")
;;   (setq smtpmail-auth-credentials
;;       '(("jcubic.<server>" 465 "jcubic@<server>" "<password>")))
;;   (setq smtpmail-stream-type 'starttls)
;;   (setq smtpmail-debug-info t)

;;   )

;; (use-package org-mime
;;   :defer t
;;   :config
;;   (setq org-mime-export-options '(:section-numbers nil
;;                                   :with-author nil
;;                                   :with-toc nil)))

;; (use-package mu4e-context
;;   :ensure nil
;;   :custom
;;   (mu4e-context-policy 'pick-first)
;;   (mu4e-compose-context-policy 'always-ask)
;;   :config
;;   (setq mu4e-contexts
;;         (list
;;          (make-mu4e-context
;;           ;; Personal context
;;           :name "personal"
;;           :enter-func (lambda () (mu4e-message "Entering personal context"))
;;           :match-func (lambda (msg)
;;                         (when msg
;;                           (mu4e-message-contact-field-matches
;;                            msg '(:from :to :cc :bcc) "zoliky@gmail.com")))
;;           :vars '((user-mail-address  . "zoliky@gmail.com")
;;                   (user-full-name     . "Zoltan Kiraly")
;;                   (mu4e-sent-folder   . "/gmail-zoliky/[Gmail].Sent Mail")
;;                   (mu4e-drafts-folder . "/gmail-zoliky/[Gmail].Drafts")
;;                   (mu4e-trash-folder  . "/gmail-zoliky/[Gmail].Trash")
;;                   (smtpmail-queue-dir . "~/Maildir/gmail-zoliky/queue/cur")
;;                   (smtpmail-smtp-user . "zoliky")
;;                   (mu4e-maildir-shortcuts
;;                    . ((:maildir "/gmail-zoliky/INBOX"             :key ?i)
;;                       (:maildir "/gmail-zoliky/[Gmail].Starred"   :key ?r)
;;                       (:maildir "/gmail-zoliky/[Gmail].Sent Mail" :key ?s)
;;                       (:maildir "/gmail-zoliky/[Gmail].Drafts"    :key ?d)
;;                       (:maildir "/gmail-zoliky/[Gmail].Trash"     :key ?t)))))
;;          (make-mu4e-context
;;           ;; Work context
;;           :name "work"
;;           :enter-func (lambda () (mu4e-message "Entering work context"))
;;           :match-func (lambda (msg)
;;                         (when msg
;;                           (mu4e-message-contact-field-matches
;;                            msg '(:from :to :cc :bcc) "zolikydev@gmail.com")))
;;           :vars '((user-mail-address  . "zolikydev@gmail.com")
;;                   (user-full-name     . "Zoltan Kiraly")
;;                   (mu4e-sent-folder   . "/gmail-zolikydev/[Gmail].Sent Mail")
;;                   (mu4e-drafts-folder . "/gmail-zolikydev/[Gmail].Drafts")
;;                   (mu4e-trash-folder  . "/gmail-zolikydev/[Gmail].Trash")
;;                   (smtpmail-queue-dir . "~/Maildir/gmail-zolikydev/queue/cur")
;;                   (smtpmail-smtp-user . "zolikydev")
;;                   (mu4e-maildir-shortcuts
;;                    . ((:maildir "/gmail-zolikydev/INBOX"             :key ?i)
;;                       (:maildir "/gmail-zolikydev/[Gmail].Starred"   :key ?r)
;;                       (:maildir "/gmail-zolikydev/[Gmail].Sent Mail" :key ?s)
;;                       (:maildir "/gmail-zolikydev/[Gmail].Drafts"    :key ?d)
;;                       (:maildir "/gmail-zolikydev/[Gmail].Trash"     :key ?t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   GNUS                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package oauth2
  :ensure t)

(use-package auth-source-xoauth2-plugin
  :after oauth2
  :ensure t
  :config
  (add-to-list 'auth-sources 'xoauth2 'append)
  (setq auth-source-xoauth2-creds
        `(("tj.theesfeld@citywide.io@outlook.office365.com" ,(lambda (host port user)
                                                               (message "Fetching XOAUTH2 for %s:%s:%s" host port user)
                                                               (list :client-id "08162f7c-0fd2-4200-a84a-f25a4db0b584"
                                                                     :client-secret "TxRBilcHdC6WGBee]fs?QR:SJ8nI[g82"
                                                                     :auth-url "https://login.microsoftonline.com/common/oauth2/v2.0/authorize"
                                                                     :token-url "https://login.microsoftonline.com/common/oauth2/v2.0/token"
                                                                     :scope "https://outlook.office.com/IMAP.AccessAsUser.All offline_access"
                                                                     :redirect-uri "http://localhost"))))))
(require 'gnus)
  (setq gnus-select-method '((nnimap "outlook365"
                                     (nnimap-address "outlook.office365.com")
                                     (nnimap-server-port 993)
                                     (nnimap-stream ssl)
                                     (nnimap-authenticator xoauth2)
                                     (nnimap-user "tj.theesfeld@citywide.io")
                                     (nnimap-authinfo-function #'auth-source-xoauth2--get-authinfo)
                                     (nnimap-expunge t)))
        gnus-secondary-select-methods '((nnrss "feeds"
                                               (nnrss-file "~/.config/emacs/gnus/rss-feeds.el")))
        gnus-verbose 10
        gnus-verbose-backends t
        gnus-summary-line-format "%U%R%z %d %I%(%[%4L: %-23,23f%]%) %s\n"
        gnus-group-line-format "%M%S%p%P%5y: %(%g%)%l\n"
        gnus-asynchronous t
        gnus-use-adaptive-scoring t
        gnus-use-cache t
        gnus-cache-directory "~/.config/emacs/gnus/cache/"
        gnus-article-save-directory "~/.config/emacs/gnus/saved/"
        gnus-read-active-file 'some
        gnus-check-new-newsgroups nil
        gnus-save-newsrc-file t
        gnus-read-newsrc-file t
        gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date
                                     gnus-thread-sort-by-number)
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-treat-fill-long-lines t
        gnus-treat-display-smileys t
        gnus-treat-emphasize t
        gnus-startup-file "~/.config/emacs/gnus/newsrc"
        gnus-save-killed-list nil
        gnus-use-dribble-file nil
        smtpmail-smtp-user "tj.theesfeld@citywide.io"
        smtpmail-smtp-server "smtp.office365.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-auth-supported '(xoauth2)
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        auth-sources '(xoauth2)
        auth-source-debug t
        gnus-draft-mode 'nnimap
        gnus-drafts-directory "outlook365:Drafts"
        browse-url-browser-function 'ignore)

;; (add-hook 'gnus-group-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "g") 'gnus-group-get-new-news)))
;; (add-hook 'gnus-summary-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "m") 'gnus-summary-mail-other-window)))

;; (defun test-xoauth2-token ()
;;   (interactive)
;;   (message "Token: %s" (auth-source-xoauth2--access-token "outlook365" "outlook.office365.com" "993" "tj.theesfeld@citywide.io")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   0x0.st                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package 0x0
  :ensure t
  :bind (:map global-map
              ("C-c u" . '0x0-dwim)
              ("C-c U" . '0x0-upload-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     EXWM                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Only load EXWM in Xorg graphical environments
(when (and (display-graphic-p)         ; Check if graphical environment
           (getenv "DISPLAY")          ; Ensure Xorg via DISPLAY variable
           (not (getenv "WAYLAND_DISPLAY"))) ; Exclude Wayland
  (require 'exwm)
  (require 'exwm-config)              ; Optional: loads default EXWM helpers

  ;; Basic EXWM setup
  (setq exwm-workspace-number 4)      ; Set number of workspaces
  (exwm-enable)                       ; Start EXWM

  ;; EXWM-specific keybindings
  (setq exwm-input-global-keys
        `((,(kbd "s-r") . exwm-reset) ; Reset EXWM layout
          (,(kbd "s-&") . (lambda ()  ; Launch terminal (e.g., alacritty)
                            (interactive)
                            (start-process "" nil "alacritty")))
          (,(kbd "s-<tab>") . exwm-workspace-switch))) ; Switch workspaces

  ;; Optional: Simulation keys for X11 apps (e.g., pass C-c to apps)
  (exwm-input-set-simulation-keys
   '(([?\C-c] . ?\C-c)
     ([?\C-v] . ?\C-v)))

  ;; Optional: Improve EXWM startup logging
  (setq exwm-debug-on t)              ; Enable debug to troubleshoot issues
  (message "EXWM loaded successfully in Xorg session"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Final Cleanup                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; init.el ends here
