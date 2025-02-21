;;; package --- summary
;;; commentary:
;;; code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Early Initial Settings                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use a higher GC threshold to speed up initialization, then revert back.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 8000000))) ;; 800k is a typical default.

;; Inhibit startup screen.
(setq inhibit-startup-message t)

;; Keep the custom variables out of our main init file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Straight and Use-Package                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

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
      org-directory "~/.org/"
      gc-cons-percentage 0.6
      ;; Visual ellipsis for truncated lines:
      truncate-string-ellipsis "…"
      scroll-margin 1
      garbage-collection-messages nil)

(require 'auth-source)
(set-time-zone-rule "America/New_York")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Shell Environment                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Properly set up PATH and environment variables on macOS/Linux.
(use-package exec-path-from-shell
  :config
  (when (or (memq window-system '(mac ns x)) (daemonp))
    ;; Remove "-i" from arguments if needed, so as not to read .bashrc again
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

;; Ensure local bin is in exec-path
(setq exec-path (append exec-path '("~/.local/bin/")))

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
  :straight t
  :ensure nil  ;; Emacs built-in
  :init
  (save-place-mode)
  :config
  (setq save-place-file (expand-file-name ".saveplace" user-emacs-directory)))

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove the menu/tool/scroll bars early for minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; Tidy up whitespace on save
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Keep backups in a dedicated directory
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
;; Do not create TRAMP backups
(with-eval-after-load 'tramp
  (setq tramp-backup-directory-alist nil))

;; Save auto-save files in a dedicated directory
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; Versions of backups
(setq delete-old-versions -1
      version-control t
      vc-make-backup-files t)

;; Savehist
(setq savehist-file "~/.config/emacs/savehist"
      history-length t
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables
      '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)
(setq save-silently t)

;; Keep an eye on undone changes
(setq undo-limit 80000000)

;; Show isearch count
(setq isearch-lazy-count t
      lazy-count-prefix-format nil
      lazy-count-suffix-format "   (%s/%s)")

;; Replacing kill-buffer key
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    vundo                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vundo
  :straight t
  :bind
  ("C-x u" . vundo)
    :config
    (setq vundo-glyph-alist vundo-unicode-symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Visual Enhancements                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dracula theme
(use-package dracula-theme
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
;;                                 iBuffer Setup                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ibuffer
  :straight (:type built-in) ;; ibuffer is part of Emacs
  :bind
  (("C-x C-b" . ibuffer))    ;; Replace default list-buffers with ibuffer
  :config
  ;; Define custom filter groups
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Emacs"
            (or (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")
                (name . "^\\*Warnings\\*$")))
           ("IRC"
            (derived-mode  . erc-mode))
           ("Org"
            (derived-mode . org-mode))
           ("Magit"
            (derived-mode . magit-mode))
           ("Programming"
            (derived-mode . prog-mode))
           ("Dired"
            (derived-mode . dired-mode))
           ("Help"
            (or (name . "^\\*Help\\*$")
                (name . "^\\*Apropos\\*$")
                (name . "^\\*info\\*$"))))))

  ;; When ibuffer opens, apply the custom groups and sort by recency
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-do-sort-by-recency))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Completion Setup                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Corfu: in-buffer completion
(use-package corfu
  :straight (:host github :repo "minad/corfu")
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 1.2)
  :init
  (global-corfu-mode)
  :config
  (defun my-disable-corfu-in-erc ()
    "Disable Corfu in erc-mode derived buffers."
    (corfu-mode -1))

  (add-hook 'erc-mode-hook #'my-disable-corfu-in-erc))

;; Better completion styles
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Terminal-friendly Corfu
(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

;; Turn off automatic corfu in Eshell, but enable on demand
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Vertico + Consult                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

;; Persist minibuffer history over Emacs restarts
(use-package savehist
  :init
  (savehist-mode 1))

;; Directory navigation in Vertico
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Consult
(use-package consult
  :bind (("C-c M-x"   . consult-mode-command)
         ("C-c h"     . consult-history)
         ("C-c k"     . consult-kmacro)
         ("C-c m"     . consult-man)
         ("C-c i"     . consult-info)
         ("C-x M-:"   . consult-complex-command)
         ("C-x b"     . consult-buffer)
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
         ("M-s l"     . consult-line)
         ("M-s L"     . consult-line-multi)))

;; Embark
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :straight t)

;; Marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;; Nerd Icons support for completions
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Editing Helpers                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show uncommitted changes in the gutter
;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode +1)
  ;; disable on slow TRAMP connections with diff-hl-disable-on-remote to t
  )

(use-package which-key
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

(use-package smartparens
  :hook ((prog-mode text-mode markdown-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

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
  :hook ((text-mode . flyspell-mode)
         (org-mode  . flyspell-mode))
  :init
  (cond
   ((executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--camel-case")))
   ((executable-find "hunspell")
    (setq ispell-program-name "hunspell"))
   (t
    (setq ispell-program-name nil)))
  :custom
  (flyspell-default-dictionary "american"))

;;(setq ispell-program-name "aspell"
;;      ispell-silently-savep t)

(use-package flycheck-aspell :defer t)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-lazy
  :after flyspell
  :init
  (flyspell-lazy-mode 1)
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Project                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO does this work
(use-package project
  :straight (:type built-in))

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '(("~/Code" . 1)
                                         ("~/.config/" . 1)))
  :bind-keymap
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map))

(use-package ibuffer-projectile :defer t)
(use-package ibuffer-vc :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Eglot (LSP) Setup                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :straight t
  :hook ((prog-mode . eglot-ensure))
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
              (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  )

;; consult-lsp
(use-package consult-lsp
  :straight t
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
;;                       Org, Org-Roam, Org-Super-Agenda                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatically kill file when aborting capture
(defun my-org-capture-delete-file-after-kill (&rest _)
  "If current buffer has a file, delete it when capture is killed."
  (when (and (buffer-file-name)
             (file-exists-p (buffer-file-name)))
    (delete-file (buffer-file-name))
    (message "Deleted aborted capture file: %s" (buffer-file-name))))

(advice-add 'org-capture-kill :after #'my-org-capture-delete-file-after-kill)

;; Use org-mode for .org files
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-tag-alist '(("@personal" . ?p) ("@work" . ?w))
      org-log-done 'time)

;; org-auto-tangle
(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

;; org-download
(use-package org-download
  :config
  (require 'org-download)
  (add-hook 'dired-mode-hook 'org-download-enable)
  ;; Additional keybind for convenience
  :bind (:map org-mode-map
              ("C-c n O" . org-download-clipboard)))

;; Org-timeblock
(use-package org-timeblock
  :straight (org-timeblock :type git :host github :repo "ichernyshovvv/org-timeblock")
  :bind (("C-c w" . org-timeblock)))

;; Configure Org Agenda
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down tag-up)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))
(setq org-agenda-log-mode-items '(closed)
      org-agenda-start-with-log-mode t)

;; Custom agenda command
(setq org-agenda-custom-commands
      '(("A" "Super Agenda"
         ((agenda "")
          (alltodo "")))))

;; org-super-agenda
(use-package org-super-agenda
  :after org-agenda
  :init
  (org-super-agenda-mode)
  :bind (("C-c g" . my/org-roam-refresh-agenda-list)
         ("C-c a" . org-agenda))
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
                 :and (:not (:tag ("personal" "work"))
                            :scheduled t))
          (:name "General Unscheduled"
                 :and (:not (:tag ("personal" "work"))
                            :not (:scheduled t)))
          (:name "Overdue" :deadline past)
          (:name "Completed Today" :and (:todo "DONE" :scheduled today)))))

;; Org-roam
(use-package org-roam
  :demand t
  :custom
  (org-roam-directory (concat org-directory "/roam"))
  :bind (("C-c n f" . org-roam-node-find)
         ("M-o"     . ugt-org-roam-node-find)
         ("M-O"     . ugt-org-roam-node-find-document-nodes)
         ("C-c n r" . org-roam-node-random)
         ("C-c n n" . org-roam-capture)
         ("C-c n l" . org-roam-buffer-toggle)
         ;; Dailies keybindings:
         ("C-c n d n" . org-roam-dailies-capture-today)
         ("C-c n d x" . org-roam-dailies-goto-today)
         ("C-c n d y" . org-roam-dailies-goto-yesterday)
         ("C-c n d t" . org-roam-dailies-goto-tomorrow)
         ("C-c n d Y" . org-roam-dailies-capture-yesterday)
         ("C-c n d T" . org-roam-dailies-capture-tomorrow)
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n b" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n g" . org-roam-graph))))
  :config
  ;; Display template for nodes
  (setq org-roam-node-display-template
        (concat "${title:70}"
                (propertize "${tags:30}" 'face 'org-tag)
                "${file:48}"))

  ;; Capture templates
  (setq org-roam-capture-templates
        `(("d" "Default (Personal Note)" plain "%?"
           :if-new (file+head
                    (lambda ()
                      (let* ((title (plist-get org-capture-plist :title))
                             (slug (if title (org-roam--slugify title) "untitled"))
                             (timestamp (format-time-string "%Y%m%d%H%M%S")))
                        (format "personal/%s-%s.org" slug timestamp)))
                    "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+startup: showall\n#+filetags: personal\n\n")
           :immediate-finish t
           :jump-to-captured nil
           :empty-lines 1
           :unnarrowed t)
          ("w" "Work Note" plain "%?"
           :if-new (file+head
                    (lambda ()
                      (let* ((title (plist-get org-capture-plist :title))
                             (slug (if title (org-roam--slugify title) "untitled"))
                             (timestamp (format-time-string "%Y%m%d%H%M%S")))
                        (format "work/%s-%s.org" slug timestamp)))
                    "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: work\n#+updated: \n\n")
           :immediate-finish t
           :jump-to-captured nil
           :empty-lines 1
           :unnarrowed t)
          ("l" "Org-Protocol Link" entry
           "%(org-protocol-capture-get-contents)"
           :if-new (file+head
                    (lambda ()
                      (let* ((title (plist-get org-capture-plist :title))
                             (slug (if title (org-roam--slugify title) "untitled"))
                             (timestamp (format-time-string "%Y%m%d%H%M%S")))
                        (format "links/%s-%s.org" slug timestamp)))
                    "#+title: ${title}\n#+date: %U\n\n")
           :immediate-finish t
           :jump-to-captured nil
           :unnarrowed t)
          ("c" "Contacts" plain "%?"
           :if-new (file+head
                    (lambda ()
                      (let* ((title (plist-get org-capture-plist :title))
                             (slug (if title (org-roam--slugify title) "untitled"))
                             (timestamp (format-time-string "%Y%m%d%H%M%S")))
                        (format "personal/contacts/%s-%s.org" slug timestamp)))
                    "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: contacts\n#+startup: showall\n\n")
           :immediate-finish t
           :jump-to-captured nil
           :empty-lines 1
           :unnarrowed t)
          ("e" "Emacs Related Note" plain "%?"
           :if-new (file+head
                    (lambda ()
                      (let* ((title (plist-get org-capture-plist :title))
                             (slug (if title (org-roam--slugify title) "untitled"))
                             (timestamp (format-time-string "%Y%m%d%H%M%S")))
                        (format "emacs/%s-%s.org" slug timestamp)))
                    "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: emacs\n#+startup: content\n")
           :empty-lines 1
           :jump-to-captured nil
           :unnarrowed t)
          ("m" "Meeting" entry
           "* TODO MEETING: %^{Meeting Title}\nSCHEDULED: %^{Meeting Date and Time: }T\n\n%?"
           :if-new (file+head
                    (lambda ()
                      (let* ((title (plist-get org-capture-plist :title))
                             (slug (if title (org-roam--slugify title) "untitled"))
                             (timestamp (format-time-string "%Y%m%d%H%M%S")))
                        (format "work/meetings/%s-%s.org" slug timestamp)))
                    "#+title: %^{Meeting Title}\n#+filetags: work meeting\n\n")
           :empty-lines 1
           :jump-to-captured nil
           :unnarrowed t)
          ("t" "TODO" plain "* TODO ${title}\n%?"
           :if-new (file+head
                    (lambda ()
                      (let* ((title (plist-get org-capture-plist :title))
                             (slug (if title (org-roam--slugify title) "untitled"))
                             (timestamp (format-time-string "%Y%m%d%H%M%S")))
                        (format "todos/%s-%s.org" slug timestamp)))
                    "#+title: ${title}\n#+date: %<%Y-%m-%d %a %R>\n#+filetags: %^{Tag:|work|personal}\n#+updated: \n\n")
           :immediate-finish nil
           :jump-to-captured nil
           :empty-lines 1
           :unnarrowed t)))

  ;; Time stamp update snippet
  (setq time-stamp-start "#\\+updated: [\t]*")

  ;; Dailies capture templates
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "\n* %<%H:%M> %?\n:properties:\n:created: %U\n:end:\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: TODO %<%Y-%m-%d %a>\n#+startup: showall\n\n")
           :empty-lines 1
           :jump-to-captured nil)))

  ;; Custom slug function
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(768 769 770 771 772 774 775 776
                                 777 778 779 780 795 803 804 805
                                 807 813 814 816 817)))
      (cl-flet* ((nonspacing-mark-p (char)
                   (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                   (string-glyph-compose
                    (apply #'string
                           (seq-remove #'nonspacing-mark-p
                                       (string-glyph-decompose s)))))
                 (cl-replace (title pair)
                   (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs '(("[^[:alnum:][:digit:]]" . "-")
                        ("--*" . "-")
                        ("^-" . "")
                        ("-$" . "")))
               (slug (-reduce-from #'cl-replace
                                   (strip-nonspacing-marks title)
                                   pairs)))
          (downcase slug)))))

  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; Additional Org-roam custom functions
(defvar my-date-regexp "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Za-z]+")

(defun ugt-filter-org-roam-node-file-p (node)
  "Return non-nil if NODE is a top-level file node, not a journal date."
  (and (= (org-roam-node-level node) 0)
       (not (string-match my-date-regexp (org-roam-node-title node)))))

(defun ugt-filter-org-roam-node-exclude-archived-and-journal-files (node)
  "Exclude nodes that are journal entries, in archive, or tagged 'archive'."
  (and (not (string-match my-date-regexp (org-roam-node-title node)))
       (not (member "archive" (org-roam-node-tags node)))
       (not (string-match-p "archive/" (org-roam-node-file node)))))

(defun ugt-org-roam-node-find ()
  "Refined search for org-roam nodes excluding archived/journal entries."
  (interactive)
  (org-roam-node-find
   :other-window nil
   #'ugt-filter-org-roam-node-exclude-archived-and-journal-files))

(defun ugt-org-roam-node-find-document-nodes ()
  "Refined search for document-level org-roam nodes (excludes dates)."
  (interactive)
  (org-roam-node-find
   :other-window nil
   #'ugt-filter-org-roam-node-file-p))

 (defun ugt-org-roam-dailies-goto-today ()
   "Open today's journal in another window."
   (interactive)
   (split-window-right)
   (other-window 1)
   (org-roam-dailies-goto-today))

;; Hooks to refresh org-agenda-files after updates
(defun my/org-roam-refresh-agenda-list ()
  "Refresh `org-agenda-files` to include all Org files in org-roam recursively."
  (interactive)
  (setq org-agenda-files
        (directory-files-recursively (concat org-directory "/roam") "\\.org$"))
  (message "org-agenda-files refreshed."))

(my/org-roam-refresh-agenda-list)
(add-hook 'org-roam-capture-after-finalize-hook #'my/org-roam-refresh-agenda-list)
(add-hook 'org-roam-db-autosync-after-save-hook #'my/org-roam-refresh-agenda-list)

;; Hooks for capturing
(defun ugt-org-capture-after-finalize-hook nil
  (org-capture-goto-last-stored)
  (end-of-buffer)
  (recenter-top-bottom '(4)))

(defun my-org-capture-hook ()
  (when (plist-member org-capture-plist :org-roam)
    (org-cycle-hide-drawers 'overview)))
(add-hook 'org-capture-mode-hook #'my-org-capture-hook)

(defun my-org-roam-hook ()
  (org-cycle-hide-drawers 'overview))
(add-hook 'org-roam-mode-hook #'my-org-roam-hook)

(defun ugt-org-roam-dailies-find-file-hook nil
  (end-of-buffer))
(add-hook 'org-roam-dailies-find-file-hook #'ugt-org-roam-dailies-find-file-hook)

(defun insert-created-date ()
  (insert (format-time-string "%Y-%m-%d %a %H:%M:%S ")))

;;(add-hook 'org-roam-dailies-capture-today #'insert-created-date)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table-vertical 2
        org-modern-table-horizontal 2
        org-modern-star ["●" "○" "✸" "✿"]
        org-modern-list '((43 . "•") (45 . "–") (42 . "•"))))


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
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
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
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :bind ("C-c A" . aidermacs-transient-menu)
  :config
  ;; Specify an Anthropic model
  (setq aidermacs-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
(setq aidermacs-use-architect-mode t)
(setq aidermacs-architect-model "o3-mini") ; default
(setq aidermacs-editor-model "deepseek/deepseek-chat") ;; defaults to aidermacs-default-model
  ;; Load the Anthropic API key from env
  ;; (let ((anthropic-api-key
  ;;        (string-trim (shell-command-to-string "$SHELL --login -c 'echo $ANTHROPIC_API_KEY'"))))
  ;;   (setenv "ANTHROPIC_API_KEY" anthropic-api-key)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Misc Packages                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recentf
(use-package recentf
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
  :straight t
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
;;                              ERC (IRC Client)                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-erc-connect ()
  "Retrieve IRC credentials from authinfo.gpg and connect to the IRC server."
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
             :password password)))

(use-package erc
  :straight t
  :config
  (setq erc-track-remove-disconnected-buffers t
        erc-hide-list '("PART" "QUIT" "JOIN")
        erc-interpret-mirc-color t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t
        erc-track-shorten-start 8
        erc-kill-buffer-on-part t
        erc-auto-query 'bury))

(global-set-key (kbd "C-c E") 'my-erc-connect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Elfeed + Dashboard                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq elfeed-feeds
      '("https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Fsachachua.com%2Fblog%2Ffeed%2Findex.xml&key=1&hash=48ac81675b0797fda5d8f4f189846563a5ed14d9&max=1000&links=preserve&exc="
        "https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Fddosecrets.com%2Farticle%2Flexipolleaks&key=1&hash=b9258f920d5b200034edd73868c42b1e68284695&max=1000&links=preserve&exc="
        "https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Fhnrss.org%2Fnewest&key=1&hash=62a3abd97ca026dbb64c82151d396f32e4c6a4fb&max=1000&links=preserve&exc="
        "https://rss.samhain.su/makefulltextfeed.php?url=https%3A%2F%2Ffeeds.bbci.co.uk%2Fnews%2Frss.xml&key=1&hash=78370b961d44c8bff594b1b41a513762d6f34560&max=1000&links=preserve&exc="))

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
    :straight t
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
  :straight t
  :init
  (dired-preview-global-mode 1))

(use-package dired-git-info
  :straight t
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
  :straight t
  :defer t
  :config
  (setq calibredb-format-nerd-icons t)
  (setq calibredb-format-character-icons t)
  (setq calibredb-root-dir "~/.calibre-library")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/.calibre-library" (name . "Calibre")))))

(use-package nov
  :straight t
  :defer t
  :config
  (setq nov-unzip-program (executable-find "bsdtar")
      nov-unzip-args '("-xC" directory "-f" filename))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Treemacs Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :init
  ;; Ensure that when you switch or open a project, Treemacs is displayed.
  (with-eval-after-load 'projectile
    (add-hook 'projectile-after-switch-project-hook
              #'treemacs-display-current-project-exclusively))
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
;;                                 Flycheck Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :init
  ;; Enable flycheck globally.
  (global-flycheck-mode)
  :config
  ;; Visual indicator of errors on the right fringe
  (setq flycheck-indication-mode 'right-fringe)

  ;; You could customize specific checkers here, e.g.:
  ;; (setq-default flycheck-python-flake8-executable "python3")
  )

;; If using Eglot, remove its default Flymake usage:
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (flymake-mode -1)   ;; Disable Flymake
            (flycheck-mode 1))) ;; Enable Flycheck

;; Optionally, if you want linting in all prog modes even outside Eglot:
(add-hook 'prog-mode-hook #'flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        snippets (Yasnippet)                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)


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
;;                               Final Cleanup                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Restore GC threshold after init
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 8000000)))

(provide 'init)
;;; init.el ends here
