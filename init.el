;;; init.el -*- lexical-binding: t -*-

;; Time-stamp: <Last changed 2025-07-21 13:13:11 by grim>

;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))

;;; vc stuff

(setq package-vc-register-as-project nil) ; Emacs 30

;;; pinentry

(setenv "GPG_AGENT_INFO" nil)  ; Use emacs pinentry
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)
(when (fboundp 'pinentry-start)
  (pinentry-start))

;;; CUSTOM FUNCTIONS

(defun prot-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let* ((source (auth-source-search :host host)))
    (if (eq prop :secret)
        (funcall (plist-get (car source) prop))
      (plist-get (flatten-list source) prop))))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(keymap-global-set "<remap> <keyboard-quit>" #'prot/keyboard-quit-dwim)

(defun my/exwm-run-program ()
  "Run a program using vertico completion with command history and PATH suggestions."
  (interactive)
  (let* ((history-commands
          (when (boundp 'shell-command-history)
            (delete-dups (copy-sequence shell-command-history))))
         (path-commands
          (when (executable-find "compgen")
            (split-string
             (shell-command-to-string "compgen -c | head -200")
             "\n" t)))
         (common-commands
          '("firefox" "chromium" "code" "thunar" "alacritty" "kitty"
            "mpv" "vlc" "gimp" "libreoffice" "pavucontrol" "qjackctl"))
         (all-commands
          (delete-dups
           (append history-commands common-commands path-commands)))
         (command
          (completing-read "Run program: " all-commands nil nil nil
                           'shell-command-history)))
    (when (and command (not (string-empty-p command)))
      (start-process-shell-command command nil command))))

(defun increase-text-and-pane ()
  "Increase text size and adjust window width proportionally."
  (interactive)
  (let* ((orig-scale (or text-scale-mode-amount 0))
         (scale-factor text-scale-mode-step))
    (text-scale-increase 1)
    (when (> (length (window-list)) 1) ; Only resize if multiple windows
      (enlarge-window-horizontally
       (round (* (window-width) (- scale-factor 1)))))))

(defun decrease-text-and-pane ()
  "Decrease text size and adjust window width proportionally."
  (interactive)
  (let* ((orig-scale (or text-scale-mode-amount 0))
         (scale-factor (/ 1.0 text-scale-mode-step)))
    (text-scale-decrease 1)
    (when (> (length (window-list)) 1) ; Only resize if multiple windows
      (shrink-window-horizontally
       (round (* (window-width) (- 1 scale-factor)))))))

;; Use modern keybinding approach
(keymap-global-set "s-=" #'increase-text-and-pane)
(keymap-global-set "s--" #'decrease-text-and-pane)

(declare-function completion-preview-insert "completion-preview")
(declare-function completion-preview-next-candidate
                  "completion-preview")
(declare-function completion-preview-prev-candidate
                  "completion-preview")
(declare-function completion-preview--hide "completion-preview")

;;; FONT CONFIGURATION

;; Primary font setup with fallback
(when (find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
  ;; Main editing font
  (set-face-attribute 'default nil
                      :font "BerkeleyMonoVariable Nerd Font Mono"
                      :height 140)
  ;; Variable-pitch font for prose/org-mode
  (set-face-attribute 'variable-pitch nil
                      :font "BerkeleyMonoVariable Nerd Font Mono"
                      :height 160))

;;; EDNC NOTIFICATIONS (DBUS)

(use-package ednc
  :ensure t
  :hook (after-init . ednc-mode)
  :config
  (require 'consult)

  ;; === Configuration Variables ===
  (defgroup ednc-enhanced nil
    "Enhanced EDNC notifications."
    :group 'ednc)

  (defcustom ednc-history-limit 200
    "Maximum number of notifications to keep in history."
    :type 'integer
    :group 'ednc-enhanced)

  ;; === State Management ===
  (defvar ednc--notification-history nil
    "List of past notifications for consult browsing.")
  (defvar ednc--notification-ring (make-ring ednc-history-limit)
    "Ring buffer for notification history.")

  ;; === Faces ===
  (defface ednc-notification-time-face
    '((t :inherit font-lock-comment-face))
    "Face for timestamps in notification history.")

  ;; === Core Functions ===
  (defun ednc--store-in-history (old new)
    "Store NEW notification in history for consult browsing.
OLD is ignored but included for hook compatibility."
    (when new
      (let* ((time (format-time-string "%Y-%m-%d %H:%M:%S"))
             (app (ednc-notification-app-name new))
             (summary (ednc-notification-summary new))
             (body (ednc-notification-body new))
             (entry (propertize
                     (format "%s │ %s: %s"
                             (propertize time 'face 'ednc-notification-time-face)
                             (propertize (or app "System") 'face 'font-lock-keyword-face)
                             summary)
                     'notification new
                     'time time
                     'body body)))
        (ring-insert ednc--notification-ring entry)
        (push entry ednc--notification-history))))

  ;; === Interactive Commands ===
  (defun ednc-browse-history ()
    "Browse notification history with consult."
    (interactive)
    (let* ((candidates (ring-elements ednc--notification-ring))
           (selected
            (consult--read
             candidates
             :prompt "Notification History: "
             :category 'ednc-notification
             :sort nil
             :annotate
             (lambda (cand)
               (when-let ((body (get-text-property 0 'body cand)))
                 (when (not (string-empty-p body))
                   (concat " " (propertize (truncate-string-to-width body 50)
                                           'face 'font-lock-doc-face)))))
             :preview-key '(:debounce 0.2 any)
             :state
             (lambda (action cand)
               (when (and (eq action 'preview) cand)
                 (when-let ((notification (get-text-property 0 'notification cand)))
                   (with-current-buffer (get-buffer-create "*EDNC Preview*")
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (ednc-view-mode)
                       (insert "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
                       (insert (ednc-format-notification notification t))
                       (insert "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
                       (goto-char (point-min))
                       (display-buffer (current-buffer)
                                       '(display-buffer-below-selected
                                         (window-height . 0.3)))))))))))
      (when selected
        (message "Notification: %s" selected))))

  (defun ednc-clear-history ()
    "Clear notification history."
    (interactive)
    (when (yes-or-no-p "Clear all notification history? ")
      (setq ednc--notification-history nil)
      (dotimes (_ (ring-length ednc--notification-ring))
        (ring-remove ednc--notification-ring))
      (message "Notification history cleared")))

  ;; === Setup ===
  (setq ednc-notification-presentation-functions nil)
  (add-hook 'ednc-notification-presentation-functions #'ednc--store-in-history))

(use-package ednc-popup
  :vc (:url "https://codeberg.org/akib/emacs-ednc-popup.git" :branch "main")
  :config
  ;; Customize ednc-popup settings
  (setq ednc-popup-timeout 5
        ednc-popup-width 50
        ednc-popup-max-height 10
        ednc-popup-max-count 4)
  (add-hook 'ednc-notification-presentation-functions #'ednc-popup-presentation-function))

;;; EXWM - Dynamic multi-monitor configuration for Emacs 30.1
(when (eq window-system 'x)
  (use-package exwm
    :ensure t
    :config
    ;; Set the number of workspaces
    (require 'exwm-randr)
    (require 'exwm-systemtray)
    (setq exwm-workspace-number 4)

    ;; These keys should always pass through to Emacs
    (setq exwm-input-prefix-keys
          '(?\C-x
            ?\C-u
            ?\C-h
            ?\M-x
            ?\M-:
            ?\C-\M-j  ; Buffer list
            ?\C-\     ; Ctrl+Space
            ;; Function keys
            XF86AudioLowerVolume
            XF86AudioRaiseVolume
            XF86AudioMute
            XF86MonBrightnessUp
            XF86MonBrightnessDown))

    ;; Global keybindings available in X windows
    (setq exwm-input-global-keys
          `(;; Reset EXWM
            ([?\s-r] . exwm-reset)
            ;; Launch application
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; Switch workspace
            ([?\s-w] . exwm-workspace-switch)
            ;; Focus windows by direction
            ([s-left] . windmove-left)
            ([s-right] . windmove-right)
            ([s-up] . windmove-up)
            ([s-down] . windmove-down)
            ;; Switch to specific workspace with Super+0-9
            ,@(mapcar (lambda (i)
                        (list (kbd (format "s-%d" i))
                              `(lambda ()
                                 (interactive)
                                 (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

    ;; Line-editing shortcuts for X windows
    (setq exwm-input-simulation-keys
          '(([?\C-b] . [left])
            ([?\C-f] . [right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end delete])
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ([?\M-d] . [C-delete])))

    ;; Make buffer names more meaningful
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))

    ;; Function to automatically configure monitors
    (defun my/exwm-configure-monitors ()
      "Automatically detect and configure monitors."
      (let* ((xrandr-output (shell-command-to-string "xrandr"))
             (connected-monitors
              (seq-filter (lambda (line)
                            (string-match-p " connected" line))
                          (split-string xrandr-output "\n")))
             (monitor-names
              (mapcar (lambda (line)
                        (car (split-string line)))
                      connected-monitors))
             (primary-monitor (car monitor-names))
             (external-monitors (cdr monitor-names)))

        ;; Build xrandr command
        (when external-monitors
          (let ((xrandr-cmd "xrandr --auto"))
            ;; Configure each external monitor to the right of the previous
            (let ((prev-monitor primary-monitor))
              (dolist (monitor external-monitors)
                (setq xrandr-cmd
                      (format "%s --output %s --auto --right-of %s"
                              xrandr-cmd monitor prev-monitor))
                (setq prev-monitor monitor)))

            ;; Execute xrandr command
            (start-process-shell-command "xrandr" nil xrandr-cmd)

            ;; Configure workspace assignment
            ;; Put workspace 0 on primary, distribute others on externals
            (let ((workspace-plist '())
                  (workspace-num 0))
              ;; Workspace 0 always on primary monitor
              (setq workspace-plist (append workspace-plist
                                            (list workspace-num primary-monitor)))
              (setq workspace-num 1)

              ;; Distribute remaining workspaces across external monitors
              (when external-monitors
                (dolist (monitor (if (> (length external-monitors) 1)
                                     external-monitors
                                   ;; If only one external, put all remaining workspaces there
                                   (make-list 9 (car external-monitors))))
                  (when (< workspace-num 10)
                    (setq workspace-plist (append workspace-plist
                                                  (list workspace-num monitor)))
                    (setq workspace-num (1+ workspace-num)))))

              ;; Apply the workspace configuration
              (setq exwm-randr-workspace-monitor-plist workspace-plist)
              (exwm-randr-refresh))))))
    (add-hook 'exwm-randr-screen-change-hook #'my/exwm-configure-monitors)
    (exwm-randr-mode 1)
    (my/exwm-configure-monitors)
    (setq exwm-systemtray-height 22)        ; Your original value
    (setq exwm-systemtray-icon-gap 5)       ; This was missing!
    (exwm-systemtray-mode 1)
    (exwm-wm-mode 1))

  ;; Optional: Simple app launcher
  (defun my/app-launcher ()
    "Launch application using completing-read."
    (interactive)
    (let* ((all-commands
            (split-string
             (shell-command-to-string
              "compgen -c | grep -v '^_' | sort -u | head -200")
             "\n" t))
           (command (completing-read "Launch: " all-commands)))
      (when command
        (start-process-shell-command command nil command))))

  (global-set-key (kbd "s-SPC") #'my/app-launcher)

  ;; Optional: Auto-start some applications
  (add-hook 'exwm-init-hook
            (lambda ()
              ;; Network manager applet
              (when (executable-find "nm-applet")
                (start-process "nm-applet" nil "nm-applet"))
              ;; Bluetooth
              (when (executable-find "blueman-applet")
                (start-process "blueman-applet" nil "blueman-applet"))
              ;; Udiskie
              (when (executable-find "udiskie")
                (start-process "udiskie" nil "udiskie" "-at"))
              ;; Mullvad VPN
              (when (executable-find "mullvad-vpn")
                (start-process "mullvad-vpn" nil "mullvad-vpn"))))

  ;; Optional: Desktop environment features
  (use-package desktop-environment
    :ensure t
    :config
    (setq desktop-environment-screenlock-command "slock")
    (desktop-environment-mode 1)))

;;; init.el version control

(use-package vc
  :ensure nil
  :demand t
  :config
  (defun my-auto-commit-init-el ()
    "Commit changes to init.el after saving."
    (when (and (buffer-file-name)
               (string=
                (file-name-nondirectory (buffer-file-name)) "init.el"))
      (ignore-errors
        (vc-checkin
         (list (buffer-file-name))
         'git
         nil
         "Auto-commit init.el changes"))))
  :hook (after-save . my-auto-commit-init-el))

;;; emacs configuration section

;; Define my-tmp-dir early so it's available for other packages
(defvar my-tmp-dir (expand-file-name "~/.tmp/")
  "Centralized directory for temporary files, backups, and history files.
This keeps the main .emacs.d directory clean and organizes cache files logically.")

;; Create the main temporary directory
(unless (file-exists-p my-tmp-dir)
  (make-directory my-tmp-dir t))

;; Create subdirectories for different types of files
;; This single directory creation handles all file management needs
(dolist (dir '("backups"        ; backup-directory-alist
               "auto-saves"     ; auto-save-file-name-transforms
               "auto-save-list" ; auto-save-list-file-prefix
               "recentf"        ; recentf-save-file
               "eshell"         ; eshell-directory-name
               "tramp-auto-save" ; tramp-auto-save-directory
               "saveplace"      ; save-place-file
               "undos"          ; vundo-files-directory
               "gnus-drafts"))  ; message-auto-save-directory
  (let ((subdir (expand-file-name dir my-tmp-dir)))
    (unless (file-exists-p subdir)
      (make-directory subdir t))))

(use-package emacs
  :ensure nil ; Built-in package
  :init
  ;; Early settings that should be set before package loads
  (setq auth-sources '("~/.authinfo.gpg")
        epg-pinentry-mode 'loopback
        epg-gpg-program "gpg2")

  :config
  ;;; Personal Information
  (setq user-full-name "TJ"
        user-mail-address "tj@emacs.su"
        calendar-location-name "New York, NY"
        calendar-latitude 40.7
        calendar-longitude -74.0)

  ;; Set timezone
  (setenv "TZ" "America/New_York")

  ;;; Encoding and Language
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")

  ;;; Files and Backups
  (setq create-lockfiles nil
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        backup-by-copying t
        backup-directory-alist `((".*" . ,(expand-file-name "backups" my-tmp-dir)))
        auto-save-default t
        auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" my-tmp-dir) t))
        auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my-tmp-dir)
        save-place-file (expand-file-name "saveplace/saveplace" my-tmp-dir))

  ;;; History and Persistence
  (setq history-length 10000
        history-delete-duplicates t
        savehist-file (expand-file-name "savehist" my-tmp-dir)
        savehist-save-minibuffer-history t
        password-cache-expiry nil
        auth-source-cache-expiry nil
        plstore-cache-directory my-tmp-dir)

  ;; Enable persistent modes
  (save-place-mode 1)
  (savehist-mode 1)

  ;;; Display and UI
  (setq truncate-string-ellipsis "…"
        x-stretch-cursor t
        help-window-select t
        echo-keystrokes-help nil
        display-time-load-average t)

  ;;; Scrolling
  (setq scroll-margin 3
        scroll-step 1
        scroll-conservatively 1
        scroll-preserve-screen-position 1
        scroll-error-top-bottom t
        auto-window-vscroll nil)

  ;;; Editing Behavior
  (setq-default indent-tabs-mode nil
                tab-width 2
                standard-indent 2
                default-directory '~)

  (setq tab-always-indent 'complete
        save-interprogram-paste-before-kill t
        delete-by-moving-to-trash t
        kill-ring-max 5000
        undo-limit 800000
        delete-pair-blink-delay 0.1
        delete-section-mode t)

  ;;; Search
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil)

  ;;; Remote Files
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t
        remote-file-name-inhibit-auto-save t)

  ;;; Process and Performance
  (setq fast-read-process-output t
        garbage-collection-messages nil)

  ;;; Miscellaneous
  (setq confirm-kill-processes nil
        confirm-kill-emacs nil
        shell-kill-buffer-on-exit t
        window-combination-resize t
        eval-expression-print-length nil
        next-error-recenter '(4)
        find-library-include-other-files nil)

  ;; Use short answers
  (fset 'yes-or-no-p 'y-or-n-p)

  ;;; Authentication and Encryption
  (require 'auth-source)
  (require 'epa-file)
  (epa-file-enable)

  ;;; Load Custom File
  (when (file-exists-p custom-file)
    (load custom-file))

  :hook
  ;; Mode-specific hooks
  ((prog-mode . display-line-numbers-mode)
   (text-mode . visual-wrap-prefix-mode)
   (before-save . (lambda ()
                    (whitespace-cleanup)))
   ;; Startup hook for global modes
   (emacs-startup . (lambda ()
                      (global-visual-line-mode 1)
                      (global-auto-revert-mode 1)
                      (line-number-mode 1)
                      (column-number-mode 1)
                      (size-indication-mode 1)
                      (display-time-mode 1))))

  :bind
  (("C-x k" . kill-current-buffer)
   ("C-x K" . kill-buffer)))

;;; Theme Configuration (separate use-package)
(use-package modus-themes
  :ensure nil
  :demand t
  :init
  ;; Theme variables should be set before loading
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-disable-other-themes t)

  :config
  ;; Load the theme
  (load-theme 'modus-vivendi t)

  ;; Font customizations
  (custom-set-faces
   '(cursor ((t (:background "#FFC107")))))

  ;; Font-lock customizations
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic
                      :weight 'light)
  (set-face-attribute 'font-lock-keyword-face nil
                      :weight 'black)

  ;; Fallback font configuration
  (unless (find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
    (set-face-attribute 'default nil :height 140)
    (set-face-attribute 'variable-pitch nil :height 160)))

(use-package windower
  :ensure t
  ;; :after exwm
  :bind
  (("s-<tab>" . windower-switch-to-last-buffer)
   ("s-o" . windower-toggle-single)
   ("s-\\" . windower-toggle-split)
   ("s-M-<left>" . windower-move-border-left)
   ("s-M-<down>" . windower-move-border-below)
   ("s-M-<up>" . windower-move-border-above)
   ("s-M-<right>" . windower-move-border-right)
   ("s-S-<left>" . windower-swap-left)
   ("s-S-<down>" . windower-swap-below)
   ("s-S-<up>" . windower-swap-above)
   ("s-S-<right>" . windower-swap-right))) ; Close windower use-package and when block

;;; shell environment (path, etc)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (setq exec-path-from-shell-variables '("PATH"))
  (let ((local-bin (expand-file-name "~/.local/bin")))
    (unless (member local-bin exec-path)
      (add-to-list 'exec-path local-bin t) ;; Add to end of exec-path
      (setenv "PATH" (concat local-bin ":" (getenv "PATH")))))
  (exec-path-from-shell-initialize))

;;; ediff settings

(use-package ediff
  :ensure nil
  :defer t

  :custom
  ;; Window setup
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-right)

  ;; Behavior
  (ediff-keep-variants nil)

  ;; Control frame parameters
  (ediff-control-frame-parameters
   (cons '(unsplittable . t) ediff-control-frame-parameters))

  :custom-face
  ;; Use inherit to respect theme colors while adding emphasis
  (ediff-current-diff-A ((t (:inherit diff-removed :extend t))))
  (ediff-fine-diff-A ((t (:inherit diff-removed :weight bold))))
  (ediff-current-diff-B ((t (:inherit diff-added :extend t))))
  (ediff-fine-diff-B ((t (:inherit diff-added :weight bold))))

  :config
  ;; Enhanced ediff quit that ensures clean exit
  (defun my/ediff-quit ()
    "Quit ediff and ensure clean window restoration."
    (interactive)
    (when (bound-and-true-p ediff-control-buffer)
      (ediff-quit t)))

  ;; Compare current buffer with its file
  (defun my/ediff-buffer-with-file ()
    "Compare current buffer with its file on disk.
If buffer is modified, offer to save first."
    (interactive)
    (unless buffer-file-name
      (user-error "Current buffer is not visiting a file"))
    (when (and (buffer-modified-p)
               (y-or-n-p "Buffer is modified. Save it first? "))
      (save-buffer))
    (ediff-current-file))

  ;; Directory comparison with sane defaults
  (defun my/ediff-directories ()
    "Compare directories with better default regex."
    (interactive)
    (let ((ediff-default-filtering-regexp ""))
      (call-interactively #'ediff-directories)))

  :hook
  ;; Enhanced keybindings for ediff sessions
  (ediff-keymap-setup . (lambda ()
                          (define-key ediff-mode-map (kbd "Q") #'my/ediff-quit)
                          (define-key ediff-mode-map (kbd "q") #'my/ediff-quit)))

  :bind
  (("C-c d f" . ediff-files)
   ("C-c d b" . ediff-buffers)
   ("C-c d c" . my/ediff-buffer-with-file)
   ("C-c d d" . my/ediff-directories)
   ("C-c d r" . ediff-regions-linewise)
   ("C-c d R" . ediff-regions-wordwise)))

;; Diff-mode configuration (separate as it's a different package)
(use-package diff-mode
  :ensure nil
  :defer t
  :custom-face
  ;; More subtle diff colors that work with most themes
  (diff-added ((t (:foreground "green4" :extend t))))
  (diff-removed ((t (:foreground "red3" :extend t))))
  (diff-hunk-header ((t (:inherit font-lock-comment-face :weight bold))))
  (diff-file-header ((t (:inherit font-lock-keyword-face :weight bold)))))

;;; tramp settings

(use-package tramp
  :ensure nil
  :defer t

  :custom
  ;; Connection settings
  (tramp-default-method "ssh")
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 1024 1024)) ; 1MB threshold for using scp

  ;; Performance optimizations
  (tramp-verbose 2) ; Low verbosity for better performance (0-10 scale)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)

  ;; Auto-save and backup
  (tramp-auto-save-directory (expand-file-name "tramp-auto-save" my-tmp-dir))

  ;; File monitoring
  (auto-revert-remote-files t)

  :config
  ;; Connection-local variables for better async process handling
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  ;; Remove the problematic compilation hook that can interfere with SSH
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

;; Magit-specific TRAMP settings
(use-package magit
  :defer t
  :custom
  ;; Improve magit performance over TRAMP
  (magit-tramp-pipe-stty-settings 'pty))

;; Optional: Enhanced TRAMP functionality
(use-package tramp
  :ensure nil
  :defer t
  :config
  ;; Helper function to clean TRAMP buffers
  (defun my/tramp-cleanup-all ()
    "Clean all TRAMP connections and buffers."
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (message "TRAMP connections and buffers cleaned"))

  ;; Clear TRAMP cache for a specific connection
  (defun my/tramp-cleanup-current ()
    "Clean TRAMP connection for current buffer's remote."
    (interactive)
    (when (file-remote-p default-directory)
      (tramp-cleanup-this-connection)
      (message "Cleaned TRAMP connection for %s" default-directory)))

  :bind
  (("C-c t c" . my/tramp-cleanup-current)
   ("C-c t C" . my/tramp-cleanup-all)))

;;; Custom Log Mode
;; Define a simple major mode for log files
(define-derived-mode log-mode fundamental-mode "Log"
  "Major mode for viewing log files with syntax highlighting."
  (setq-local font-lock-defaults
              '((("\\<DEBUG\\>" . font-lock-comment-face)
                 ("\\<INFO\\>" . font-lock-string-face)
                 ("\\<WARN\\>" . font-lock-warning-face)
                 ("\\<ERROR\\>" . font-lock-function-name-face)
                 ;; Timestamps (common formats)
                 ("\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[T ][0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\b"
                  . font-lock-constant-face)
                 ;; IP addresses
                 ("\\b[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\b"
                  . font-lock-variable-name-face))))
  ;; Make buffer read-only by default
  (setq buffer-read-only t)
  ;; Enable auto-revert for live log viewing
  (auto-revert-tail-mode 1))

;; Register log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . log-mode))

;;; Undo Tree Visualization
(use-package vundo
  :ensure t
  :defer t
  :bind ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-files-directory (expand-file-name "vundo" my-tmp-dir))
  ;; Compact display for better overview
  (vundo-compact-display t))

;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :diminish
  :hook ((prog-mode . rainbow-delimiters-mode)
         ;; Also useful in these modes
         (conf-mode . rainbow-delimiters-mode)
         (yaml-mode . rainbow-delimiters-mode))
  :custom
  (rainbow-delimiters-max-face-count 9)
  :config
  ;; Make outermost parens more prominent
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :weight 'bold))

;;; Highlight Current Symbol
(use-package highlight-thing
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode . highlight-thing-mode)
  :custom
  (highlight-thing-delay-seconds 0.5)
  (highlight-thing-what-thing 'symbol)
  (highlight-thing-case-sensitive-p t)
  (highlight-thing-exclude-thing-under-point t)
  (highlight-thing-limit-to-region-in-large-buffers-p t)
  (highlight-thing-narrow-region-lines 300)
  :custom-face
  ;; Use a subtle highlight that works with most themes
  (highlight-thing ((t (:inherit highlight :background unspecified
                                 :underline (:color "#5e81ac" :style line))))))

;;; Indentation Guides
(use-package indent-bars
  :ensure t
  :defer t
  :diminish
  :hook ((prog-mode . indent-bars-mode)
         (yaml-mode . indent-bars-mode)
         (python-mode . indent-bars-mode))
  :custom
  ;; Visual appearance
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-display-on-blank-lines t)
  (indent-bars-prefer-character nil)

  ;; Highlighting
  (indent-bars-highlight-current-depth '(:blend 0.3))

  ;; Behavior
  (indent-bars-no-descend-strings t)
  (indent-bars-no-descend-lists t)
  (indent-bars-depth-update-delay 0.1)

  ;; Tree-sitter integration
  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope
   '((python function_definition class_definition for_statement
             if_statement while_statement with_statement)
     (emacs-lisp defun defmacro defvar defcustom let let*
                 when unless if progn save-excursion)
     (c function_definition if_statement for_statement
        while_statement switch_statement)
     (cpp function_definition if_statement for_statement
          while_statement switch_statement class_specifier namespace_definition)
     (rust function_item impl_item match_expression if_expression)
     (javascript function_declaration function_expression arrow_function
                 class_declaration if_statement for_statement)
     (typescript function_declaration function_expression arrow_function
                 class_declaration if_statement for_statement)
     (go function_declaration method_declaration if_statement
         for_statement switch_statement))))

;;; files package
(use-package files
  :ensure nil
  :defer t
  :custom
  ;; Revert buffers automatically when files change on disk
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Backup settings (if not already configured elsewhere)
  (make-backup-files t)
  (vc-make-backup-files t))

;;; Auto-revert Configuration
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil) ; Less noisy
  (global-auto-revert-non-file-buffers t))

;;; Modeline Management
(use-package delight
  :ensure t
  :config
  ;; Hide or rename minor modes in modeline
  (delight
   '((auto-revert-mode nil "autorevert")
     (eldoc-mode nil "eldoc")
     (global-hl-line-mode nil "hl-line")
     (save-place-mode nil "saveplace")
     (flyspell-mode " ✍" "flyspell")
     (yas-minor-mode nil "yasnippet")
     (smartparens-mode nil "smartparens"))))

;;; Structural Editing
(use-package smartparens
  :ensure t
  :defer t
  :diminish
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  ;; Disable smartparens in minibuffer to avoid conflicts
  (add-hook 'minibuffer-setup-hook #'turn-off-smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)))

;;; Completion Framework

;;; vertico
;; Completion UI
(use-package vertico
  :ensure t
  :demand t  ; Core completion system
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize t)
  (vertico-preselect 'first)
  (vertico-sort-function #'vertico-sort-history-length-alpha)
  :init
  (vertico-mode 1)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-j" . vertico-exit-input)))

;;; orderless
;; Completion Style
(use-package orderless
  :ensure t
  :demand t  ; Core completion dependency
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion orderless))
     (buffer (styles basic orderless))
     (eglot (styles orderless))
     (eglot-capf (styles orderless))))
  (orderless-matching-styles '(orderless-regexp
                               orderless-literal
                               orderless-initialism))
  (orderless-smart-case t))

;;; savehist
;; Persist History
(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :custom
  (savehist-file (expand-file-name "savehist" my-tmp-dir))
  (savehist-additional-variables
   '(kill-ring
     mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history
     vertico-repeat-history)))

;;; marginalia
;; Rich Annotations
(use-package marginalia
  :ensure t
  :demand t  ; Load with vertico
  :init (marginalia-mode 1)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;; consult
;; Enhanced Commands
(use-package consult
  :ensure t
  :defer 1
  :custom
  ;; Preview settings
  (consult-preview-key '(:debounce 0.3 any))
  (consult-narrow-key "<")

  ;; Performance tuning
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)

  ;; Better grep experience
  (consult-line-numbers-widen t)
  (consult-line-start-from-top t)

  :config
  ;; Configure specific commands
  (consult-customize
   ;; Disable preview for these commands
   consult-theme :preview-key nil
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))

  ;; Custom hidden buffer source for consult-buffer
  (defvar consult-source-hidden-buffer
    `(:name "Hidden Buffer"
            :narrow ?h
            :category buffer
            :face consult-buffer
            :history buffer-name-history
            :action ,#'consult--buffer-action
            :items ,(lambda ()
                      (consult--buffer-query
                       :predicate (lambda (buf)
                                    (string-prefix-p " " (buffer-name buf)))
                       :sort 'visibility))))

  (add-to-list 'consult-buffer-sources 'consult-source-hidden-buffer 'append)

  :bind
  ;; C-c bindings (mode-specific-map)
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x C-b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))

  ;; Replace bindings
  :init
  ;; Use Consult for some built-in commands
  (global-set-key [remap Info-search] #'consult-info)
  (global-set-key [remap isearch-forward] #'consult-line)
  (global-set-key [remap recentf-open-files] #'consult-recent-file))

;; Consult integration for yasnippet
(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet)
  :bind ("C-c y" . consult-yasnippet))

;; Consult integration for flycheck
(use-package consult-flycheck
  :ensure t
  :after (consult flycheck)
  :bind (:map flycheck-mode-map
              ("C-c ! c" . consult-flycheck)))

;;; In-buffer Completion Preview
(use-package completion-preview
  :ensure nil
  :hook ((prog-mode . completion-preview-mode)
         (conf-mode . completion-preview-mode))
  :custom
  (completion-preview-minimum-symbol-length 2)
  (completion-preview-idle-delay 0.2)
  (completion-preview-exact-match-only nil)
  :custom-face
  (completion-preview ((t (:inherit shadow :foreground "#FFC107"))))
  (completion-preview-exact ((t (:inherit completion-preview :weight bold))))
  :bind
  (:map completion-preview-active-mode-map
        ("TAB" . completion-preview-insert)
        ([tab] . completion-preview-insert)
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate)))

;;; Mode-line Configuration

(use-package emacs
  :ensure nil
  :config
  ;; Basic mode-line settings
  (setq mode-line-compact t)  ; Emacs 28+ compact mode

  ;; Position format - line:column
  (setq mode-line-position-column-line-format '(" %l:%c"))
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)

  ;; Time with ISO date format
  (setq display-time-format "%Y-%m-%d %H:%M"
        display-time-default-load-average nil)
  (display-time-mode 1)

  ;; Battery display
  (require 'battery)
  (when (and battery-status-function
             (not (string-match-p "N/A"
                                  (battery-format "%B"
                                                  (funcall battery-status-function)))))
    (setq battery-mode-line-format "%b%p%%  ")  ; Extra spaces for separation
    (setq battery-mode-line-limit 85)
    (display-battery-mode 1))

  ;; Which function mode
  (which-function-mode 1)
  (setq which-func-modes '(prog-mode)
        which-func-unknown "")

  ;; Right alignment edge (Emacs 30.1 feature)
  (setq mode-line-right-align-edge 'right-fringe)

  ;; Standard mode-line format using built-in variables
  (setq-default mode-line-format
                '("%e"  ; Out of memory indicator
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "  "
                  mode-line-position
                  "  "
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  ;; Right-aligned section (Emacs 30.1)
                  mode-line-format-right-align
                  which-func-mode  ; Current function
                  "  "
                  mode-line-misc-info  ; Includes battery and time
                  mode-line-end-spaces))

  ;; Customize faces to inherit from theme
  :custom-face
  (mode-line ((t (:box (:line-width -1 :style released-button)))))
  (mode-line-inactive ((t (:box (:line-width -1 :style released-button)))))
  (mode-line-buffer-id ((t (:weight bold :inherit font-lock-keyword-face))))
  (mode-line-emphasis ((t (:weight bold :inherit warning)))))

;; Minions for minor mode management
(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  :custom
  (minions-prominent-modes '(flymake-mode
                             flycheck-mode
                             projectile-mode
                             lsp-mode
                             eglot--managed-mode))
  (minions-mode-line-lighter " ◎"))

;; Visual bell in mode-line
(use-package mode-line-bell
  :ensure t
  :config
  (mode-line-bell-mode 1))

;;; nerd-icons

(use-package nerd-icons
  :ensure t
  :defer t   ; Defer icon loading for faster startup
  :custom
  (nerd-icons-color-icons t)
  (nerd-icons-scale-factor 1.0)
  :config
  (defvar nerd-icons-cache (make-hash-table :test 'equal))
  (defun nerd-icons-cached (icon-name)
    "Get cached icon or generate and cache it."
    (or (gethash icon-name nerd-icons-cache)
        (puthash icon-name (nerd-icons-icon-for-file icon-name) nerd-icons-cache))))

(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode 1)
  :hook
  (marginalia-mode . #'nerd-icons-completion-marginalia-setup))

(use-package expand-region :ensure t :bind ("C-=" . er/expand-region))

;;; multiple cursors

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-c >" . mc/mark-next-like-this)
   ("C-c <" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;;; diff-hl

(use-package diff-hl
  :ensure t
  :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode +1))

;;; which-key

(use-package which-key
  :ensure nil  ; Built-in since Emacs 29
  :demand t
  :diminish
  :custom
  ;; Core settings
  (which-key-idle-delay 0)
  (which-key-idle-secondary-delay 0)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.30)
  (which-key-show-early-on-C-h t)

  ;; Display settings
  (which-key-separator " → ")
  (which-key-prefix-prefix "+")
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-sort-order 'which-key-prefix-then-key-order)

  ;; Let which-key automatically format things
  (which-key-dont-use-unicode nil)
  (which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))

  :config
  (which-key-mode 1)
  (which-key-add-key-based-replacements
    "C-c n" "notes"
    "C-c L" "lsp"
    "C-c 0" "0x0-upload"
    "C-c F" "firefox"
    "C-c d" "diff"
    "C-c n q" "query")

  (push '((nil . "\\`\\([[:alnum:]-]+\\)\\+'") . (nil . "\\1+"))
        which-key-replacement-alist))

;;; flyspell

(use-package flyspell
  :ensure nil
  :defer t
  :hook
  ((text-mode . flyspell-mode)
   (org-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_US")
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  ;; Prot's performance tweaks
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)

  ;; FIX: Disable ispell word list since we're using aspell
  (setq ispell-alternate-dictionary nil)

  ;; Remove ispell completions from text modes since we use aspell
  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (remove 'ispell-completion-at-point
                                  completion-at-point-functions))))

  ;; Ensure aspell is installed
  (unless (executable-find "aspell")
    (message "Aspell not found; flyspell disabled")
    (flyspell-mode -1))
  :bind
  (:map
   flyspell-mode-map
   ("M-$" . flyspell-correct-wrapper)))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind
  (:map flyspell-mode-map ("M-$" . flyspell-correct-wrapper)))

;;; Eglot - Built-in LSP client for Emacs 30.1
(use-package eglot
  :ensure nil  ; Built-in
  :defer t
  :custom
  ;; Keep it simple - Eglot works well with defaults
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-extend-to-xref t)

  :config
  ;; Python configuration (if needed)
  (setq-default eglot-workspace-configuration
                '((pylsp (plugins (flake8 (enabled . t))
                                  (black (enabled . t))
                                  (pycodestyle (enabled . :json-false))))))

  :hook
  ;; Auto-start for these modes
  ((python-mode python-ts-mode
                js-mode js-ts-mode
                typescript-mode typescript-ts-mode) . eglot-ensure)

  :bind
  ;; Eglot uses standard Emacs commands!
  (:map eglot-mode-map
        ;; Use built-in xref commands
        ("C-c l r" . xref-find-references)
        ("C-c l d" . xref-find-definitions)
        ("C-c l i" . eglot-find-implementation)
        ("C-c l t" . eglot-find-typeDefinition)
        ;; Eglot-specific commands
        ("C-c l a" . eglot-code-actions)
        ("C-c l R" . eglot-rename)
        ("C-c l f" . eglot-format)
        ("C-c l F" . eglot-format-buffer)))

;; Consult integration for Eglot (NOT consult-lsp!)
(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :bind
  (:map eglot-mode-map
        ("C-c l s" . consult-eglot-symbols)))

;; Better diagnostics display with Flymake (built-in)
(use-package flymake
  :ensure nil  ; Built-in
  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! L" . flymake-show-project-diagnostics)))

;; Cape for better completion (works with Eglot)
(use-package cape
  :ensure t
  :init
  ;; Add Cape completion functions to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :bind
  ("C-c p f" . cape-file)
  ("C-c p d" . cape-dabbrev)
  ("C-c p l" . cape-line))

;;; Org Mode
(use-package org
  :ensure nil
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :custom
  ;; Core settings
  (org-directory "~/Documents/notes/")
  (org-startup-indented t)
  (org-startup-folded 'show)  ; 'content' hides too much
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-hide-emphasis-markers t)

  ;; Better defaults
  (org-agenda-files (list org-directory))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c@)")))

  ;; Capture templates
  (org-capture-templates
   '(("t" "Todo" entry (file "todo.org")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
     ("n" "Note" entry (file "notes.org")
      "* %? :NOTE:\n%U\n")))

  ;; Export settings
  (org-export-with-broken-links t)
  (org-html-validation-link nil)

  ;; LaTeX/PDF export settings
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"))

  :hook
  (org-mode . visual-line-mode))

;; LaTeX export configuration for custom document classes
(with-eval-after-load 'ox-latex
  ;; Add your custom document class
  (add-to-list 'org-latex-classes
               '("citywide"
                 "\\documentclass{citywide}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  ;; Use latexmk if available (better than multiple pdflatex runs)
  (when (executable-find "latexmk")
    (setq org-latex-pdf-process
          '("latexmk -pdf -f -interaction=nonstopmode -output-directory=%o %f"))))

;; Optional: Modern Org for better experience
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-label-border 1)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2))

;; Optional: GitHub Flavored Markdown (if you use GitHub)
(use-package ox-gfm
  :ensure t
  :after org)

;;; markdown

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)  ; Use gfm-mode for all .md files
  :init
  (setq markdown-fontify-code-blocks-natively t)  ; Syntax highlight code blocks
  (setq markdown-command "pandoc")               ; Use pandoc for export (optional)
  :bind (:map markdown-mode-map
              ("C-c C-c p" . markdown-preview)   ; Preview Markdown in browser
              ("C-c C-c e" . markdown-export)    ; Export to HTML or other formats
              ("C-c C-c o" . markdown-open)      ; Open exported file
              ("C-c C-t" . markdown-toggle-gfm-checkbox)) ; Toggle checkboxes
  :config
  (add-hook 'markdown-mode-hook #'auto-fill-mode)) ; Enable word wrapping

;;; Magit - Git interface
(use-package magit
  :ensure t
  :defer t
  :bind ("C-c g" . magit-status)
  :custom
  ;; Better performance
  (magit-diff-refine-hunk t)
  (magit-refresh-status-buffer nil)  ; Don't auto-refresh, use 'g' manually

  ;; Commit message standards
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(non-empty-second-line))

  ;; Repository directories (if you have multiple projects)
  (magit-repository-directories '(("~/Code" . 1))))

;; Forge - GitHub/GitLab integration (optional)
(use-package forge
  :ensure t
  :after magit
  :custom
  ;; Use built-in SQLite (Emacs 29+)
  (forge-database-connector 'sqlite-builtin))

;; That's it! Magit has excellent defaults.

;;; grep settings

(use-package grep
  :config
  (setq grep-find-ignored-directories
        (append
         '(".angular"
           ".git"
           ".hg"
           ".idea"
           ".project"
           ".settings"
           ".svn"
           "3rdparty"
           "bootstrap*"
           "pyenv"
           "target")
         grep-find-ignored-directories))
  (setq grep-find-ignored-files
        (append
         '("*.blob"
           "*.class"
           "*.gz"
           "*.jar"
           "*.pack"
           "*.xd"
           ".factorypath"
           "TAGS"
           "dependency-reduced-pom.xml"
           "projectile.cache"
           "workbench.xmi")
         grep-find-ignored-files)))

;;; recentf

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq
   recentf-save-file (expand-file-name "recentf/recentf" my-tmp-dir)
   recentf-max-saved-items 200
   recentf-max-menu-items 25
   recentf-auto-cleanup nil)
  (run-at-time nil (* 5 60) 'recentf-save-list) ; Save every 5 minutes
  :bind (("C-c r" . consult-recent-file)))

;;; Dired - Directory Editor
(use-package dired
  :ensure nil
  :bind
  ("C-x C-j" . dired-jump)
  :hook
  (dired-mode . hl-line-mode)
  :custom
  ;; Better defaults
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)          ; Copy/move to other dired window
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)  ; Ask once for top level
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t)  ; Emacs 28+ feature

  :config
  ;; Load dired-x for extra features
  (require 'dired-x)
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'")

  ;; Simple open externally function
  (defun dired-open-externally ()
    "Open file at point with system handler."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (call-process "xdg-open" nil 0 nil file)))

  :bind
  (:map dired-mode-map
        ("M-o" . dired-omit-mode)
        ("E" . dired-open-externally)
        ("<backspace>" . dired-up-directory)))

;; Icons support (optional but nice)
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Better colors
(use-package diredfl
  :ensure t
  :config (diredfl-global-mode 1))

;; Subtree navigation (genuinely useful)
(use-package dired-subtree
  :ensure t
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<C-tab>" . dired-subtree-cycle))
  :custom
  (dired-subtree-use-backgrounds nil))

;;; EWW - Emacs Web Wowser
(use-package eww
  :ensure nil
  :defer t
  :custom
  ;; Just the essentials
  (eww-search-prefix "https://duckduckgo.com/html?q=")
  (shr-use-colors t)
  (shr-use-fonts t)
  (shr-max-image-proportion 0.7)
  (shr-width 80)

  :bind
  (:map eww-mode-map
        ;; Zoom controls (genuinely useful)
        ("+" . text-scale-increase)
        ("-" . text-scale-decrease)
        ("0" . text-scale-set))

  :hook
  (eww-mode . visual-line-mode))  ; Better text wrapping

;;; pdf-tools

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :init
  (defun my-open-remote-pdf-in-emacs (url &rest _args)
    "Download a PDF from URL and open it in Emacs with pdf-view-mode."
    (interactive "sPDF URL: ")
    (let ((temp-file (make-temp-file "emacs-pdf-" nil ".pdf")))
      (condition-case err
          (progn
            (url-copy-file url temp-file t)
            (find-file temp-file))
        (error
         (message "Failed to open PDF from %s: %s"
                  url
                  (error-message-string err))))
      (when (file-exists-p temp-file)
        (delete-file temp-file))))
  :config
  (unless (featurep 'pdf-tools)   ; Install only if not already loaded
    (pdf-tools-install :no-query))
  (setq
   pdf-view-display-size 'fit-page
   pdf-view-continuous t
   pdf-view-use-scaling t)
  (add-to-list 'pdf-view-incompatible-modes 'display-line-numbers-mode)
  :hook
  (pdf-view-mode
   .
   (lambda ()
     (pdf-view-themed-minor-mode)
     (pdf-view-fit-page-to-window)
     (display-line-numbers-mode -1)
     (hl-line-mode -1))))

;;; denote

(use-package denote
  :ensure t
  :defer t
  :hook
  ( ;; If you use Markdown or plain text files, then you want to make
   ;; the Denote links clickable (Org renders links as buttons right
   ;; away)
   (text-mode . denote-fontify-links-mode-maybe)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  (:map
   global-map
   ("C-c n n" . denote)
   ("C-c n j" . denote-journal-new-or-existing-entry)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep)

   ;; If you intend to use Denote with a variety of file types, it is
   ;; easier to bind the link-related commands to the `global-map', as
   ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
   ;; `markdown-mode-map', and/or `text-mode-map'.
   ("C-c n l" . denote-link)
   ("C-c n L" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
   ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
   ;; Note that `denote-rename-file' can work from any context, not just
   ;; Dired bufffers.  That is why we bind it here to the `global-map'.
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)

   ;; Key bindings specifically for Dired.
   :map
   dired-mode-map
   ("C-c C-d C-i" . denote-dired-link-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-R"
    .
    denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords
        '("emacs" "philosophy" "politics" "economics"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations
        '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :ensure t
  :after denote
  :bind
  (("C-c n f" . consult-denote-find) ("C-c n g" . consult-denote-grep))
  :config (consult-denote-mode 1))

(use-package denote-journal
  :ensure t
  :after denote
  ;; Bind those to some key for your convenience.
  :commands
  (denote-journal-new-entry
   denote-journal-new-or-existing-entry
   denote-journal-link-or-create-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  ;; Use the "journal" subdirectory of the `denote-directory'.  Set this
  ;; to nil to use the `denote-directory' instead.
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  ;; Default keyword for new journal entries. It can also be a list of
  ;; strings.
  (setq denote-journal-keyword "journal")
  ;; Read the doc string of `denote-journal-title-format'.
  (setq denote-journal-title-format 'day-date-month-year))

(use-package denote-org :ensure t :after denote :defer t)

;;; Tree-sitter - Only for languages that benefit from it
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  ;; Only use tree-sitter for languages where it's actually better
  (treesit-auto-langs '(python typescript tsx json bash))
  :config
  (global-treesit-auto-mode))

;;; so-long

(use-package so-long :ensure nil :config (global-so-long-mode 1))

;;; Flymake - Built-in syntax checking (works with Eglot!)
(use-package flymake
  :ensure nil  ; Built-in
  :hook
  ;; Enable for programming modes
  (prog-mode . flymake-mode)
  :custom
  ;; Performance settings
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-start-on-flymake-mode t)

  ;; Display settings
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-show-diagnostics-at-end-of-line t)  ; Emacs 30.1 feature!

  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! L" . flymake-show-project-diagnostics)
        ("C-c ! e" . display-local-help))  ; Shows error at point

  :config
  ;; Better error display at end of line (Emacs 30.1)
  (setq flymake-diagnostic-functions
        (append flymake-diagnostic-functions
                '(flymake-proc-legacy-flymake))))

;; Enhanced Flymake UI (optional but nice)
(use-package flymake-popon
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-popon-mode)
  :custom
  (flymake-popon-delay 0.5)
  (flymake-popon-width 60)
  (flymake-popon-method 'popon))  ; or 'posframe if you prefer

;; For Elisp files specifically
(use-package package-lint-flymake
  :ensure t
  :hook
  (emacs-lisp-mode . package-lint-flymake-setup)
  :config
  ;; Only enable for actual packages, not config files
  (defun my/maybe-enable-package-lint ()
    (when (and (buffer-file-name)
               (string-match-p "\\(?:packages\\|lisp\\)/" (buffer-file-name))
               (not (string-match-p "init\\.el\\|config\\.el" (buffer-file-name))))
      (package-lint-flymake-setup)))

  ;; Replace the hook
  (remove-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
  (add-hook 'emacs-lisp-mode-hook #'my/maybe-enable-package-lint))

;; Alternative: If you REALLY want Flycheck despite Eglot using Flymake
;; (but this is not recommended - pick one!)
;;
;; (use-package flycheck
;;   :ensure t
;;   :hook (prog-mode . flycheck-mode)
;;   :custom
;;   (flycheck-indication-mode 'right-fringe)
;;   (flycheck-check-syntax-automatically '(save mode-enabled))
;;   :config
;;   ;; Disable Flymake when Flycheck is enabled
;;   (add-hook 'flycheck-mode-hook
;;             (lambda () (flymake-mode -1))))

;;; YAsnippet

(use-package yasnippet
  :ensure t
  :defer t
  :init
  (yas-global-mode 1) ; Enable yasnippet globally
  :custom
  (yas-snippet-dirs
   (list
    (expand-file-name "snippets/" user-emacs-directory) ; Custom snippets at ~/.config/emacs/snippets/
    (when (locate-library "yasnippet-snippets")
      (expand-file-name "snippets" (file-name-directory (locate-library "yasnippet-snippets")))))) ; yasnippet-snippets snippets
  (yas-prompt-functions '(yas-completing-prompt)) ; Use completing-read for vertico
  (yas-choose-keys-first t) ; Sort snippets by key
  (yas-choose-tables-first t) ; Prioritize current major mode snippets
  :hook
  (after-init . yas-reload-all) ; Reload snippets after init
  :bind
  (:map yas-minor-mode-map
        ("C-c y" . yas-insert-snippet))) ; Trigger snippet insertion

;; Yasnippet-snippets: Predefined snippet collection
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (let ((snippets-dir (when (locate-library "yasnippet-snippets")
                        (expand-file-name "snippets" (file-name-directory (locate-library "yasnippet-snippets"))))))
    (when snippets-dir
      (unless (file-directory-p snippets-dir)
        (warn "yasnippet-snippets directory %s not found; consider reinstalling the package" snippets-dir)))))

;; Consult-yasnippet: Enhanced snippet selection with consult
(use-package consult-yasnippet
  :ensure t
  :after (yasnippet consult)
  :bind
  ("C-c Y" . consult-yasnippet)) ; Trigger consult-yasnippet

;;; helpful

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key))

;;; elisp-demos

(use-package elisp-demos
  :ensure t
  :config
  (advice-add
   'helpful-update
   :after #'elisp-demos-advice-helpful-update))

;;; 0x0

(use-package 0x0
  :ensure t
  :defer t
  :init
  (setq 0x0-server "https://0x0.st")
  (setq 0x0-use-curl t)
  ;; Define the prefix map before using it
  :config
  (setq 0x0-kill-ring-results t)
  (defvar my-0x0-prefix-map (make-sparse-keymap)
    "Prefix keymap for 0x0 commands.")
  (define-prefix-command 'my-0x0-prefix-map)
  ;; Bind the prefix command to "C-c 0"
  (global-set-key (kbd "C-c 0") 'my-0x0-prefix-map)
  ;; Define the subcommands under the prefix
  (define-key my-0x0-prefix-map (kbd "f") '0x0-upload-file)
  (define-key my-0x0-prefix-map (kbd "s") '0x0-shorten-uri)
  (define-key my-0x0-prefix-map (kbd "t") '0x0-upload-text)
  (define-key my-0x0-prefix-map (kbd "d") '0x0-dwim)
  (define-key my-0x0-prefix-map (kbd "p") '0x0-popup)
  ;; Optional: Integrate with which-key
  )

;;; Eshell - Emacs Shell
(use-package eshell
  :ensure nil
  :defer t
  :custom
  ;; Store eshell files in temp directory
  (eshell-directory-name (expand-file-name "eshell" my-tmp-dir))
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-prompt-regexp "^[^#$\n]*[#$] ")

  :config
  ;; Simple prompt with directory
  (setq eshell-prompt-function
        (lambda ()
          (concat (propertize (abbreviate-file-name default-directory)
                              'face 'font-lock-comment-face)
                  (if (= (user-uid) 0) " # " " $ "))))

  ;; Basic aliases
  (defalias 'eshell/ll 'eshell/ls)
  (defalias 'eshell/la '(lambda () (eshell/ls "-a")))
  (defalias 'eshell/clear 'eshell/clear-scrollback)

  :hook
  ;; Disable line numbers in eshell
  (eshell-mode . (lambda () (display-line-numbers-mode -1)))

  :bind
  ("C-c e" . eshell))

;; Syntax highlighting (optional but nice)
(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode 1))

;;; E MA I L EMAIL

(use-package message
  :ensure nil
  :defer t
  :config
  (setq message-citation-line-format "On %a, %b %d %Y, %N wrote:\n")
  (setq message-citation-line-function
        'message-insert-formatted-citation-line)
  (setq message-kill-buffer-on-exit t)
  (setq message-default-charset 'utf-8)
  (setq message-auto-save-directory
        (expand-file-name "gnus-drafts" my-tmp-dir)))

;;; Frame setup for daemon compatibility

(defun my-after-make-frame-setup (&optional frame)
  "Ensure UI elements are disabled for new frames, especially daemon clients.

This function explicitly disables menu-bar-mode, tool-bar-mode, and scroll-bar-mode
for the specified FRAME (or current frame if nil). This complements the frame
parameters set in early-init.el to ensure robust UI element disabling."
  (when (display-graphic-p frame)
    (with-selected-frame (or frame (selected-frame))
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))))

;; Apply to non-daemon initial frame (for regular Emacs startup)
(unless (daemonp)
  (when (display-graphic-p)
    (my-after-make-frame-setup)))

;; Apply to all new frames created later (essential for daemon mode)
(add-hook 'after-make-frame-functions #'my-after-make-frame-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 LaTeX templates                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil
  :config
  (let ((templates-dir "~/.config/emacs/latex/templates/"))
    (when (file-exists-p templates-dir)
      (dolist (file
               (directory-files-recursively templates-dir "\\.el$"))
        (load-file file)))))

;;; hl-line

(use-package hl-line
  :ensure nil ; Built-in, no need to install
  :commands (hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50)
  :hook
  (prog-mode . hl-line-mode)
  (occur-mode . hl-line-mode))

;;; sly

(use-package sly
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program "sbcl")
  :config  )

;;; xoauth2

(use-package auth-source-xoauth2-plugin
  :ensure t
  :defer t
  :custom (auth-source-xoauth2-plugin-mode t))

;;; .mailcap

(require 'mailcap)
(mailcap-parse-mailcaps)

;;; EAT - Emulate A Terminal
(use-package eat
  :ensure t
  :defer t
  :custom
  ;; Basic settings
  (eat-kill-buffer-on-exit t)
  (eat-query-before-killing-running-terminal nil)

  ;; Display settings
  (eat-enable-blinking-text t)
  (eat-term-scrollback-size 10000)  ; 100k might be excessive

  ;; Shell settings - use default login shell
  (eat-shell-command (list (getenv "SHELL") "-l"))

  ;; Integration settings
  (eat-eshell-visual-command-mode-map
   '(("git" . ("log" "diff" "show"))))  ; Commands that need visual mode

  :hook
  ;; Eshell integration
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)

  :bind
  ;; Global binding to launch eat
  ("C-c T" . eat))

;; Hide eat modes from modeline (if using delight)
(with-eval-after-load 'delight
  (delight '((eat-eshell-mode nil "eat")
             (eat-eshell-visual-command-mode nil "eat"))))

;;; Claude Code

(use-package claude-code
  :ensure t
  :defer t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c v" . claude-code-command-map))

;;; EditorConfig support optimized for Emacs 30.1

(use-package editorconfig
  :ensure nil  ; Built-in since Emacs 30.1
  :demand t    ; Load immediately for consistent behavior
  :config
  ;; Enable EditorConfig support globally
  (editorconfig-mode 1)

  ;; Enhanced EditorConfig properties for Emacs 30.1
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode ; Use ws-butler if available
        editorconfig-get-properties-function
        #'editorconfig-get-properties-from-exec) ; Use external EditorConfig for better compatibility

  ;; Comprehensive tree-sitter mode support
  (setq editorconfig-indentation-alist
        (append editorconfig-indentation-alist
                '((typescript-ts-mode typescript-indent-level)
                  (js-ts-mode js-indent-level)
                  (tsx-ts-mode tsx-indent-level)
                  (python-ts-mode python-indent-offset)
                  (c-ts-mode c-basic-offset)
                  (c++-ts-mode c-basic-offset)
                  (rust-ts-mode rust-indent-offset)
                  (yaml-ts-mode yaml-indent-offset))))

  ;; Exclude modes where EditorConfig shouldn't apply
  (setq editorconfig-exclude-modes
        '(help-mode
          magit-mode
          magit-diff-mode
          dired-mode
          ibuffer-mode
          minibuffer-mode))

  ;; Optimize for large projects
  (setq editorconfig-exec-path (executable-find "editorconfig"))

  (message "EditorConfig optimized for Emacs 30.1 with tree-sitter support"))

;;; aggressive indent

(use-package aggressive-indent
  :ensure t
  :hook ((prog-mode . aggressive-indent-mode)
         (org-mode . aggressive-indent-mode)))

;;; Tooltips (tooltip-mode)

(use-package tooltip
  :ensure nil
  :hook (after-init . tooltip-mode)
  :config
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.5
        x-gtk-use-system-tooltips t
        tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 10)
          (border-width . 0)
          (no-special-glyphs . t))))

;;; `man' (manpages)

(use-package man
  :ensure nil
  :defer t
  :commands (man)
  :config
  (setq Man-notify-method 'pushy))

;;; `proced' (process monitor, similar to `top')

(use-package proced
  :ensure nil
  :defer t
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible) ; Enhanced in 30.1
  (setq proced-enable-color-flag t)
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

;;; Emacs server (allow emacsclient to connect to running session)

(use-package server
  :ensure nil
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;;; pass (password-store) integration

(use-package password-store
  :ensure t)

(use-package password-store-otp
  :ensure t)

(use-package pass
  :ensure t
  :after (password-store password-store-otp)
  :bind (("C-c P" . pass))
  :commands (pass))

;;; isearch settings

(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq search-whitespace-regexp ".*?"
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil)
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4)
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq isearch-wrap-pause t
        isearch-repeat-on-direction-change t)
  (setq list-matching-lines-jump-to-current-line nil)
  :bind
  ( :map global-map
    ("M-s ." . isearch-forward-symbol-at-point)
    :map occur-mode-map
    ("t" . toggle-truncate-lines)
    :map isearch-mode-map
    ("C-g" . isearch-cancel)
    ("M-/" . isearch-complete)))

;;; Buffer naming
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

;;; Minibuffer enhancements
(use-package minibuffer
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (minibuffer-depth-indicate-mode 1))

;;; Better defaults for built-in behavior
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  ;; These are for when you're NOT using Vertico
  ;; (kept commented as reference)
  ;; (completion-cycle-threshold 3)
  ;; (completion-auto-help 'always)
  ;; (completions-detailed t)
  ;; (completions-format 'one-column)
  ;; (completions-max-height 10)
  )

;;; Shell (M-x shell)

(use-package shell
  :ensure nil
  :defer t
  :bind
  (
   ("C-c E" . shell)
   :map shell-mode-map
   ("C-c C-k" . comint-clear-buffer)
   ("C-c C-w" . comint-write-output))
  :config
  (setq shell-command-prompt-show-cwd t) ; Built-in since 27.1
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t)
  (setq shell-has-auto-cd nil)
  (setq shell-get-old-input-include-continuation-lines t) ; New in 30.1
  (setq shell-kill-buffer-on-exit t)
  (setq shell-completion-fignore '("~" "#" "%"))
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input)
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq tramp-default-remote-shell "/bin/bash")

  (setq shell-font-lock-keywords
        '(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
          ("^[^ \t\n]+:.*" . font-lock-string-face)
          ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

  (add-hook 'comint-output-filter-functions 'comint-osc-process-output))

;;; dictionary / definitions

(use-package dictionary
  :ensure nil
  :defer t
  :custom
  (dictionary-server "dict.org")
  :bind
  ("<f6>" . dictionary-lookup-definition))

;;; log file handling

(use-package logview
  :ensure t
  :mode (("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode)
         ("\\<\\(syslog\\|messages\\|error\\|debug\\|server\\|access\\|log\\)\\'" . logview-mode))
  :config
  ;; Infer log files based on content (timestamp patterns)
  (add-to-list 'magic-mode-alist
               '("^\\(?:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-+\\|[A-Za-z]\\{3\\}\\s-+[0-9]\\{1,2\\}\\s-+[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" . logview-mode)))

(use-package journalctl-mode
  :ensure t
  :defer t
  :bind (("C-c j" . journalctl)))

;;; pulsar

(use-package pulsar
  :ensure t
  :defer t
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'isearch)
  (pulsar-highlight-face 'pulsar-yellow)
  :hook
  ((minibuffer-setup . pulsar-pulse-line)
   (consult-after-jump . pulsar-recenter-middle)
   (consult-after-jump . pulsar-reveal-entry))
  :config
  (pulsar-global-mode 1))

;;; volatile highlighting

(use-package volatile-highlights
  :ensure t
  :init (volatile-highlights-mode 1))

;;; SMTP configuration for sending mail

(use-package smtpmail
  :ensure nil
  :config
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587
        smtpmail-debug-info t
        smtpmail-debug-verb t))

;;; mastodon

(use-package mastodon
  :ensure t
  :defer t
  :config
  (setq mastodon-active-user "blackdream"
        mastodon-instance-url "https://defcon.social")
  (mastodon-discover))

;;; stupid fucking emojis
;; im tired of the squares

(use-package emojify
  :ensure t
  :config
  (when (member "Noto Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Noto Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  :hook (after-init . global-emojify-mode))

;;; STARTUP INFORMATION

(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (when (fboundp 'profiler-cpu-start)
    (defun my/profile-init ()
      "Profile Emacs initialization."
      (interactive)
      (profiler-cpu-start)
      (add-hook 'after-init-hook
                (lambda ()
                  (profiler-cpu-stop)
                  (profiler-report)))))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs started in %.2f seconds with %d garbage collections"
                       (float-time (time-subtract after-init-time before-init-time))
                       gcs-done))))

;;; nerd icons completion
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ERC (IRC Client)                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package erc
  :ensure nil
  :defer t
  :config
  ;; Core settings
  (setq
   erc-server-coding-system '(utf-8 . utf-8)
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t
   erc-prompt (lambda () (concat "[" (or (erc-default-target) "ERC") "] "))
   erc-timestamp-format "[%H:%M] "
   erc-fill-function 'erc-fill-static
   erc-fill-static-center 20
   erc-fill-prefix "      "
   erc-log-channels-directory (expand-file-name "irc-logs" my-tmp-dir)
   erc-save-buffer-on-part t
   erc-save-queries-on-quit t
   erc-log-write-after-send t
   erc-log-write-after-insert t)

  ;; Modules: Enable only what we need, exclude services
  (setq erc-modules '(autojoin button completion fill irccontrols
                               list log match menu move-to-prompt netsplit
                               networks noncommands readonly ring stamp track
                               sasl))
  (erc-update-modules)

  ;; Built-in ERC tracking - shows only channels with activity
  (setq erc-track-enable-keybindings t    ; Enable C-c C-SPC
        erc-track-visibility t             ; Always visible
        erc-track-position-in-mode-line t  ; Show in mode-line
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
        erc-track-exclude-server-buffer t
        erc-track-shorten-start 8          ; Don't shorten short names
        erc-track-shorten-function nil     ; No shortening
        erc-track-switch-direction 'newest
        erc-track-showcount t
        erc-track-showcount-string ":")

  ;; Autojoin configuration
  (setq erc-autojoin-channels-alist
        '(("Libera.Chat" "#emacs" "#gnu" "#fsf" "#lisp" "#commonlisp"))
        erc-autojoin-timing 'ident
        erc-autojoin-delay 5)

  ;; Buffer behavior
  (setq erc-join-buffer 'bury
        erc-auto-query 'bury
        erc-query-display 'bury)

  ;; Connection function
  (defun my-erc-connect-libera ()
    "Connect to Libera Chat using SASL authentication."
    (interactive)
    (require 'erc-sasl)
    (let* ((host "irc.libera.chat")
           (port 6697)
           (auth (car (auth-source-search :host host
                                          :port (number-to-string port)
                                          :require '(:user :secret)
                                          :max 1)))
           (user (plist-get auth :user))
           (pass (let ((secret (plist-get auth :secret)))
                   (if (functionp secret) (funcall secret) secret))))
      (unless (and user pass)
        (error "No credentials found in authinfo.gpg"))
      (setq erc-sasl-user user
            erc-sasl-password pass)
      (erc-tls :server host :port port :nick user :full-name user)))

  ;; Simple buffer cycling
  (defun my-erc-next-channel ()
    "Switch to next ERC channel buffer."
    (interactive)
    (let ((channels (erc-channel-list nil)))
      (when channels
        (switch-to-buffer
         (or (cadr (member (current-buffer) channels))
             (car channels))))))

  (defun my-erc-prev-channel ()
    "Switch to previous ERC channel buffer."
    (interactive)
    (let ((channels (reverse (erc-channel-list nil))))
      (when channels
        (switch-to-buffer
         (or (cadr (member (current-buffer) channels))
             (car channels))))))

  (defun my-erc-latest-activity ()
    "Jump to channel with most recent activity."
    (interactive)
    (if erc-modified-channels-alist
        (switch-to-buffer (caar erc-modified-channels-alist))
      (message "No channel activity")))

  ;; Completion setup
  (defun my-erc-completion-setup ()
    "Enable completion-preview for ERC."
    (setq-local pcomplete-cycle-completions nil)
    (completion-preview-mode 1))

  ;; Use consult to switch between ALL ERC buffers
  (defun my-erc-switch-to-buffer ()
    "Switch to any ERC buffer using consult."
    (interactive)
    (let ((erc-buffers (mapcar #'buffer-name (erc-buffer-list))))
      (if erc-buffers
          (switch-to-buffer
           (completing-read "ERC buffer: " erc-buffers nil t))
        (message "No ERC buffers"))))

  :hook
  ((erc-mode . my-erc-completion-setup)
   (erc-mode . (lambda () (display-line-numbers-mode -1) (hl-line-mode 1)))
   (erc-insert-post . erc-save-buffer-in-logs))

  :bind
  (:map erc-mode-map
        ("TAB" . completion-at-point)
        ("M-<right>" . my-erc-next-channel)
        ("M-<left>" . my-erc-prev-channel)
        ("M-SPC" . my-erc-latest-activity)
        ("C-c C-b" . my-erc-switch-to-buffer)  ; Browse all ERC buffers
        ("C-c C-SPC" . erc-track-switch-buffer) ; Jump to active channel
        :map global-map
        ("C-c L" . my-erc-connect-libera)))

;;; csv-mode

(use-package csv-mode
  :ensure t
  :mode (("\\.[Cc][Ss][Vv]\\'" . csv-mode)
         ("\\.[Tt][Ss][Vv]\\'" . tsv-mode))
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . (lambda ()
                       (csv-header-line)
                       (hl-line-mode))))
  :config
  (setq csv-separators '("," ";" "|" "\t"))
  (setq csv-quote-char "\"")
  (setq csv-field-quotes '("\"" "'"))
  (setq csv-header-lines 1)
  (setq csv-align-style 'auto)
  (setq csv-align-padding 2)

  :bind (:map csv-mode-map
              ("C-c C-s" . csv-sort-fields)
              ("C-c C-r" . csv-reverse-region)
              ("C-c C-k" . csv-kill-fields)
              ("C-c C-t" . csv-transpose)
              ("C-c C-a" . csv-align-fields)
              ("C-c C-u" . csv-unalign-fields)
              ("C-c C-f" . csv-forward-field)
              ("C-c C-b" . csv-backward-field)
              ("C-c C-n" . csv-forward-record)
              ("C-c C-p" . csv-backward-record)))

;;; final cleanup

(provide 'init)

;;; init.el ends here
(put 'eshell 'disabled nil)
