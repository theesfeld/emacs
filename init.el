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

;;; security model

;; Configure the new security model for trusted directories
(when (boundp 'trusted-content)
  (setq trusted-content
        `(,(expand-file-name user-emacs-directory)     ; Trust .emacs.d
          ,(expand-file-name "~/Code/")                ; Trust ~/Code directory
          ,(expand-file-name "~/Documents/notes/")     ; Trust notes directory
          ,(expand-file-name "~/.config/emacs/")
          "/usr/share/emacs/"))     ; Trust config directory

  ;; Allow certain risky operations in trusted directories
  (setq trusted-content-allow-dangerous-local-variables 'safe
        trusted-content-allow-risky-eval 'prompt)

  (setq trusted-content-log-file
        (expand-file-name "trusted-content.log" user-emacs-directory)))

(add-hook 'package-menu-mode-hook #'hl-line-mode)

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

;;; EXWM

(defun my/gui-available-p ()
  "Check if GUI is available and suitable for EXWM."
  (and (display-graphic-p)
       (eq window-system 'x)
       (not (getenv "WAYLAND_DISPLAY"))))

(when (eq window-system 'x)
  (defun grim/run-in-background (command)
    (condition-case err
        (let ((command-parts (split-string command "[ ]+")))
          (apply #'call-process
                 `(,(car command-parts)
                   nil
                   0
                   nil
                   ,@
                   (cdr command-parts))))
      (error
       (message "Failed to run %s: %s"
                command
                (error-message-string err)))))

  (defun grim/exwm-init-hook ()
    ;; Ensure modeline is visible on all workspaces
    (setq-default mode-line-format
                  (default-value 'mode-line-format))
    (exwm-workspace-switch-create 1)
    ;; Force redisplay to ensure modeline appears
    (redisplay t)
    (display-battery-mode 1)
    (setq
     display-time-24hr-format t
     display-time-day-and-date t)
    (display-time-mode 1)
    (run-at-time 2 nil #'grim/run-in-background "nm-applet")
    (run-at-time 2 nil #'grim/run-in-background "udiskie -at")
    (run-at-time 2 nil #'grim/run-in-background "blueman-applet")
    (run-at-time 5 nil #'grim/run-in-background "mullvad-vpn")
    )

  (defun grim/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

  (defun grim/exwm-update-title ()
    (pcase exwm-class-name
      ("Firefox" (exwm-workspace-rename-buffer
                  (format "Firefox: %s" exwm-title)))))

  ;; Modern EXWM Multi-Monitor Configuration
  (defcustom grim/exwm-laptop-monitor "eDP-1"
    "Name of the laptop's built-in monitor."
    :type 'string
    :group 'exwm)

  (defcustom grim/exwm-laptop-scale 0.75
    "Scale factor for the laptop monitor when used alone."
    :type 'float
    :group 'exwm)

  (defcustom grim/exwm-laptop-dpi 192
    "DPI setting for the laptop monitor when used alone."
    :type 'integer
    :group 'exwm)

  (defcustom grim/exwm-external-dpi 96
    "DPI setting for external monitors."
    :type 'integer
    :group 'exwm)

  (defvar grim/exwm-debug-monitors nil
    "Enable debug logging for monitor configuration.")

  (defun grim/exwm-log (message &rest args)
    "Log MESSAGE with ARGS if debugging is enabled."
    (when grim/exwm-debug-monitors
      (message "[EXWM Monitor] %s" (apply #'format message args)))))

(use-package exwm
  :ensure t
  :when (eq window-system 'x)
  :config (setq exwm-workspace-number 10) ; Increased for multi-monitor support
  (add-hook 'exwm-update-class-hook #'grim/exwm-update-class)
  (add-hook 'exwm-update-title-hook #'grim/exwm-update-title)
  (add-hook 'exwm-init-hook #'grim/exwm-init-hook)
  ;; Ensure modeline is visible when switching workspaces
  ;; Single consolidated hook for EXWM buffer management
  (defun my/exwm-manage-finish-setup ()
    "Consolidated setup for newly managed EXWM windows."
    ;; Disable process query on exit
    (when-let ((proc (get-buffer-process (current-buffer))))
      (set-process-query-on-exit-flag proc nil))
    ;; Disable kill-buffer query
    (setq-local kill-buffer-query-functions nil))

  (add-hook 'exwm-manage-finish-hook #'my/exwm-manage-finish-setup)

  ;; Remove the global EXWM query function (do this once, not in a hook)
  (setq kill-buffer-query-functions
        (delq 'exwm-manage--kill-buffer-query-function kill-buffer-query-functions))

  ;; Ensure modeline is visible when switching workspaces
  (add-hook 'exwm-workspace-switch-hook
            (lambda ()
              (dolist (buffer (buffer-list))
                (with-current-buffer buffer
                  (when (not (eq major-mode 'exwm-mode))
                    (setq mode-line-format (default-value 'mode-line-format)))))))
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-manage-force-tiling nil)
  (setq mouse-autoselect-window t)
  (setq focus-follows-mouse t)
  (setq mouse-wheel-scroll-amount '(5 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed t)
  (setq
   x-select-enable-clipboard t
   x-select-enable-primary t
   select-enable-clipboard t)


  ;; Initialize empty - will be set by simple monitor handler
  (setq exwm-randr-workspace-monitor-plist nil)


  (defvar my-internal-monitor "eDP-1" "Name of the internal monitor.")

  (defun exwm-change-screen-hook ()
    (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
          (connected-monitors))
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (while (re-search-forward xrandr-output-regexp nil t)
          (push (match-string 1) connected-monitors))
        (setq connected-monitors (nreverse connected-monitors)))
      (let ((externals (remove my-internal-monitor connected-monitors)))
        (if (null externals)
            ;; Only internal monitor is connected
            (progn
              (call-process "xrandr" nil nil nil "--output" my-internal-monitor "--auto")
              (setq exwm-randr-workspace-monitor-plist (list 0 my-internal-monitor)))
          ;; External monitors are connected
          (let ((xrandr-args (list "--output" my-internal-monitor "--off"))
                (plist))
            (let ((first-external (car externals)))
              (setq xrandr-args (append xrandr-args (list "--output" first-external "--primary" "--auto")))
              (setq plist (list 0 first-external))
              (let ((previous first-external)
                    (workspace 1))
                (dolist (ext (cdr externals))
                  (setq xrandr-args (append xrandr-args (list "--output" ext "--right-of" previous "--auto")))
                  (setq previous ext)
                  (setq plist (append plist (list workspace ext)))
                  (setq workspace (1+ workspace)))))
            (apply 'call-process "xrandr" nil nil nil xrandr-args)
            (setq exwm-randr-workspace-monitor-plist plist))))))

  (add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)
  (require 'exwm-randr)
  (exwm-randr-mode 1)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 20)
  (setq exwm-systemtray-icon-gap 5)
  (exwm-systemtray-mode 1)

  ;; Input Prefix Keys
  (setq exwm-input-prefix-keys
        '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-y ?\M-& ?\M-: ?\C-\M-j ?\C-\ ))

  ;; Add XF86 media keys to prefix keys so they work in X windows
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86AudioMute
               XF86AudioPlay
               XF86AudioStop
               XF86AudioPrev
               XF86AudioNext
               XF86MonBrightnessUp
               XF86MonBrightnessDown
               XF86PowerOff))
    (cl-pushnew k exwm-input-prefix-keys))

  ;; Global keybindings
  (setq exwm-input-global-keys
        (nconc
         `(([?\s-r] . exwm-reset)
           ([s-left] . windmove-left)
           ([s-right] . windmove-right)
           ([s-up] . windmove-up)
           ([s-down] . windmove-down)
           ([?\s-w] . exwm-workspace-switch)
           ([?\s-&] . my/exwm-run-program)
           ([?\s-\ ]
            .
            (lambda ()
              (interactive)
              (my/exwm-run-program)))
           ([?\s-v] . consult-yank-pop)
           ([?\s-y] . consult-yank-pop)
           ([?\s-l] . desktop-environment-lock-screen)
           ([?\s-q]
            .
            (lambda ()
              (interactive)
              (kill-buffer-and-window)))
           ([XF86PowerOff]
            .
            (lambda ()
              (interactive)
              (when (executable-find "systemctl")
                (start-process-shell-command
                 "poweroff" nil "systemctl poweroff")))))
         (mapcar
          (lambda (i)
            (cons
             (kbd (format "s-%d" i))
             (lambda ()
               (interactive)
               (message "Switching to workspace %d" i)
               (exwm-workspace-switch-create i))))
          (number-sequence 0 9))
         (mapcar
          (lambda (i)
            (cons
             (kbd (format "M-s-%d" i))
             (lambda ()
               (interactive)
               (message "Moving window to workspace %d" i)
               (exwm-workspace-move-window i))))
          (number-sequence 0 9))))

  ;; Simulation Keys
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
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])))

  ;; Enable EXWM - this is required for EXWM to function!
  (exwm-wm-mode 1))

(use-package exwm-edit
  :ensure t
  :when (eq window-system 'x)
  :init
  ;; Pre-load settings
  (setq exwm-edit-default-major-mode 'text-mode) ; Default mode for editing
  :config
  ;; Explicitly load exwm-edit
  (require 'exwm-edit nil t)
  (when (featurep 'exwm-edit)
    (message "exwm-edit loaded successfully"))
  ;; Optional: Customize split behavior
  (setq exwm-edit-split 'below) ; Open edit buffer below current window
  :bind (:map exwm-mode-map ("C-c e" . exwm-edit--compose))
  :hook
  ;; Log initialization
  (exwm-init
   .
   (lambda ()
     (when (featurep 'exwm-edit)
       (message "exwm-edit initialized"))
     ;; Update global prefix keys to ensure M-x and media keys work in X windows
     (exwm-input--update-global-prefix-keys)
     (message "EXWM global prefix keys updated"))))

(use-package exwm-firefox-core
  :ensure t
  :when (eq window-system 'x)
  :init
  ;; Pre-load settings
  (setq exwm-firefox-core-classes
        '("firefox" "firefoxdeveloperedition"))

  ;; Define Firefox-specific minor mode before package loads
  (define-minor-mode exwm-firefox-mode
    "Minor mode for Firefox-specific keybindings in EXWM."
    :init-value nil
    :lighter " Firefox"
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-c F n") 'exwm-firefox-core-tab-new)
              (define-key map (kbd "C-c F t") 'exwm-firefox-core-tab-close)
              (define-key map (kbd "C-c F <right>") 'exwm-firefox-core-tab-right)
              (define-key map (kbd "C-c F <left>") 'exwm-firefox-core-tab-left)
              (define-key map (kbd "C-c F h") 'exwm-firefox-core-back)
              (define-key map (kbd "C-c F l") 'exwm-firefox-core-forward)
              (define-key map (kbd "C-c F f") 'exwm-firefox-core-find)
              (define-key map (kbd "C-c F r") 'exwm-firefox-core-reload)
              (define-key map (kbd "C-c F b") 'exwm-firefox-core-bookmark)
              map))

  :config
  ;; Load package safely
  (require 'exwm-firefox-core nil t)

  ;; Function to manage Firefox buffer setup
  (defun exwm-firefox-setup ()
    "Set up Firefox-specific configuration for current buffer."
    (when (and (derived-mode-p 'exwm-mode)
               (member (downcase (or exwm-class-name ""))
                       '("firefox" "firefoxdeveloperedition")))
      ;; Enable Firefox mode
      (exwm-firefox-mode 1)
      ;; Rename buffer to page title
      (when exwm-title
        (exwm-workspace-rename-buffer exwm-title))))

  ;; Function to handle buffer switches
  (defun exwm-firefox-maybe-enable ()
    "Enable or disable Firefox mode based on current buffer."
    (when (derived-mode-p 'exwm-mode)
      (if (member (downcase (or exwm-class-name ""))
                  '("firefox" "firefoxdeveloperedition"))
          (exwm-firefox-mode 1)
        (exwm-firefox-mode -1))))

  :hook
  ;; Set up Firefox buffers when they're created
  ((exwm-manage-finish . exwm-firefox-setup)
   ;; Update buffer name when title changes
   (exwm-update-title . exwm-firefox-setup)
   ;; Handle buffer switches to enable/disable mode
   (window-configuration-change . exwm-firefox-maybe-enable))

  :custom
  ;; Optional: Customize Firefox-specific EXWM settings
  (exwm-firefox-core-enable-auto-move-focus t)

  :init
  ;; Optional: Additional EXWM configuration for Firefox
  (with-eval-after-load 'exwm
    ;; Make Firefox windows floating by default (optional)
    ;; (add-to-list 'exwm-manage-configurations
    ;;              '((string-match "Firefox" exwm-class-name)
    ;;                floating t
    ;;                floating-mode-line nil))

    ;; Set Firefox-specific workspace (optional)
    ;; (add-to-list 'exwm-manage-configurations
    ;;              '((string-match "Firefox" exwm-class-name)
    ;;                workspace 2))
    ))

;;; Desktop Environment

(use-package desktop-environment
  :ensure t
  :when (eq window-system 'x)
  :init
  ;; Pre-configure settings before mode activation
  (setq desktop-environment-notifications t) ; Enable notifications
  (setq desktop-environment-screenshot-directory
        "~/Pictures/Screenshots")     ; Screenshot path
  (setq desktop-environment-screenlock-command "slock") ; Use slock for screen locking
  (setq
   desktop-environment-volume-get-command
   "pactl get-sink-volume @DEFAULT_SINK@ | awk '/Volume:/ {print $5}'")
  (setq desktop-environment-volume-get-regexp "\\([0-9]+%\\)")
  (setq desktop-environment-volume-set-command
        "pactl set-sink-volume @DEFAULT_SINK@ %s%%") ; Set volume
  (setq
   desktop-environment-mute-get-command
   "pactl get-sink-mute @DEFAULT_SINK@ | awk '{print ($2 == \"yes\") ? \"true\" : \"false\"}'")
  (setq desktop-environment-volume-toggle-command
        "pactl set-sink-mute @DEFAULT_SINK@ toggle") ; Toggle mute
  (setq desktop-environment-volume-normal-increment "+1") ; Volume step up
  (setq desktop-environment-volume-normal-decrement "-1") ; Volume step down

  :config
  ;; Ensure dependencies are installed
  (desktop-environment-mode 1)
  (dolist (cmd '("scrot" "slock" "pactl" "brightnessctl"))
    (unless (executable-find cmd)
      (message
       "Warning: %s not found; desktop-environment may not work fully"
       cmd))))

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
  (setq mode-line-compact t)  ; Emacs 28+ feature for compact mode-line
  (setq mode-line-position-column-line-format '(" (%l,%c)")) ; Line,Column format

  ;; File size indication in mode-line
  (size-indication-mode 1)

  ;; Show column numbers
  (column-number-mode 1)

  ;; Line number mode
  (line-number-mode 1)

  ;; Display time in mode-line
  (setq display-time-24hr-format t)
  (setq display-time-default-load-average nil) ; Don't show load average
  (setq display-time-format "%H:%M")          ; Simple time format
  (display-time-mode 1)

  ;; Battery display (if applicable)
  (require 'battery)
  (when (and battery-status-function
             (not (string-match-p "N/A"
                                  (battery-format "%B"
                                                  (funcall battery-status-function)))))
    (setq battery-mode-line-format " %b%p%%")
    (setq battery-mode-line-limit 85) ; Only show when below 85%
    (display-battery-mode 1))

  ;; Which function mode - shows current function in mode-line
  (which-function-mode 1)
  (setq which-func-modes '(prog-mode))

  ;; Cleaner buffer identification
  (setq mode-line-buffer-identification
        '(:eval (propertize "%b" 'face 'mode-line-buffer-id)))

  ;; Remote host indicator
  (setq-default mode-line-remote
                '(:eval (when (file-remote-p default-directory)
                          (propertize " @"
                                      'face 'font-lock-function-name-face))))

  ;; Project name in mode-line
  (with-eval-after-load 'project
    (setq-default mode-line-misc-info
                  (append mode-line-misc-info
                          '((:eval (when-let ((project (project-current)))
                                     (propertize (format " [%s]"
                                                         (project-name project))
                                                 'face 'font-lock-constant-face)))))))

  ;; Simpler recursive edit indication
  (setq-default mode-line-front-space
                '(:eval (if (> (recursion-depth) 0)
                            (propertize (format "[%d] " (recursion-depth))
                                        'face 'font-lock-warning-face)
                          " ")))

  ;; Custom mode-line faces for better visibility
  :custom-face
  ;; Subtle box that inherits theme colors
  (mode-line ((t (:box (:line-width -1 :style released-button)))))
  (mode-line-inactive ((t (:box (:line-width -1 :style released-button)))))
  ;; Make buffer name prominent using theme's keyword face color
  (mode-line-buffer-id ((t (:weight bold :inherit font-lock-keyword-face))))
  ;; Use warning face color for emphasis (themes always define this)
  (mode-line-emphasis ((t (:weight bold :inherit warning)))))

;; Minions - Better minor mode menu
;; This package automatically respects theme colors
(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  :custom
  ;; Show these minor modes directly
  (minions-prominent-modes '(flymake-mode
                             flycheck-mode
                             projectile-mode
                             lsp-mode
                             eglot--managed-mode))
  ;; Use a lighter symbol
  (minions-mode-line-lighter " ◎"))

;; Optional: Nyan Mode for fun (shows position in file)
;; Uncomment if you want a visual position indicator
;; (use-package nyan-mode
;;   :ensure t
;;   :config
;;   (nyan-mode 1)
;;   :custom
;;   (nyan-wavy-trail t)
;;   (nyan-bar-length 16))

;; Mode-line bell (visual bell in mode-line)
(use-package mode-line-bell
  :ensure t
  :config
  (mode-line-bell-mode 1))

;; Clean up mode-line clutter
(use-package emacs
  :ensure nil
  :config
  ;; Hide some default minor mode indicators
  (setq rm-excluded-modes
        '(" WS"    ; whitespace-mode
          " ws"    ; whitespace-mode
          " ElDoc" ; eldoc-mode
          " hl-p"  ; highlight-parentheses-mode
          " Wrap"  ; visual-line-mode
          " Vis"   ; visible-mode
          " VLin"  ; visual-line-mode
          " Undo-Tree" ; undo-tree-mode
          " MRev"  ; make-revision-mode
          " ARev"  ; auto-revert-mode
          ))

  ;; Cleaner version control display
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces)))

;; (add-hook 'enable-theme-functions
;;           (lambda (theme)
;;             (message "Mode-line will adapt to %s theme" theme)))

;; (when (facep 'modus-themes-heading-1)
;;   (set-face-attribute 'mode-line-buffer-id nil
;;                       :inherit 'modus-themes-heading-1))


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
  :ensure nil             ; Built-in since Emacs 29
  :demand t               ; Load immediately for optimal startup
  :config
  ;; Enable which-key mode globally
  (which-key-mode 1)

  ;; Optimized settings for Emacs 30.1
  (setq which-key-idle-delay 0          ; Slightly longer delay for better UX
        which-key-idle-secondary-delay 0.1
        which-key-add-column-padding 1
        which-key-max-description-length 50  ; More descriptive text
        which-key-show-early-on-C-h t
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-separator " → "
        which-key-prefix-prefix "+"
        which-key-sort-order 'which-key-key-order-alpha ; Better organization
        which-key-min-display-lines 3)      ; Minimum lines for better visibility

  ;; ===== PREFIX GROUP LABELS =====
  ;; Add bracketed labels for all major prefix groups

  ;; Main C-c prefix groups
  (which-key-add-key-based-replacements
    "C-c 0"   "[0x0] Upload Service"
    "C-c F"   "[Firefox] EXWM Browser"
    "C-c L"   "[LSP] Language Server"
    "C-c n"   "[Notes] Denote System")

  ;; Core C-c commands with descriptions
  (which-key-add-key-based-replacements
    "C-c >"   "[MC] Next Multiple Cursor"
    "C-c <"   "[MC] Previous Multiple Cursor"
    "C-c '"   "[Jump] Avy Goto Char"
    "C-c a"   "[Org] Agenda"
    "C-c c"   "[Org] Capture"
    "C-c d"   "[Diff] Ediff Dispatch"
    "C-c e"   "[EXWM] Edit Compose"
    "C-c g"   "[Git] Magit Status"
    "C-c r"   "[Recent] Consult Recent File"
    "C-c s"   "[Search] Deadgrep"
    "C-c y"   "[Snippet] YASnippet Insert")

  ;; 0x0 Upload Service subcommands
  (which-key-add-key-based-replacements
    "C-c 0 f" "Upload File"
    "C-c 0 s" "Shorten URI"
    "C-c 0 t" "Upload Text"
    "C-c 0 d" "Do What I Mean"
    "C-c 0 p" "Upload Popup")

  ;; Firefox EXWM subcommands
  (which-key-add-key-based-replacements
    "C-c F n" "New Tab"
    "C-c F t" "Close Tab"
    "C-c F <right>" "Next Tab"
    "C-c F <left>" "Previous Tab"
    "C-c F h" "Back"
    "C-c F l" "Forward"
    "C-c F f" "Find"
    "C-c F r" "Reload"
    "C-c F b" "Bookmark")

  ;; LSP subcommands
  (which-key-add-key-based-replacements
    "C-c L a" "Code Actions"
    "C-c L d" "Diagnostics"
    "C-c L s" "Symbols"
    "C-c L f" "File Symbols"
    "C-c L i" "Implementation"
    "C-c L r" "References"
    "C-c L D" "Definition")

  ;; Denote Notes subcommands
  (which-key-add-key-based-replacements
    "C-c n n" "New Note"
    "C-c n j" "Journal Entry"
    "C-c n d" "Dired Notes"
    "C-c n g" "Grep Notes"
    "C-c n l" "Link"
    "C-c n L" "Add Links"
    "C-c n b" "Backlinks"
    "C-c n q" "[Query] Links"
    "C-c n q c" "Query Contents Link"
    "C-c n q f" "Query Filenames Link"
    "C-c n r" "Rename File"
    "C-c n R" "Rename Using Front Matter"
    "C-c n f" "Find Note")

  ;; Enhanced navigation and utility keys
  (which-key-add-key-based-replacements
    "C-x u"   "[Undo] Vundo Tree"
    "C-="     "[Region] Expand"
    "C-& y"   "[Snippet] Consult YASnippet")

  ;; Super key bindings with windower
  (which-key-add-key-based-replacements
    "s-<tab>"   "[Windower] Last Buffer"
    "s-o"     "[Windower] Toggle Single"
    "s-\\"    "[Windower] Toggle Split"
    "s-+"     "[Text] Increase Size & Pane"
    "s-_"     "[Text] Decrease Size & Pane")

  ;; eww browser mode keybindings
  (which-key-add-key-based-replacements
    "C-c C-f" "[EWW] Open in Firefox"
    "J"       "[EWW] Next Buffer"
    "K"       "[EWW] Previous Buffer"
    "+"       "[EWW] Increase Text Size"
    "-"       "[EWW] Decrease Text Size"
    "0"       "[EWW] Reset Text Size"
    "V"       "[EWW] View Source"
    "I"       "[EWW] Toggle Images"
    "W"       "[EWW] Copy Page Title"
    "D"       "[EWW] Download as PDF"
    "B"       "[EWW] Bookmark with Tags"
    "C-c /"   "[EWW] New Search"
    "C-c C-o" "[EWW] Bookmark to Firefox"))

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

;;; eglot / lsp

(use-package eglot
  :ensure nil
  :defer t
  :commands (eglot eglot-ensure)
  :custom
  (eglot-sync-connect nil)                   ; Async connection
  (eglot-events-buffer-size 0)              ; Disable events buffer for performance
  (eglot-autoshutdown t)                     ; Auto-shutdown unused servers
  (eglot-extend-to-xref t)                   ; Better xref integration
  (eglot-workspace-configuration
   '((:pylsp . (:plugins (:pycodestyle (:enabled :json-false)
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :flake8 (:enabled t)
                                       :autopep8 (:enabled :json-false)
                                       :yapf (:enabled :json-false)
                                       :black (:enabled t))))))
  :config
  ;; Server configurations with performance focus
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp")))
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode) . ("typescript-language-server" "--stdio")))

  ;; Optimize for large projects
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider           ; Reduce visual noise
          :documentFormattingProvider         ; Use dedicated formatters
          :documentRangeFormattingProvider))

  :hook
  ;; Selective activation for better performance
  ((python-mode python-ts-mode) . eglot-ensure)
  ((js-mode js-ts-mode typescript-mode typescript-ts-mode) . eglot-ensure))

(use-package consult-lsp
  :ensure t
  :after (eglot consult)
  :bind
  (:map
   eglot-mode-map
   ("C-c L a" . consult-lsp-code-actions)
   ("C-c L d" . consult-lsp-diagnostics)
   ("C-c L s" . consult-lsp-symbols)
   ("C-c L f" . consult-lsp-file-symbols)
   ("C-c L i" . consult-lsp-implementation)
   ("C-c L r" . consult-lsp-references)
   ("C-c L D" . consult-lsp-definition)))

;;; org-mode

;; Core Org Mode Configuration
(setq org-directory "~/Documents/notes/") ; had to set this prior to loading org
(use-package org
  :ensure nil
  :custom
  ;; Basic settings
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-image-actual-width '(400))           ; Limit image size for performance
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-agenda-files-cache-time 600)
  :config
  ;; TODO states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)"
                    "|" "DONE(d!)" "CANCELED(c@)")))

  ;; Simple org-agenda-files setup
  (setq org-agenda-files (list org-directory))

  :hook
  ((org-mode . visual-line-mode)
   (org-mode . org-indent-mode)))

;; Capture Templates
(use-package org-capture
  :ensure nil
  :after org
  :config
  (setq org-capture-templates
        `(("t" "Todo" entry
           (file ,(expand-file-name "todo.org" org-directory))
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :empty-lines 1)
          ("r" "RSS Feed" entry
           (file ,(expand-file-name "rss.org" org-directory))
           "* %:description\n:PROPERTIES:\n:RSS_URL: %:link\n:END:\n"
           :immediate-finish t))))

;; Agenda Configuration
(use-package org-agenda
  :ensure nil
  :after org
  :custom
  ;; Performance improvements
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)

  ;; Enhanced custom commands with better performance
  (org-agenda-custom-commands
   '(("t" "All TODOs" todo "TODO|NEXT|WAITING"
      ((org-agenda-overriding-header "All Open TODOs")
       (org-agenda-max-entries 50)))          ; Limit entries for performance
     ("d" "Today's Agenda" agenda ""
      ((org-agenda-span 'day)
       (org-agenda-overriding-header "Today")
       (org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          "......" "----------------")))))))

;; Protocol support for browser integration
(use-package org-protocol
  :ensure nil
  :after org
  :config
  ;; No additional config needed, just load it
  (require 'org-protocol))

;; Export Configuration
(use-package ox
  :ensure nil
  :after org
  :config
  ;; Load built-in export backends
  (require 'ox-html)
  (require 'ox-latex)
  (require 'ox-md)
  (require 'ox-odt)

  ;; HTML export settings
  (setq org-html-validation-link nil)
  (setq org-html-head-include-scripts nil)
  (setq org-html-head-include-default-style nil)

  ;; LaTeX/PDF export settings
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f")))

;; Optional: GitHub Flavored Markdown export
(use-package ox-gfm
  :ensure t
  :after ox)

;; Optional: Pandoc export (provides DOCX and many other formats)
(use-package ox-pandoc
  :ensure t
  :after ox
  :config
  (setq org-pandoc-options '((standalone . t))))

;; Auto-tangle for literate config
(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode))

;; Standard Org keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("citywide"
                 "\\documentclass{citywide}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))

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

;;; magit / forge

(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
  :config
  (defun my/magit-set-log-margin ()
    (when (magit-git-repo-p default-directory)
      (let ((commit-count (string-to-number (magit-git-string "rev-list" "--count" "HEAD"))))
        (when (>= commit-count 1000)
          (setq-local magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))))))
  (add-hook 'magit-mode-hook #'my/magit-set-log-margin)
  (setq git-commit-summary-max-length 50)
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-diff-refine-hunk t)
  (when (require 'nerd-icons nil t)
    (setq magit-format-file-function #'magit-format-file-nerd-icons))
  :bind ("C-c g" . magit-status))

(use-package magit-repos
  :ensure nil ; part of `magit'
  :commands (magit-list-repositories)
  :after magit
  :init
  (setq magit-repository-directories
        '(("~/Code" . 1))))

(use-package forge
  :ensure t
  :after magit
  :custom
  ;; Performance settings
  (forge-database-connector 'sqlite-builtin) ; Use built-in SQLite
  (forge-pull-notifications nil)             ; Reduce API calls
  :config
  ;; Optimize API usage
  (setq forge-topic-list-limit 100))         ; Limit topic fetching

(use-package magit-todos :ensure t :after magit :config (magit-todos-mode 1))

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

;;; dired

(use-package dired
  :ensure nil
  :bind
  (("C-x C-j" . dired-jump)
   :map dired-mode-map
   ("M-o" . dired-omit-mode)
   :map dired-mode-map
   ("RET" . dired-find-alternate-file)
   ("<backspace>" . dired-up-directory)
   ("C-c C-e" . wdired-change-to-wdired-mode)
   ("C-c C-g" . dired-git-info-mode) ; Changed from C-c g to avoid conflict with magit-status
   ("C-c t" . dired-toggle-read-only)
   ("M-!" . dired-smart-shell-command)
   ("C-c o" . dired-open-externally)
   ("C-c w" . dired-copy-file-path)
   ("C-c f" . dired-consult-filter)
   ("C-c e" . dired-open-eshell)
   ("C-c t" . dired-open-eat))
  :hook
  (;;(dired-mode . dired-hide-details-mode)
   (dired-mode . nerd-icons-dired-mode)
   ;;   (dired-mode . dired-preview-mode)
   (dired-mode . hl-line-mode))
  :custom
  (dired-listing-switches "-lah --group-directories-first --time=access")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-auto-revert-buffer t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open")))
  (dired-use-ls-dired t)
  (dired-git-info-auto-enable t)
  :config
  (require 'dired-x)
  (require 'consult)
  (put 'dired-find-alternate-file 'disabled nil)

  (defun dired-open-eshell ()
    "Open an eshell buffer in the directory at point in Dired."
    (interactive)
    (let ((dir (dired-get-file-for-visit)))
      (if (file-directory-p dir)
          (progn
            (eshell)
            (cd dir))
        (message "Not a directory"))))

  (defun dired-open-eat ()
    "Open an eat buffer in the directory at point in Dired."
    (interactive)
    (let ((dir (dired-get-file-for-visit)))
      (if (file-directory-p dir)
          (progn
            (eat)
            (eat-cd dir))
        (message "Not a directory"))))

  (defun dired-open-externally ()
    "Open file under cursor with xdg-open."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (start-process "dired-open" nil "xdg-open" file)))

  (defun dired-copy-file-path ()
    "Copy the full path of the file under cursor to kill ring."
    (interactive)
    (let ((path (dired-get-file-for-visit)))
      (kill-new path)
      (message "Copied path: %s" path)))

  (defun dired-consult-filter ()
    "Filter Dired buffer using Consult narrowing."
    (interactive)
    (consult-focus-lines
     (lambda (file)
       (string-match-p
        (regexp-quote (consult--read "Filter: ")) file))))

  (use-package diredfl :ensure t :config (diredfl-global-mode 1))
  (use-package nerd-icons-dired
    :ensure t
    :after (nerd-icons dired)
    :hook (dired-mode . nerd-icons-dired-mode))

  (use-package dired-git-info
    :ensure t
    :custom
    (dgi-auto-hide-details-p nil)
    :config
    (setq dired-git-info-format " (%s)")
    (define-key dired-mode-map ")" 'dired-git-info-mode))

  (use-package dired-subtree
    :ensure t
    :bind
    (:map
     dired-mode-map
     ("<tab>" . dired-subtree-toggle)
     ("<C-tab>" . dired-subtree-cycle))
    :config
    (setq dired-subtree-use-backgrounds nil)
    ;; Subtle background for visual subtree depth indication
    ;; Using a dark, muted background from modus-vivendi palette for hierarchy
    (set-face-attribute 'dired-subtree-depth-1-face nil
                        :background "#3b4252")) ; Dark subtle background from modus-vivendi scheme
  (use-package dired-async
    :ensure nil
    :after dired
    :config (dired-async-mode 1)))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map ("C-c n" . dired-narrow-fuzzy)))

(use-package dired-rainbow
  :ensure t
  :config
  (dired-rainbow-define-chmod executable-unix "green" ".*x.*"))

;;; eww browser

(use-package eww
  :ensure nil ; built-in package
  :defer t
  :commands (eww eww-browse-url)
  :init
  ;; Set eww as the default browser for certain contexts
  (setq browse-url-browser-function 'eww-browse-url)

  :config
  ;; Core eww settings for better browsing experience
  (setq eww-search-prefix "https://duckduckgo.com/html?q="  ; Privacy-focused search
        eww-download-directory "~/Downloads/"
        eww-bookmarks-directory "~/.config/emacs/eww-bookmarks/"
        eww-history-limit 150
        eww-use-external-browser-for-content-type "\\`\\(video/\\|audio\\)" ; Use external browser for media
        eww-browse-url-new-window-is-tab nil
        eww-form-checkbox-selected-symbol "[X]"
        eww-form-checkbox-symbol "[ ]"
        eww-header-line-format "%t: %u"
        shr-use-colors t
        shr-use-fonts t
        shr-indentation 2
        shr-width 80
        shr-max-image-proportion 0.7
        shr-image-animate nil  ; Don't animate images by default
        shr-discard-aria-hidden t
        shr-cookie-policy 'same-origin)

  ;; Custom functions for enhanced functionality
  (defun eww-open-in-firefox ()
    "Open the current EWW UR in Firefox via EXWM.
This function integrates with exwm-firefox-core to open the current page."
    (interactive)
    (when (derived-mode-p 'eww-mode)
      (let ((url (eww-current-url)))
        (if url
            (progn
              (message "Opening %s in Firefox..." url)
              (start-process "firefox" nil "firefox" url))
          (message "No URL to open")))))

  (defun eww-download-pdf ()
    "Download the current page as PDF using an external tool."
    (interactive)
    (let ((url (eww-current-url)))
      (if url
          (let ((filename (expand-file-name
                           (concat (format-time-string "%Y%m%d-%H%M%S")
                                   "-"
                                   (replace-regexp-in-string "[^a-zA-Z0-9]" "-" (or (plist-get eww-data :title) "page"))
                                   ".pdf")
                           eww-download-directory)))
            (message "Downloading PDF to %s..." filename)
            (start-process "wkhtmltopdf" nil "wkhtmltopdf" url filename))
        (message "No URL to download"))))

  (defun eww-toggle-images ()
    "Toggle whether images are loaded in EWW."
    (interactive)
    (setq shr-inhibit-images (not shr-inhibit-images))
    (eww-reload)
    (message "Images are now %s" (if shr-inhibit-images "disabled" "enabled")))

  (defun eww-increase-text-size ()
    "Increase text size in EWW buffer."
    (interactive)
    (text-scale-increase 1))

  (defun eww-decrease-text-size ()
    "Decrease text size in EWW buffer."
    (interactive)
    (text-scale-decrease 1))

  (defun eww-reset-text-size ()
    "Reset text size in EWW buffer to default."
    (interactive)
    (text-scale-set 0))

  (defun eww-view-source ()
    "View the HTML source of the current page."
    (interactive)
    (let ((source (plist-get eww-data :source)))
      (when source
        (with-current-buffer (get-buffer-create "*eww-source*")
          (delete-region (point-min) (point-max))
          (insert source)
          (html-mode)
          (display-buffer (current-buffer))))))

  (defun eww-copy-page-title ()
    "Copy the title of the current page to the kill ring."
    (interactive)
    (let ((title (plist-get eww-data :title)))
      (if title
          (progn
            (kill-new title)
            (message "Copied: %s" title))
        (message "No title found"))))

  (defun eww-open-bookmark-in-firefox ()
    "Open the selected bookmark in Firefox."
    (interactive)
    (let ((bookmark (eww-read-bookmark)))
      (when bookmark
        (start-process "firefox" nil "firefox" (plist-get bookmark :url)))))

  ;; Enhanced bookmark functionality
  (defun eww-bookmark-with-tags ()
    "Bookmark the current page with optional tags."
    (interactive)
    (let ((tags (read-string "Tags (comma-separated): ")))
      (eww-add-bookmark)
      (when (and tags (not (string-empty-p tags)))
        ;; Store tags in bookmark (would need custom bookmark format)
        (message "Bookmark saved with tags: %s" tags))))

  :bind
  (:map eww-mode-map
        ;; Custom keybindings only - not re-declaring defaults
        ;; Default keys preserved: g (reload), G (search), l/r (back/forward),
        ;; H (history), b (bookmarks), d (download), w (copy-url), & (external browser)

        ;; Open in Firefox - using C-c C-f for consistency with Emacs conventions
        ("C-c C-f" . eww-open-in-firefox)

        ;; Additional navigation helpers
        ("J" . eww-next-buffer)      ; Quick buffer switching
        ("K" . eww-previous-buffer)  ; Quick buffer switching

        ;; Content manipulation
        ("+" . eww-increase-text-size)  ; Zoom in
        ("-" . eww-decrease-text-size)  ; Zoom out
        ("0" . eww-reset-text-size)     ; Reset zoom

        ;; View and inspection
        ("V" . eww-view-source)      ; View page source
        ("I" . eww-toggle-images)    ; Toggle image loading

        ;; Enhanced functionality
        ("W" . eww-copy-page-title)  ; Copy page title (w is URL)
        ("D" . eww-download-pdf)     ; Download as PDF (d is save)
        ("B" . eww-bookmark-with-tags) ; Bookmark with tags

        ;; Quick search with different engines
        ("C-c /" . eww)              ; New search
        ("C-c C-o" . eww-open-bookmark-in-firefox)) ; Open bookmark in Firefox

  :hook
  ;; Hooks for better integration
  ((eww-mode . visual-line-mode)       ; Better line wrapping
   (eww-mode . (lambda ()
                 (setq-local scroll-conservatively 101) ; Smooth scrolling
                 (setq-local mouse-wheel-scroll-amount '(1 ((shift) . 1)))))))

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

;;; tree-sitter

;; Built-in tree-sitter configuration
(use-package treesit
  :ensure nil
  :defer t
  :init
  ;; Tell Emacs where to look for tree-sitter libraries
  (setq treesit-extra-load-path
        (list (expand-file-name "tree-sitter" user-emacs-directory)
              "/usr/local/lib/tree-sitter"    ; System-wide installation
              "/usr/lib/tree-sitter"))        ; Alternative system path

  (setq treesit-language-source-alist
        '((awk . ("https://github.com/Beaglefoot/tree-sitter-awk"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (commonlisp . ("https://github.com/theHamsta/tree-sitter-commonlisp"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (latex . ("https://github.com/latex-lsp/tree-sitter-latex"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (scheme . ("https://github.com/6cdh/tree-sitter-scheme"))
          (sql . ("https://github.com/DerekStride/tree-sitter-sql"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (typst . ("https://github.com/uben0/tree-sitter-typst"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

  ;; Major mode remapping
  (setq major-mode-remap-alist
        '((awk-mode . awk-ts-mode)
          (bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (css-mode . css-ts-mode)
          (html-mode . html-ts-mode)
          (mhtml-mode . html-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (json-mode . json-ts-mode)
          (makefile-mode . make-ts-mode)
          (makefile-gmake-mode . make-ts-mode)
          (markdown-mode . markdown-ts-mode)
          (python-mode . python-ts-mode)
          (sql-mode . sql-ts-mode)
          (toml-mode . toml-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  ;; Emacs 30.1+: Ensure parent mode relationships for proper integration
  (when (fboundp 'derived-mode-add-parents)
    (with-eval-after-load 'c-ts-mode
      (derived-mode-add-parents 'c-ts-mode '(c-mode)))
    (with-eval-after-load 'c++-ts-mode
      (derived-mode-add-parents 'c++-ts-mode '(c++-mode)))
    (with-eval-after-load 'python-ts-mode
      (derived-mode-add-parents 'python-ts-mode '(python-mode)))
    (with-eval-after-load 'js-ts-mode
      (derived-mode-add-parents 'js-ts-mode '(js-mode)))
    (with-eval-after-load 'typescript-ts-mode
      (derived-mode-add-parents 'typescript-ts-mode '(typescript-mode)))
    (with-eval-after-load 'yaml-ts-mode
      (derived-mode-add-parents 'yaml-ts-mode '(yaml-mode))))

  ;; Enhanced tree-sitter settings optimized for Emacs 30.1
  (setq treesit-font-lock-level 4              ; Maximum highlighting
        treesit-max-buffer-size (* 16 1024 1024) ; 16MB for modern systems
        treesit-defun-prefer-top-level t        ; Better function detection
        treesit-defun-name-function #'treesit-defun-name ; Use built-in function naming
        treesit-simple-indent-presets           ; Enhanced indentation support
        '((offset . treesit-simple-indent-offset)
          (line-start . (column 0))
          (line-end . (column (length (thing-at-point 'line))))
          (first-sibling . (treesit-simple-indent-sibling 1))
          (prev-sibling . (treesit-simple-indent-sibling 0)))
        ;; Performance optimizations for Emacs 30.1
        treesit-node-outdated-p #'treesit--node-outdated-p ; Use built-in optimization
        treesit-query-validate t))               ; Validate queries for safety

;; Enhanced treesit-auto for intelligent mode selection
(use-package treesit-auto
  :ensure t
  :after treesit
  :custom
  (treesit-auto-install 'prompt)   ; Prompt before installing grammars
  (treesit-auto-fallback-alist     ; Intelligent fallback mapping
   '((c-ts-mode . c-mode)
     (c++-ts-mode . c++-mode)
     (python-ts-mode . python-mode)
     (js-ts-mode . js-mode)
     (typescript-ts-mode . typescript-mode)
     (rust-ts-mode . rust-mode)
     (yaml-ts-mode . yaml-mode)))
  :config
  ;; Enhanced auto-mode configuration
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Error handling for missing grammars
  (setq treesit-auto-recipe-list
        (seq-filter (lambda (recipe)
                      (treesit-language-available-p
                       (treesit-auto-recipe-lang recipe)))
                    treesit-auto-recipe-list))

  ;; Enable with better error handling
  (condition-case err
      (global-treesit-auto-mode)
    (error
     (message "treesit-auto failed to initialize: %s" err)
     (message "Falling back to standard modes"))))

;;; tree-sitter languages

;; Python with tree-sitter
(use-package python
  :ensure nil
  :defer t
  :hook
  (python-ts-mode . (lambda ()
                      ;; Tree-sitter handles these automatically via treesit-major-mode-setup:
                      ;; - treesit-defun-type-regexp
                      ;; - forward-sexp-function
                      ;; - beginning-of-defun-function
                      ;; - imenu-create-index-function
                      ;; - which-function-functions
                      ;; Just ensure eglot starts
                      (eglot-ensure))))


;; JavaScript/TypeScript with tree-sitter
(use-package js
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2)
  :hook
  ((js-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure))

(use-package cc-mode
  :ensure nil
  :defer t
  :hook
  ((c-ts-mode c++-ts-mode) . eglot-ensure))

;; Rust with tree-sitter
(use-package rust-ts-mode
  :ensure nil
  :defer t
  :hook
  (rust-ts-mode . eglot-ensure))

;; Go with tree-sitter
(use-package go-ts-mode
  :ensure nil
  :defer t
  :hook
  (go-ts-mode . eglot-ensure))

;; Ruby with tree-sitter
(use-package ruby-ts-mode
  :ensure nil
  :defer t
  :hook
  (ruby-ts-mode . eglot-ensure))

;;; tree-sitter folding

;; Use built-in outline-minor-mode for code folding with tree-sitter
(use-package outline
  :ensure nil
  :after treesit
  :hook
  ((prog-mode . outline-minor-mode))
  :custom
  (outline-minor-mode-cycle t)      ; Enable TAB cycling
  (outline-minor-mode-highlight 'override))
;; Built-in outline keybindings work automatically:
;; TAB - cycle visibility (when outline-minor-mode-cycle is t)
;; C-c @ C-c - hide entry
;; C-c @ C-e - show entry
;; C-c @ C-l - hide leaves
;; C-c @ C-k - show branches
;; C-c @ C-q - hide sublevels
;; C-c @ C-a - show all
;; C-c @ C-t - hide body

;;; so-long

(use-package so-long :ensure nil :config (global-so-long-mode 1))

;;; flycheck

(use-package flycheck
  :ensure t
  :defer t
  :init
  (global-flycheck-mode)
  :config
  ;; Use right fringe for indicators (matching previous flymake config)
  (setq flycheck-indication-mode 'right-fringe)

  ;; Performance optimizations
  (setq flycheck-idle-change-delay 1.0)  ; Delay before checking after changes
  (setq flycheck-idle-buffer-switch-delay 0.5) ; Delay after switching buffers
  (setq flycheck-display-errors-delay 0.3)  ; Delay before showing errors at point

  ;; Display settings
  (setq flycheck-highlighting-mode 'symbols)  ; Highlight entire symbols
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Disable flycheck in scratch buffer
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (when (string= (buffer-name) "*scratch*")
                (flycheck-mode -1))))

  ;; For elisp, disable package-lint if not needed
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; Enable for specific modes
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'python-ts-mode-hook 'flycheck-mode)
  (add-hook 'c-ts-mode-hook 'flycheck-mode)
  (add-hook 'c++-ts-mode-hook 'flycheck-mode)
  (add-hook 'js-ts-mode-hook 'flycheck-mode)
  (add-hook 'typescript-ts-mode-hook 'flycheck-mode)
  (add-hook 'rust-ts-mode-hook 'flycheck-mode)

  :bind
  (:map flycheck-mode-map
        ("C-c ! l" . flycheck-list-errors)
        ("C-c ! n" . flycheck-next-error)
        ("C-c ! p" . flycheck-previous-error)
        ("C-c ! v" . flycheck-verify-setup)
        ("C-c ! c" . flycheck-clear)
        ("C-c ! e" . flycheck-explain-error-at-point)
        ("C-c ! s" . flycheck-select-checker)
        ("C-c ! d" . flycheck-disable-checker)))

(use-package elisp-lint
  :ensure t
  :commands (elisp-lint-buffer elisp-lint-file)
  :config (setq elisp-lint-ignored-validators '("package-lint")))

;;; flycheck-eglot

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;;; flyover

(use-package flyover
  :ensure t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flyover-mode)
  (setq flyover-levels '(error warning))
  (setq flyover-use-theme-colors t)
  (setq flyover-background-lightness 45)
  (setq flyover-percent-darker 40)
  (setq flyover-text-tint 'lighter) ;; or 'darker or nil
  (setq flyover-text-tint-percent 50)
  (setq flyover-checkers '(flycheck))
  (setq flyover-debug nil)
  (setq flyover-debounce-interval 0.2)
  (setq flyover-line-position-offset 1)
  (setq flyover-wrap-messages t)
  (setq flyover-max-line-length 80)
  (setq flyover-info-icon "🛈")
  (setq flyover-warning-icon "⚠")
  (setq flyover-error-icon "✘")
  (setq flyover-icon-left-padding 0.9)
  (setq flyover-icon-right-padding 0.9)
  (setq flyover-virtual-line-type 'curved-line-no-arrow)
  (setq flyover-hide-checker-name t)
  (setq flyover-show-at-eol nil)
  (setq flyover-hide-when-cursor-is-on-same-line t)
  (setq flyover-show-virtual-line t)
  )

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

;;; eshell settings

(use-package eshell
  :ensure nil ;; Built-in package
  :commands (eshell eshell-command)
  :bind
  (
   :map
   eshell-mode-map
   ("C-l" . eshell/clear)
   ("C-r" . eshell-history-backward)
   ("C-s" . eshell-history-forward)
   ("M-." . eshell-find-file-at-point)
   ("M-y" . consult-yank-pop)
   ("M-x" . execute-extended-command))
  :init
  ;; Initial settings before Eshell loads
  (setq eshell-directory-name (expand-file-name "eshell" my-tmp-dir)) ;; Store history and data
  (setq eshell-scroll-to-bottom-on-input 'all) ;; Scroll to bottom on input
  (setq eshell-scroll-to-bottom-on-output 'all) ;; Scroll on output
  (setq eshell-error-if-no-glob t) ;; Error on failed glob
  (setq eshell-hist-ignoredups t) ;; Ignore duplicate history entries
  (setq eshell-save-history-on-exit t) ;; Save history on exit
  (setq eshell-prefer-lisp-functions t) ;; Prefer external commands
  (setq eshell-destroy-buffer-when-process-dies t) ;; Kill buffer when process dies
  (setq eshell-visual-commands ;; Commands that need a terminal
        '("htop"
          "btop"
          "top"
          "less"
          "more"
          "vim"
          "nano"
          "ssh"
          "tail"
          "watch"
          "claude"
          "source"))
  (setq eshell-visual-subcommands ;; Subcommands needing terminal
        '(("git" "log" "diff" "show")))
  (setq eshell-buffer-maximum-lines 10000) ;; Buffer line limit
  (setq eshell-history-size 10000) ;; Large history size
  (setq eshell-prompt-regexp "^[^#$\n]*[#$] ") ;; Prompt regexp for parsing

  :config
  ;; Custom prompt with Git branch and color
  (defun my-eshell-prompt ()
    "Custom Eshell prompt with abbreviated path and Git branch."
    (let* ((path (abbreviate-file-name (eshell/pwd)))
           (git-branch
            (when (and (fboundp 'vc-git-branch) (vc-git-working-dir))
              (let ((branch (vc-git-branch)))
                (if branch
                    (concat " (" branch ")")
                  ""))))
           (prompt
            (concat
             (propertize path 'face '(:foreground "cyan"))
             (propertize (or git-branch "")
                         'face
                         '(:foreground "magenta"))
             (propertize " $ "
                         'face
                         '(:foreground "green" :weight bold)))))
      prompt))

  (setq eshell-prompt-function #'my-eshell-prompt)

  ;; Clear Eshell buffer
  (defun eshell/clear ()
    "Clear the Eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  ;; Open file at point
  (defun eshell-find-file-at-point ()
    "Find file at point in Eshell."
    (interactive)
    (let ((file (thing-at-point 'filename t)))
      (when file
        (find-file file))))

  ;; History navigation
  (defun eshell-history-backward ()
    "Cycle backward through Eshell history."
    (interactive)
    (eshell-bol)
    (eshell-previous-input 1))

  (defun eshell-history-forward ()
    "Cycle forward through Eshell history."
    (interactive)
    (eshell-bol)
    (eshell-next-input 1))

  ;; Disable visual distractions
  (defun my-eshell-disable-distractions ()
    "Disable line numbers and highlighting in Eshell and subprocess buffers."
    (display-line-numbers-mode -1)
    (when (fboundp 'hl-line-mode)
      (hl-line-mode -1)))

  ;; Truncate buffer when too large
  (defun my-eshell-truncate-buffer ()
    "Truncate Eshell buffer to `eshell-buffer-maximum-lines'."
    (when eshell-buffer-maximum-lines
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (forward-line (- eshell-buffer-maximum-lines))
          (delete-region (point-min) (point))
          (eshell-send-input)))))

  ;; Common Eshell aliases
  (defun my-eshell-setup-aliases ()
    "Define common Eshell aliases."
    (eshell/alias "ff" "find-file $1") ;; Open file in Emacs
    (eshell/alias "ll" "ls -lh --color=yes") ;; Long listing
    (eshell/alias "la" "ls -asl --color=yes") ;; Long listing with hidden
    (eshell/alias "cls" "eshell/clear") ;; Clear buffer
    (eshell/alias "emacs" "find-file $1") ;; Open in Emacs
    (eshell/alias "g" "git") ;; Git shorthand
    (eshell/alias "d" "dired $1") ;; Open Dired
    (eshell/alias "fd" "find-dired $PWD $1")
    (eshell/alias "rg" "rg --color=always"))

  ;; Optimize Eshell performance
  (setq eshell-modules-list
        '(eshell-alias
          eshell-basic
          eshell-cmpl
          eshell-dirs
          eshell-glob
          eshell-hist
          eshell-ls
          eshell-pred
          eshell-prompt
          eshell-script
          eshell-term
          eshell-unix))

  ;; Enable eshell-syntax-highlighting for better readability
  (use-package eshell-syntax-highlighting
    :ensure t
    :after eshell
    :config (eshell-syntax-highlighting-global-mode +1))

  ;; Enable eshell-git-prompt for advanced Git-aware prompts
  (use-package eshell-git-prompt
    :after eshell
    :config
    (eshell-git-prompt-use-theme 'powerline)) ;; Use powerline theme

  :hook
  ((eshell-mode . my-eshell-disable-distractions) ;; Disable distractions
   (eshell-mode . my-eshell-setup-aliases) ;; Setup aliases
   (eshell-pre-output-filter . my-eshell-truncate-buffer) ;; Truncate buffer
   (eshell-visual-subprocess-hook . my-eshell-disable-distractions)
   (eshell-mode . eat-eshell-visual-command-mode))) ;; Subprocess distractions

;; E MA I L EMAIL

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

;;; slime

(use-package slime
  :ensure t
  :defer t
  :hook
  ((lisp-mode . slime-mode) ;; Enable slime-mode for Lisp files
   (inferior-lisp-mode . inferior-slime-mode)) ;; Enhance inferior-lisp buffers
  :bind
  (:map
   slime-mode-map
   ("C-c C-c" . slime-compile-defun)         ;; Compile defun
   ("C-c C-k" . slime-compile-and-load-file) ;; Compile and load file
   ("C-c C-s" . slime-complete-form)         ;; Complete form at point
   ("C-c C-d d" . slime-describe-symbol)     ;; Describe symbol
   ("C-c C-d h" . slime-documentation-lookup) ;;  Lookup in CLHS
   ("M-." . slime-edit-definition)            ;; Go to definition
   ("M-," . slime-pop-find-definition-stack)) ;; Return from definition
  :custom
  ((slime-default-lisp 'sbcl) ;; Default to SBCL
   (slime-contribs
    '(slime-fancy ;; Load essential contribs
      slime-repl slime-asdf slime-fuzzy slime-autodoc))
   (slime-complete-symbol-function 'slime-fuzzy-complete-symbol) ;; Fuzzy completion
   (slime-fuzzy-completion-in-place t) ;; Complete in buffer
   (slime-autodoc-use-multiline-p t)   ;; Better autodoc display
   (slime-enable-evaluate-in-emacs t)  ;; Allow Emacs to evaluate Lisp
   (inferior-lisp-program "/bin/sbcl")      ;; Path to SBCL
   (slime-lisp-implementations         ;; Support multiple Lisps
    '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)
      (ccl ("ccl"))
      (clisp ("clisp" "-q")))))
  :config
  ;; Ensure SLIME doesn't override user keybindings (e.g., C-c x)
  (add-hook
   'slime-mode-hook
   (lambda ()
     (when (boundp 'slime-mode-map)
       (define-key slime-mode-map (kbd "C-c x") nil))))
  ;; Override SLIME REPL's DEL to work with paredit
  (add-hook
   'slime-repl-mode-hook
   (lambda ()
     (define-key
      slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key)
      nil)))
  ;; Start SLIME automatically when opening a Lisp file
  (defun my/start-slime ()
    (unless (slime-connected-p)
      (save-excursion (slime))))
  (add-hook 'slime-mode-hook #'my/start-slime)
  ;; Enable paredit in SLIME REPL for better Lisp editing
  (when (featurep 'paredit)
    (add-hook 'slime-repl-mode-hook #'enable-paredit-mode)))

;;; xoauth2

(use-package auth-source-xoauth2-plugin
  :ensure t
  :defer t
  :custom (auth-source-xoauth2-plugin-mode t))

;;; .mailcap

(require 'mailcap)
(mailcap-parse-mailcaps)

;;; EAT Terminal

(use-package eat
  :ensure t
  :defer t  ; Let it load when needed, not immediately
  ;; Remove the :init section entirely - no (require 'eat)
  :custom
  (eat-shell (list (or (executable-find "bash") "/bin/bash") "--login" "-i"))
  (eat-kill-buffer-on-exit t)
  (eat-enable-blinking-text t)
  ;;  (eat-enable-mouse t)
  (eat-enable-shell-prompt-annotation t)
  (eat-term-scrollback-size 100000)
  (eat-term-resize t)
  (eat-query-before-killing-running-terminal nil)
  (eat-eshell-fallback-if-stty-not-available t)
  :hook
  ((eshell-load . eat-eshell-mode)
   (eshell-load . eat-eshell-visual-command-mode))
  ;;   (eat-mode . eat-semi-char-mode))  ; Only once!
  :config
  ;; Set up shell integration directory properly
  (setq eat-term-name "xterm-256color")
  (with-eval-after-load 'eat
    (when-let ((eat-dir (file-name-directory (locate-library "eat"))))
      (setenv "EAT_SHELL_INTEGRATION_DIR" eat-dir)))
  :delight
  (eat-eshell-mode nil)
  (eat-eshell-visual-command-mode nil))

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
  :bind (("C-c p" . pass))
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

;;; General window and buffer configurations

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; Show the name of the current definition or heading for context (which-function-mode)

(use-package which-func
  :ensure nil
  :hook (after-init . which-function-mode)
  :config
  (setq which-func-modes '(prog-mode org-mode))
  (setq which-func-display 'mode)       ; Available in 30.1
  (setq which-func-unknown "")
  (setq which-func-format
        '((:propertize which-func-current
                       face bold
                       mouse-face mode-line-highlight))))

;;; General minibuffer settings

(use-package minibuffer
  :ensure nil
  :demand t
  :config
  ;; Optimized completion behavior for Emacs 30.1
  (setq tab-always-indent 'complete              ; TAB completes when at end of word
        completion-cycle-threshold 3             ; TAB cycles through completions
        completion-pcm-leading-wildcard t
        completion-category-defaults nil
        completion-auto-deselect nil
        completion-auto-help 'always
        completion-auto-select 'second-tab
        completion-show-help nil
        completion-show-inline-help nil
        completions-detailed t
        completions-format 'one-column
        completions-header-format ""
        completions-highlight-face 'completions-highlight
        completions-max-height 10
        completions-sort 'historical            ; Emacs 30.1 feature
        completion-eager-display 'auto
        minibuffer-completion-auto-choose t
        minibuffer-visible-completions nil
        ;; Emacs 30.1 specific optimizations
        completion-lazy-hilit t                 ; Lazy highlighting for performance
        completion-lazy-hilit-fn #'completion-lazy-hilit-highlight ; 30.1 feature
        minibuffer-depth-indicate-mode t        ; Show minibuffer depth
        minibuffer-prompt-properties            ; Enhanced prompt properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Enable advanced minibuffer features in Emacs 30.1
  (when (fboundp 'minibuffer-depth-indicate-mode)
    (minibuffer-depth-indicate-mode 1)))

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

;;; insert-uuid

(use-package insert-uuid
  :ensure t
  :defer t
  :vc (:url "https://github.com/theesfeld/insert-uuid")
  :bind (("C-c u" . insert-uuid)
         ("C-c U" . insert-uuid-random))
  :custom
  (insert-uuid-default-version 4)
  (insert-uuid-uppercase nil))

;;; BACKGROUND?

(use-package buffer-background
  :ensure t
  :vc (:url "https://github.com/theesfeld/buffer-background")
  :config
  (setq buffer-background-color-alist
        '(("*scratch*" . "#2d2d2d")
          ("*Messages*" . "#7fdb22")
          (org-mode . (:color "#1e1e2e" :opacity 0.9))))
  (buffer-background-global-mode 1))

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
