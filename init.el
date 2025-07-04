;;; init.el -*- lexical-binding: t -*-

;; Time-stamp: <Last changed 2025-07-04 14:36:27 by grim>

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

;;; MELPA

(setq package-vc-register-as-project nil) ; Emacs 30

;; === NATIVE COMPILATION OPTIMIZATION FOR EMACS 30.1 ===
;; Native compilation is enabled by default in 30.1 - optimize it
(when (and (fboundp 'native-compile-available-p)
           (native-compile-available-p))
  ;; Optimize native compilation settings
  (setq native-comp-speed 2                    ; Balance speed vs compile time
        native-comp-debug 0                    ; Disable debug for performance
        native-comp-verbose 0                  ; Reduce compilation noise
        native-comp-async-report-warnings-errors nil ; Less interruption
        native-comp-deferred-compilation t)    ; Compile in background

  ;; Increase native compilation job limit for modern systems
  (when (> (num-processors) 4)
    (setq native-comp-async-jobs-number (/ (num-processors) 2)))

  ;; Prioritize frequently used packages for native compilation
  (setq native-comp-bootstrap-deny-list
        '("tramp" "tramp-.*" "docker-tramp")) ; Avoid compiling tramp for stability

  (message "Native compilation optimized for Emacs 30.1 (jobs: %d)"
           native-comp-async-jobs-number))

;; === TRUSTED CONTENT SECURITY MODEL (EMACS 30.1) ===
;; Configure the new security model for trusted directories
(when (boundp 'trusted-content)
  (setq trusted-content
        `(,(expand-file-name user-emacs-directory)     ; Trust .emacs.d
          ,(expand-file-name "~/Code/")                ; Trust ~/Code directory
          ,(expand-file-name "~/Documents/notes/")     ; Trust notes directory
          ,(expand-file-name "~/.config/emacs/")))     ; Trust config directory

  ;; Allow certain risky operations in trusted directories
  (setq trusted-content-allow-dangerous-local-variables t
        trusted-content-allow-risky-eval t)

  (message "Trusted content directories configured for enhanced security"))

(add-hook 'package-menu-mode-hook #'hl-line-mode)

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

(global-set-key [remap keyboard-quit] #'prot/keyboard-quit-dwim)

(defun my/consult-yasnippet-with-minor-modes ()
  "Use Vertico to select YASnippets, considering active major and minor modes."
  (interactive)
  (require 'yasnippet)
  (require 'consult-yasnippet nil t)
  (let*
      ((major-mode-snippets (yas--table-get-create major-mode))
       (minor-mode-snippets
        (cl-loop
         for mode in minor-mode-list when
         (and (boundp mode)
              (symbol-value mode)
              (not (eq mode 'nerd-icons-completion-mode))) ; Exclude problematic mode
         append
         (let ((table (yas--table-get-create mode)))
           (when table
             (if (fboundp 'yas--table-all-templates)
                 (yas--table-all-templates table)
               (hash-table-values (yas--table-hash table)))))))
       (all-snippets
        (append
         (if (fboundp 'yas--table-all-templates)
             (yas--table-all-templates major-mode-snippets)
           (hash-table-values (yas--table-hash major-mode-snippets)))
         minor-mode-snippets)))
    (if all-snippets
        (consult-yasnippet all-snippets))))

  (global-set-key (kbd "C-& y") #'my/consult-yasnippet-with-minor-modes)

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
  (let* ((orig-scale
          (or (car (get 'text-scale-mode-amount 'customized-value))
              text-scale-mode-amount))
         (new-scale (+ orig-scale 1))
         (scale-factor
          (/ (float (expt text-scale-mode-step new-scale))
             (float (expt text-scale-mode-step orig-scale)))))
    (text-scale-increase 1)
    (enlarge-window-horizontally
     (round (* (window-width) (- scale-factor 1))))))

(global-set-key (kbd "s-+") 'increase-text-and-pane)

(defun decrease-text-and-pane ()
  "Decrease text size and adjust window width proportionally."
  (interactive)
  (let* ((orig-scale
          (or (car (get 'text-scale-mode-amount 'customized-value))
              text-scale-mode-amount))
         (new-scale (- orig-scale 1))
         (scale-factor
          (/ (float (expt text-scale-mode-step new-scale))
             (float (expt text-scale-mode-step orig-scale)))))
    (text-scale-decrease 1)
    (shrink-window-horizontally
     (round (* (window-width) (- 1 scale-factor))))))

(global-set-key (kbd "s-_") 'decrease-text-and-pane)

(defun my-org-download-images-from-capture ()
  (when (string-match-p ":website:" (buffer-string))
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\(http[^][]+\\)\\]\\[.*\\]\\]" nil t)
      (org-download-image (match-string 1))))
  (add-hook
   'org-capture-after-finalize-hook
   #'my-org-download-images-from-capture))

(defun my/toggle-buffer (buffer-name command)
  "Toggle a buffer with BUFFER-NAME, running COMMAND if it doesn't exist."
  (interactive)
  (unless (commandp command)
    (error "Second argument must be an interactive command"))
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer (get-buffer-window buffer))
        ;; If the buffer exists and is visible, hide it
        (quit-window nil (get-buffer-window buffer))
      ;; If it doesn't exist or isn't visible, start it or switch to it
      (if buffer
          (switch-to-buffer buffer)
        (call-interactively command)))))

(defun my-org-capture-delete-file-after-kill (&rest _)
  "Delete file if capture is aborted."
  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (delete-file (buffer-file-name))
    (message "Deleted aborted capture file: %s" (buffer-file-name))))

(declare-function completion-preview-insert "completion-preview")
(declare-function completion-preview-next-candidate
                  "completion-preview")
(declare-function completion-preview-prev-candidate
                  "completion-preview")
(declare-function completion-preview--hide "completion-preview")

;;; EDNC NOTIFICATIONS (DBUS)

(use-package ednc
  :ensure t
  :hook (after-init . ednc-mode)
  :config
  (use-package posframe :ensure t)
  (require 'consult)

  ;; === Configuration Variables ===
  (defgroup ednc-enhanced nil
    "Enhanced EDNC notifications."
    :group 'ednc)

  (defcustom ednc-notification-timeout 5
    "Seconds before notification auto-dismisses."
    :type 'integer
    :group 'ednc-enhanced)

  (defcustom ednc-notification-position 'top-right
    "Position for notifications."
    :type '(choice (const top-right)
                   (const top-left)
                   (const bottom-right)
                   (const bottom-left))
    :group 'ednc-enhanced)

  (defcustom ednc-notification-max-width 50
    "Maximum width of notification popup."
    :type 'integer
    :group 'ednc-enhanced)

  (defcustom ednc-history-limit 200
    "Maximum number of notifications to keep in history."
    :type 'integer
    :group 'ednc-enhanced)

  ;; === State Management ===
  (defvar ednc--active-notifications (make-hash-table :test 'equal))
  (defvar ednc--notification-history nil)
  (defvar ednc--notification-ring (make-ring ednc-history-limit))

  ;; === Faces (using theme colors) ===
  (defface ednc-notification-default-face
    '((t :inherit mode-line))
    "Default face for notifications.")

  (defface ednc-notification-app-face
    '((t :inherit mode-line :weight bold))
    "Face for application name in notifications.")

  (defface ednc-notification-time-face
    '((t :inherit font-lock-comment-face))
    "Face for timestamps.")

  ;; === Core Functions ===
  (defun ednc--format-for-display (notification)
    "Format NOTIFICATION for posframe display."
    (let* ((app-name (ednc-notification-app-name notification))
           (summary (ednc-notification-summary notification))
           (body (ednc-notification-body notification)))
      (concat
       (when app-name
         (concat (propertize app-name 'face 'ednc-notification-app-face)
                 "\n"))
       (propertize summary 'face 'ednc-notification-default-face)
       (when (and body (not (string-empty-p body)))
         (concat "\n" (propertize body 'face 'ednc-notification-default-face))))))

  (defun ednc--store-in-history (notification)
    "Store NOTIFICATION in history for consult browsing."
    (let* ((time (format-time-string "%Y-%m-%d %H:%M:%S"))
           (app (ednc-notification-app-name notification))
           (summary (ednc-notification-summary notification))
           (body (ednc-notification-body notification))
           (entry (propertize
                   (format "%s │ %s: %s"
                           (propertize time 'face 'ednc-notification-time-face)
                           (propertize (or app "System") 'face 'font-lock-keyword-face)
                           summary)
                   'notification notification
                   'time time
                   'body body)))
      (ring-insert ednc--notification-ring entry)
      (push entry ednc--notification-history)))

  (defun ednc--calculate-position (index)
    "Calculate position for notification at INDEX."
    (let ((spacing 10)
          (height 80))
      (pcase ednc-notification-position
        ('top-right (cons -20 (+ 40 (* index (+ height spacing)))))
        ('top-left (cons 20 (+ 40 (* index (+ height spacing)))))
        ('bottom-right (cons -20 (- -40 (* index (+ height spacing)))))
        ('bottom-left (cons 20 (- -40 (* index (+ height spacing))))))))

  (defun ednc--show-notification (old new)
    "Main notification handler."
    (when new
      (ednc--store-in-history new)
      (let* ((id (ednc-notification-id new))
             (buffer-name (format " *ednc-%d*" id))
             (notification-count (hash-table-count ednc--active-notifications))
             (position (ednc--calculate-position notification-count))
             ;; Use theme colors
             (bg-color (face-attribute 'mode-line :background))
             (fg-color (face-attribute 'mode-line :foreground))
             (border-color (face-attribute 'mode-line-inactive :background)))

        ;; Show posframe
        (posframe-show buffer-name
                       :string (ednc--format-for-display new)
                       :position (pcase ednc-notification-position
                                   ((or 'top-right 'bottom-right) (point-max))
                                   ((or 'top-left 'bottom-left) (point-min)))
                       :poshandler (pcase ednc-notification-position
                                     ('top-right #'posframe-poshandler-frame-top-right-corner)
                                     ('top-left #'posframe-poshandler-frame-top-left-corner)
                                     ('bottom-right #'posframe-poshandler-frame-bottom-right-corner)
                                     ('bottom-left #'posframe-poshandler-frame-bottom-left-corner))
                       :x-pixel-offset (car position)
                       :y-pixel-offset (cdr position)
                       :left-fringe 10
                       :right-fringe 10
                       :internal-border-width 12
                       :internal-border-color border-color
                       :background-color bg-color
                       :foreground-color fg-color
                       :accept-focus nil
                       :width ednc-notification-max-width)

        ;; Store and set timer
        (let ((timer (run-with-timer ednc-notification-timeout nil
                                     (lambda () (ednc--dismiss-notification id)))))
          (puthash id (list buffer-name timer) ednc--active-notifications))))

    ;; Handle removal
    (when (and old (not new))
      (ednc--dismiss-notification (ednc-notification-id old))))

  (defun ednc--dismiss-notification (id)
    "Dismiss notification with ID."
    (when-let ((data (gethash id ednc--active-notifications)))
      (let ((buffer-name (car data))
            (timer (cadr data)))
        (when (timerp timer) (cancel-timer timer))
        (posframe-delete buffer-name)
        (remhash id ednc--active-notifications)
        (ednc--reposition-all))))

  (defun ednc--reposition-all ()
    "Reposition all active notifications."
    (let ((index 0))
      (maphash (lambda (id data)
                 (let ((buffer-name (car data))
                       (position (ednc--calculate-position index)))
                   (when (posframe-workable-p)
                     ;; Use posframe-show to reposition, not posframe-refresh
                     (posframe-show buffer-name
                                    :position (pcase ednc-notification-position
                                                ((or 'top-right 'bottom-right) (point-max))
                                                ((or 'top-left 'bottom-left) (point-min)))
                                    :poshandler (pcase ednc-notification-position
                                                  ('top-right #'posframe-poshandler-frame-top-right-corner)
                                                  ('top-left #'posframe-poshandler-frame-top-left-corner)
                                                  ('bottom-right #'posframe-poshandler-frame-bottom-right-corner)
                                                  ('bottom-left #'posframe-poshandler-frame-bottom-left-corner))
                                    :x-pixel-offset (car position)
                                    :y-pixel-offset (cdr position))))
                 (cl-incf index))
               ednc--active-notifications)))

  ;; === Interactive Commands ===
  (defun ednc-dismiss-all ()
    "Dismiss all active notifications."
    (interactive)
    (maphash (lambda (id _) (ednc--dismiss-notification id))
             ednc--active-notifications))

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
  (add-hook 'ednc-notification-presentation-functions #'ednc--show-notification)
  (ednc-mode 1))

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
    (exwm-workspace-switch-create 1)
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
      (message "[EXWM Monitor] %s" (apply #'format message args))))


  ;; Only configure EXWM if running in suitable GUI environment
  (when (my/gui-available-p)
    (use-package
      exwm
      :ensure t
      :config (setq exwm-workspace-number 10) ; Increased for multi-monitor support
      (add-hook 'exwm-update-class-hook #'grim/exwm-update-class)
      (add-hook 'exwm-update-title-hook #'grim/exwm-update-title)
      (add-hook 'exwm-init-hook #'grim/exwm-init-hook)

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

      ;; Modern EXWM RandR Configuration
      (require 'exwm-randr)

      ;; Enable randr mode first
      (exwm-randr-mode 1)

      ;; Initialize empty - will be set by simple monitor handler
      (setq exwm-randr-workspace-monitor-plist nil)

      ;; Simple monitor change handler
      (add-hook 'exwm-randr-screen-change-hook
                (lambda ()
                  (let ((monitors (split-string (shell-command-to-string
                                                 "xrandr --listmonitors | grep -v '^Monitors:' | awk '{print $4}'") "\n" t)))
                    (when (>= (length monitors) 2)
                      ;; Assign workspace 0 to first monitor, workspace 1 to second monitor
                      (setq exwm-randr-workspace-monitor-plist
                            (list 0 (nth 0 monitors) 1 (nth 1 monitors)))
                      ;; Configure monitors: left monitor first, right monitor second
                      (start-process-shell-command
                       "xrandr" nil
                       (format "xrandr --output %s --auto --pos 0x0 --output %s --primary --auto --right-of %s --output eDP-1 --off"
                               (nth 0 monitors) (nth 1 monitors) (nth 0 monitors))))
                    (when (fboundp 'exwm-randr-refresh)
                      (exwm-randr-refresh)))))

      ;; To enable monitor debugging: (setq grim/exwm-debug-monitors t)
      ;; Load the system tray before exwm-init
      (require 'exwm-systemtray)
      (setq exwm-systemtray-height 20)
      (setq exwm-systemtray-icon-gap 5)
      (exwm-systemtray-mode 1)

      ;; Input Prefix Keys
      (setq exwm-input-prefix-keys
            '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-& ?\M-: ?\C-\M-j ?\C-\ ))

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
               ([?\s-x]
                .
                (lambda ()
                  (interactive)
                  (save-buffers-kill-emacs)))
               ([?\s-\ ]
                .
                (lambda ()
                  (interactive)
                  (async-shell-command)))
               ([?\s-v] . consult-yank-pop)
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

      ;;(exwm-wm-mode 1)
      (exwm-enable))) ; Close the when condition

  (when (my/gui-available-p)
    (use-package
      exwm-edit
      :ensure t
      :after exwm
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
           (message "exwm-edit initialized")))))

    (use-package exwm-firefox-core
      :ensure t
      :after exwm
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
        )


      (use-package
        desktop-environment
        :ensure t
        :after exwm
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
             cmd)))))))

;;; init.el version control

  (use-package
    vc
    :ensure nil
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

(use-package
  emacs
  :ensure nil ; Built-in, no need to install
  :init (server-start)

  (setq
   user-full-name "TJ"
   user-mail-address "tj@emacs.su"
   calendar-location-name "New York, NY"
   calendar-time-zone-rule "EST"
   calendar-standard-time-zone-name "EST"
   calendar-daylight-time-zone-name "EDT"
   auth-sources '("~/.authinfo.gpg")
   epg-pinentry-mode 'loopback
   create-lockfiles nil
   password-cache-expiry nil
   delete-pair-blink-delay 0.1
   next-error-recenter '(4)
   find-library-include-other-files nil
   remote-file-name-inhibit-delete-by-moving-to-trash t
   remote-file-name-inhibit-auto-save t
   save-interprogram-paste-before-kill t
   eval-expression-print-length nil
     scroll-error-top-bottom t
     ;; this
     echo-keystrokes-help nil
     delete-section-mode t
     x-stretch-cursor t
     help-window-select t
     auth-source-cache-expiry nil
     truncate-string-ellipsis "…" ; Visual ellipsis for truncated lines
     scroll-margin 1
     garbage-collection-messages nil
     plstore-cache-directory my-tmp-dir
     epg-gpg-program "gpg2"

     ;; Backup settings
     backup-by-copying t
     backup-directory-alist `((".*" . ,(expand-file-name "backups" my-tmp-dir)))
     delete-old-versions t
     kept-new-versions 6
     kept-old-versions 2
     version-control t

     ;; Auto-save settings
     auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" my-tmp-dir) t))
     auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my-tmp-dir)
     auto-save-default t)
    (setq fast-read-process-output t)

    (setenv "TZ" "America/New_York")
    (prefer-coding-system 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-language-environment "UTF-8")
    (save-place-mode 1)
    ;; History settings consolidated - see main emacs config block
    (setq history-delete-duplicates t)
    (setq savehist-save-minibuffer-history t)

    ;; Enable auto-insert for new files
    (require 'autoinsert)
    (auto-insert-mode 1)

    ;; When there is a "Time-stamp: <>" string in the first 10 lines of the file,
    ;; Emacs will write time-stamp information there when saving the file.
    ;; (Borrowed from http://home.thep.lu.se/~karlf/emacs.html)
    (setq
     time-stamp-active t ; Do enable time-stamps.
     time-stamp-line-limit 10 ; Check first 10 buffer lines for Time-stamp: <>
     time-stamp-format "Last changed %Y-%02m-%02d %02H:%02M:%02S by %u")
    (add-hook 'write-file-hooks 'time-stamp) ; Update when saving.

    ;; Simplified auto-insert: use inline lambdas instead of custom function
    ;; The time-stamp system already handles comment syntax automatically
    (setq auto-insert-alist
          '(;; Add time-stamps to programming and documentation files
            (prog-mode . (lambda () (insert (or comment-start "#") " Time-stamp: <>\n")))
            (org-mode . (lambda () (insert "#+Time-stamp: <>\n")))
            (text-mode . (lambda () (insert "# Time-stamp: <>\n")))))
    ;; Enable time-stamp updates on save
    (add-hook 'before-save-hook 'time-stamp)

    :config
    (setq modus-themes-italic-constructs t
          modus-themes-bold-constructs t)
    ;; Font configuration moved to consolidated section below
    (custom-set-faces
     '(cursor ((t (:background "#FFC107")))))
    (load-theme 'modus-vivendi t)

    ;; === CONSOLIDATED FONT CONFIGURATION ===
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

    ;; Fallback font sizing if custom font not available
    (unless (find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
      (set-face-attribute 'default nil :height 140)
      (set-face-attribute 'variable-pitch nil :height 160))

    ;; Font-lock face customization
    (set-face-attribute 'font-lock-comment-face nil
                        :slant 'italic
                        :weight 'light)
    (set-face-attribute 'font-lock-keyword-face nil
                        :weight 'black)


    (setq scroll-conservatively 101) ; Scroll line-by-line without recentering
    (when (file-exists-p custom-file)
      (load custom-file))
    ;; Global Emacs Settings
    (global-visual-line-mode 1)
    (setq-default
     default-directory '~
     kill-ring-max 5000
     indent-tabs-mode nil) ; Use spaces instead of tabs
    (setq
     tab-always-indent 'complete
     tab-width 2
     standard-indent 2
     scroll-conservatively 100000
     scroll-preserve-screen-position 1
     delete-by-moving-to-trash t
     window-combination-resize t
     display-time-load-average t
     savehist-file (expand-file-name "savehist" my-tmp-dir)
     ;; history-length set in main emacs config block
     history-delete-duplicates t
     savehist-save-minibuffer-history t
     undo-limit 800000
     ;; History settings - consolidated here for consistency
     history-length 10000
     history-delete-duplicates t
     isearch-lazy-count t
     lazy-count-prefix-format "(%s/%s) "
     lazy-count-suffix-format nil
     save-place-file (expand-file-name "saveplace/saveplace" my-tmp-dir))
    (fset 'yes-or-no-p 'y-or-n-p)
    (require 'auth-source)
    (require 'epa-file)
    (epa-file-enable)

    :hook
    ((text-mode . visual-wrap-prefix-mode)
     (before-save . whitespace-cleanup)
     (prog-mode . display-line-numbers-mode)
     (emacs-startup
      .
      (lambda ()
        (line-number-mode 1)
        (column-number-mode 1)
        (size-indication-mode 1)
        (global-auto-revert-mode 1)
        (display-time-mode 1))))

  :bind
  (("C-x k" . kill-current-buffer)
   ("C-x K" . kill-buffer))) ; Close the use-package emacs block

(when (my/gui-available-p)
  (use-package windower
    :ensure t
    :after exwm
    :config
    (global-set-key (kbd "<s-tab>") 'windower-switch-to-last-buffer)
    (global-set-key (kbd "<s-o>") 'windower-toggle-single)
    (global-set-key (kbd "s-\\") 'windower-toggle-split)

    (global-set-key (kbd "<s-M-left>") 'windower-move-border-left)
    (global-set-key (kbd "<s-M-down>") 'windower-move-border-below)
    (global-set-key (kbd "<s-M-up>") 'windower-move-border-above)
    (global-set-key (kbd "<s-M-right>") 'windower-move-border-right)

    (global-set-key (kbd "<s-S-left>") 'windower-swap-left)
    (global-set-key (kbd "<s-S-down>") 'windower-swap-below)
    (global-set-key (kbd "<s-S-up>") 'windower-swap-above)
    (global-set-key (kbd "<s-S-right>") 'windower-swap-right))) ; Close windower use-package and when block

;;; shell environment (path, etc)

(use-package
  exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (setq exec-path-from-shell-arguments '("-l"))
  ;; Only initialize if needed and not already done
  (when (and (memq window-system '(mac ns x))
             (not (getenv "EMACS_SHELL_INITIALIZED")))
    (exec-path-from-shell-initialize)
    (setenv "EMACS_SHELL_INITIALIZED" "1"))
  ;; Explicitly add ~/.local/bin to exec-path and PATH
  (let ((local-bin (expand-file-name "~/.local/bin")))
    (unless (member local-bin exec-path)
      (add-to-list 'exec-path local-bin t) ;; Add to end of exec-path
      (setenv "PATH" (concat local-bin ":" (getenv "PATH"))))))

;;; ediff settings

(use-package
  ediff
  :ensure nil
  :defer t
  :custom
  (ediff-split-window-function
   'split-window-right "Split windows vertically")
  (ediff-keep-variants
   nil "Do not keep variant buffers after quitting")
  :config
  ;; Customize faces to respect themes
  (custom-theme-set-faces 'user
                          '(ediff-current-diff-A
                            ((t
                              (:foreground
                               "red"
                               :background unspecified))))
                          '(ediff-fine-diff-A
                            ((t
                              (:foreground
                               "red"
                               :background unspecified))))
                          '(ediff-current-diff-B
                            ((t
                              (:foreground
                               "green"
                               :background unspecified))))
                          '(ediff-fine-diff-B
                            ((t
                              (:foreground
                               "green"
                               :background unspecified))))
                          '(diff-added
                            ((t
                              (:foreground
                               "green4"
                               :background unspecified))))
                          '(diff-removed
                            ((t
                              (:foreground
                               "red3"
                               :background unspecified)))))



  ;; Add convenient wrapper for ediff with current buffer
  (defun my-ediff-current-buffer-with-file ()
    "Compare current buffer with its file on disk."
    (interactive)
    (if buffer-file-name
        (ediff-current-file)
      (user-error "Current buffer is not visiting a file")))

  ;; Enhanced ediff quit that restores window configuration
  (defun my-ediff-quit-and-restore ()
    "Quit ediff and restore previous window configuration."
    (interactive)
    (when (and (boundp 'ediff-control-buffer) ediff-control-buffer)
      (ediff-quit t)))

  ;; Add keybinding for enhanced quit function in ediff sessions
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (define-key ediff-mode-map (kbd "Q") #'my-ediff-quit-and-restore)))

  :bind (("C-c d f" . ediff-files)        ; Compare two files
         ("C-c d b" . ediff-buffers)      ; Compare two buffers
         ("C-c d c" . my-ediff-current-buffer-with-file) ; Compare buffer with file
         ("C-c d d" . ediff-directories))) ; Compare directories

(with-eval-after-load 'ediff-wind
  (setq ediff-control-frame-parameters
        (cons '(unsplittable . t) ediff-control-frame-parameters)))

;;; tramp settings

(use-package
  tramp
  :ensure nil
  :defer t
  :config
  ;; Essential TRAMP optimizations only
  ;; Most defaults are sensible - only override what's necessary for performance
  (setq tramp-auto-save-directory (expand-file-name "tramp-auto-save" my-tmp-dir)
        tramp-verbose 1                    ; Minimal verbosity for performance
        tramp-default-method "ssh"         ; Reliable connection method
        auto-revert-remote-files t         ; Enable remote file auto-revert
        remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        remote-file-name-inhibit-auto-save-visited t)
  (setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
        tramp-verbose 2)
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))
  )

(use-package
  files
  :ensure nil
  :config

  ;; Log mode configuration
  (defvar log-mode-font-lock-keywords
    '(("\\bDEBUG\\b" . 'font-lock-comment-face)
      ("\\bINFO\\b" . 'font-lock-string-face)
      ("\\bWARN\\b" . 'font-lock-warning-face)
      ("\\bERROR\\b" . 'font-lock-function-name-face))
    "Font-lock keywords for `log-mode' highlighting.")

  (define-derived-mode
    log-mode
    fundamental-mode
    "Log"
    "A simple mode for log files."
    (setq font-lock-defaults '((log-mode-font-lock-keywords))))

  (add-to-list 'auto-mode-alist '("\\.log\\'" . log-mode))

  :hook
  (log-mode . auto-revert-tail-mode))

;;; vundo settings

(use-package
  vundo
  :ensure t
  :defer t
  :bind ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-files-directory (expand-file-name "undos" my-tmp-dir)))

;;; deadgrep

(use-package
  deadgrep
  :ensure t
  :defer t
  :bind
  (("C-c s" . deadgrep)
   :map
   deadgrep-mode-map
   ("q" . deadgrep-kill-all-buffers)))

;;; visual enhancements (rainbow/indent/highlight/...)

;;; Rainbow Delimiters

(use-package
  rainbow-delimiters
  :ensure t
  :defer t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom
  (rainbow-delimiters-max-face-count 9)) ;; Default 9 faces

;;; Highlight Thing at Point

(use-package
  highlight-thing
  :ensure t
  :defer t
  :custom
  (highlight-thing-delay-seconds 0.5)   ; Delay before highlighting
  (highlight-thing-what-thing 'symbol)  ; Highlight symbols
  :config
  (set-face-attribute 'highlight-thing nil
                      :background "#5e81ac" ; Soft blue from modus-vivendi color scheme
                      :weight 'normal)      ; Keep text weight normal for readability
  :hook (prog-mode . highlight-thing-mode))

;;; indent-bars

(use-package indent-bars
  :ensure t
  :diminish indent-bars-mode
  :hook
  (prog-mode . indent-bars-mode)
  :custom
  ;; Appearance
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-display-on-blank-lines t)
  (indent-bars-prefer-character nil)
  ;; Highlight current level
  (indent-bars-highlight-current-depth '(:blend 0.6))
  ;; Behavior
  (indent-bars-no-descend-strings t)
  (indent-bars-no-descend-lists t)
  (indent-bars-depth-update-delay 0.05)
  ;; Tree-sitter support
  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope
   '((python function_definition class_definition)
     (emacs-lisp defun let progn)
     (c function_definition compound_statement)
     (cpp function_definition compound_statement class_specifier)
     (rust function_item impl_item)
     (javascript function_declaration arrow_function class_declaration)
     (typescript function_declaration arrow_function class_declaration)))
  :config
  ;; Force font-lock refresh
  (defun indent-bars-refresh-font-lock ()
    (when indent-bars-mode
      (font-lock-flush)
      (font-lock-ensure)))
  (add-hook 'indent-bars-mode-hook #'indent-bars-refresh-font-lock))

;;; modeline

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose t))

(use-package
  delight
  :ensure t
  :config
  (delight
   '((global-hl-line-mode nil "hl-line")
     (save-place-mode nil "saveplace")
     (flyspell-mode " ✍" "flyspell")
     (yas-minor-mode nil "yasnippet")
     (smartparens-mode nil "smartparens"))))

;;; smartparens

(use-package
  smartparens
  :ensure t
  :defer t
  :hook
  ((prog-mode . smartparens-mode)
   (text-mode . smartparens-mode)
   (markdown-mode . smartparens-mode))
  :config (require 'smartparens-config)
  :bind
  (:map
   smartparens-mode-map
   ("C-)" . sp-forward-sexp)          ; Jump to next sexp
   ("C-(" . sp-backward-sexp)         ; Jump to prev sexp
   ("C-{" . sp-backward-up-sexp))) ; Up a level

;;; vertico

(use-package vertico
  :ensure t
  :demand t  ; Core completion system - load immediately
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-sort-function 'vertico-sort-history-length-alpha)
  :config
  (with-eval-after-load 'nerd-icons
    (defun my-consult-buffer-format (buffer)
      "Add nerd-icons to BUFFER name for consult-buffer."
      (let ((icon (nerd-icons-icon-for-buffer buffer)))
        (concat icon " " (buffer-name buffer))))
    (advice-add
     'consult-buffer
     :filter-return
     (lambda (buffers) (mapcar #'my-consult-buffer-format buffers))))
  :bind
  (:map
   vertico-map
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   ("s-<tab>" . vertico-next)
   ("S-s-<tab>" . vertico-previous)))

;;; orderless

(use-package orderless
  :ensure t
  :demand t  ; Core completion dependency
  :custom
  ;; Core completion styles - primary configuration
  (completion-styles '(orderless basic partial-completion flex))
  (completion-category-overrides
   '((file (styles basic partial-completion))
     (eglot (styles orderless basic))
     (eglot-capf (styles orderless basic))
     (buffer (styles orderless basic))
     (info-menu (styles orderless basic))
     (consult-multi (styles orderless basic))
     (org-heading (styles orderless basic))
     (unicode-name (styles orderless basic)))))

;;; savehist
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :custom
  ;; history-length set in main emacs config block
  (savehist-file "~/.tmp/savehist")
  (savehist-additional-variables
   '(kill-ring
     mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history
     vertico-repeat-history)))

;;; consult

(use-package consult
  :ensure t
  :defer 2   ; Defer loading to improve startup
  :commands (consult-line consult-buffer consult-grep consult-ripgrep)
  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :config
  ;; history-length set in main emacs config block
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (global-set-key [remap isearch-forward] #'consult-line)
  (global-set-key [remap switch-to-buffer] #'consult-buffer)
  (global-set-key [remap list-buffers] #'consult-buffer)
  (global-set-key [remap yank-pop] #'consult-yank-pop)
  (global-set-key [remap goto-line] #'consult-goto-line)
  (global-set-key [remap imenu] #'consult-imenu)
  (global-set-key [remap bookmark-jump] #'consult-bookmark)
  (defvar my-consult-hidden-buffer-source
    `(:name
      "Hidden Buffers"
      :narrow ?h
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :items
      ,(lambda ()
         (mapcar
          #'buffer-name
          (seq-filter
           (lambda (buf)
             (and (string-match-p "^\\*" (buffer-name buf))
                  (not (get-buffer-window buf 'visible))))
           (buffer-list)))))
    "Source for hidden buffers starting with *.")
  (add-to-list 'consult-buffer-sources 'my-consult-hidden-buffer-source
               t)
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)
   (:map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
   (:map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))))

(use-package consult-yasnippet :ensure t :after (consult yasnippet))

;;; marginalia

(use-package marginalia
  :ensure t
  :defer 1   ; Defer marginalia for faster startup
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (run-with-idle-timer 1 nil #'marginalia-mode))

;;; completion-preview

(use-package completion-preview
  :ensure nil
  :hook ((prog-mode . completion-preview-mode)
         ;; Add tree-sitter modes explicitly
         (python-ts-mode . completion-preview-mode)
         (c-ts-mode . completion-preview-mode)
         (c++-ts-mode . completion-preview-mode)
         (js-ts-mode . completion-preview-mode)
         (typescript-ts-mode . completion-preview-mode)
         (rust-ts-mode . completion-preview-mode))
  :custom
  (completion-preview-minimum-symbol-length 2)
  (completion-preview-idle-delay nil)
  :config
  ;; Make completion-preview work with paredit/smartparens
  (defun my/completion-preview-insert-word ()
    "Insert completion preview and move past any closing delimiters."
    (interactive)
    (when completion-preview--overlay
      (completion-preview-insert)
      ;; Move past closing parens/brackets if needed
      (when (looking-at "[])\\]]")
        (forward-char 1))))

  ;; Fix completion detection with paredit
  (advice-add 'completion-preview--completion-at-point :around
              (lambda (orig-fun &rest args)
                (let ((result (apply orig-fun args)))
                  ;; If no completion found and we're before a closing paren
                  (when (and (not result) (looking-at-p "[})\\]\"]"))
                    ;; Try again with a temporary marker past the paren
                    (save-excursion
                      (forward-char 1)
                      (setq result (apply orig-fun args))))
                  result)))

  (set-face-attribute 'completion-preview nil
                      :foreground "#FFC107"
                      :background "unspecified")

  :bind
  (:map completion-preview-active-mode-map
        ("TAB" . my/completion-preview-insert-word)
        ([tab] . my/completion-preview-insert-word)
        ("M-n" . #'completion-preview-next-candidate)
        ("M-p" . #'completion-preview-prev-candidate)))

;; Core completion settings consolidated - removed duplicate block

;;; nerd-icons

(use-package nerd-icons
  :ensure t
  :defer 3   ; Defer icon loading for faster startup
  :config
  ;; Install fonts if not already present
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))

(use-package
  nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :config (nerd-icons-completion-mode)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package expand-region :ensure t :bind ("C-=" . er/expand-region))

;;; multiple cursors

(use-package
  multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-c >" . mc/mark-next-like-this)
   ("C-c <" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;;; diff-hl

(use-package
  diff-hl
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
  (setq which-key-idle-delay 0.8          ; Slightly longer delay for better UX
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
    "C-c n"   "[Notes] Denote System"
    "C-c w"   "[AI] GPTel Assistant")

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

  ;; GPTel AI Assistant subcommands
  (which-key-add-key-based-replacements
    "C-c w s" "Send to AI"
    "C-c w m" "AI Menu"
    "C-c w a" "Add Context"
    "C-c w f" "Add File"
    "C-c w r" "Rewrite")

  ;; Enhanced navigation and utility keys
  (which-key-add-key-based-replacements
    "C-x u"   "[Undo] Vundo Tree"
    "C-x o"   "[Window] Ace Window"
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

;;; avy

(use-package
  avy
  :ensure t
  :defer t
  :bind (
         ("M-j" . avy-goto-char-timer)
         ("C-c '" . avy-goto-char-2))
  :init (avy-setup-default)
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
    (select-window (cdr (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

;;; ace-window

(use-package ace-window
  :ensure t
  :after avy
  :bind ("C-x o" . ace-window))

;;; flyspell

(use-package
  flyspell
  :ensure nil
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

(use-package
  flyspell-correct
  :ensure t
  :after flyspell
  :bind
  (:map flyspell-mode-map ("M-$" . flyspell-correct-wrapper)))

;;; eglot / lsp

(use-package eglot
  :ensure nil
  :hook
  ((prog-mode
    .
    (lambda ()
      (unless (or (string-match-p "^\\*.*\\*$" (buffer-name))
                  (string= (buffer-file-name)
                           (expand-file-name "init.el" user-emacs-directory)))
        (eglot-ensure))))
   (eglot-managed-mode
    .
    (lambda ()
      (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
      (setq eldoc-documentation-strategy #'eldoc-documentation-default)
      (eglot-inlay-hints-mode))))
  :config

  (setq eglot-events-buffer-size 0) ; Disable event logging for performance
  (setq eglot-sync-connect 1) ; Reduce connection timeout
  (setq eglot-autoshutdown t) ; Shutdown unused servers
  (setq eglot-send-changes-idle-time 0.5) ; Optimize idle time

  ;; Disable python-flymake when eglot is active
  (add-hook
   'eglot-managed-mode-hook
   (lambda ()
     (when (derived-mode-p 'python-mode 'python-ts-mode)
       (remove-hook 'flymake-diagnostic-functions 'python-flymake t))))

  ;; Process cleanup on exit
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (fboundp 'eglot-shutdown-all)
                (eglot-shutdown-all)))))

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
(use-package org
  :ensure nil
  :config
  ;; Basic settings
  (setq org-directory "~/Documents/notes/")
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-return-follows-link t)
  (setq org-log-done 'time)

  ;; TODO states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)"
                    "|" "DONE(d!)" "CANCELED(c@)")))

  ;; Cache variables for org-agenda-files optimization
  (defvar my/org-agenda-files-cache nil)
  (defvar my/org-agenda-cache-time nil)

  ;; Optimized function to update org-agenda-files with caching
  (defun my/update-org-agenda-files ()
    "Update org-agenda-files with caching (5 min cache)."
    (when (or (null my/org-agenda-cache-time)
              (> (- (float-time) my/org-agenda-cache-time) 300)) ; 5 minutes
      (setq my/org-agenda-files-cache
            (directory-files-recursively "~/Documents/notes/" "\\.org$"))
      (setq my/org-agenda-cache-time (float-time)))
    (setq org-agenda-files my/org-agenda-files-cache))

  ;; Initial agenda files setup with cache
  (my/update-org-agenda-files)

  :hook
  ((before-save . my/update-org-agenda-files)
   (org-agenda-mode . my/update-org-agenda-files)))

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
  :config
  ;; Simple agenda views
  (setq org-agenda-custom-commands
        '(("t" "All TODOs" todo "TODO|NEXT|WAITING"
           ((org-agenda-overriding-header "All Open TODOs")))
          ("d" "Today's Agenda" agenda ""
           ((org-agenda-span 'day)
            (org-agenda-overriding-header "Today")))))

  ;; Start week on Monday
  (setq org-agenda-start-on-weekday 1))

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

;;; magit / forge

(use-package magit
  :ensure t
  :bind
  ( :map global-map
    ("C-c g" . magit-status)
    :map magit-mode-map
    ("C-w" . nil)
    ("M-w" . nil))
  :init
  (setq magit-define-global-key-bindings nil)
  (setq magit-section-visibility-indicator '(magit-fringe-bitmap> . magit-fringe-bitmapv))
  :config
  (setq git-commit-summary-max-length 50)
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-diff-refine-hunk t)
  ;;  (with-eval-after-load 'magit
  ;;    (setq magit-format-file-function #'magit-format-file-nerd-icons))
  )

(use-package magit-repos
  :ensure nil ; part of `magit'
  :commands (magit-list-repositories)
  :init
  (setq magit-repository-directories
        '(("~/Code" . 1))))

(use-package forge :ensure t :after magit)
(use-package magit-todos :ensure t :after magit :config (magit-todos-mode 1))

;;; grep settings

(use-package
  grep
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

(use-package
  recentf
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

(use-package
  dired
  :ensure nil
  :bind
  (("C-x C-d" . dired)
   :map
   dired-mode-map
   ("RET" . dired-find-alternate-file)
   ("<backspace>" . dired-up-directory)
   ("C-c C-e" . wdired-change-to-wdired-mode)
   ("C-c C-g" . dired-git-info-mode) ; Changed from C-c g to avoid conflict with magit-status
   ("C-c t" . dired-toggle-read-only)
   ("M-!" . dired-smart-shell-command)
   ("C-c o" . dired-open-externally)
   ("C-c w" . dired-copy-file-path)
   ("C-c f" . dired-consult-filter))
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
  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open")))
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)

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
  (use-package
    nerd-icons-dired
    :ensure t
    :after (nerd-icons dired)
    :hook (dired-mode . nerd-icons-dired-mode))

  (use-package
    dired-git-info
    :ensure t
    :custom
    (dgi-auto-hide-details-p nil)
    :config
    (setq dired-git-info-format " (%s)")
    (define-key dired-mode-map ")" 'dired-git-info-mode))
  (use-package
    dired-subtree
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
  (use-package
    dired-async
    :ensure nil
    :after dired
    :config (dired-async-mode 1)))

;;; eww browser

(use-package eww
  :ensure nil ; built-in package
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
    "Open the current EWW URL in Firefox via EXWM.
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
                 (setq-local scroll-conservatively 100) ; Smooth scrolling
                 (setq-local mouse-wheel-scroll-amount '(1 ((shift) . 1)))))))

;; Optional: Configure eww-lnum for link selection with numbers
(use-package eww-lnum
  :after eww
  :bind
  (:map eww-mode-map
        ("f" . eww-lnum-follow)        ; Follow link by number
        ("F" . eww-lnum-universal)))   ; Universal argument link selection

;; Optional: ace-link integration for quick link selection
(use-package ace-link
  :after eww
  :bind
  (:map eww-mode-map
        ("o" . ace-link-eww)))         ; Jump to any link with ace


;;; pdf-tools

(use-package
  pdf-tools
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

(use-package
  denote
  :ensure t
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

(use-package
  consult-denote
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find) ("C-c n g" . consult-denote-grep))
  :config (consult-denote-mode 1))

(use-package
  denote-journal
  :ensure t
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

(use-package denote-org :ensure t :defer t)

;;; tree-sitter

;; Built-in tree-sitter configuration
(use-package treesit
  :ensure nil
  :init
  ;; Tell Emacs where to look for tree-sitter libraries
  (add-to-list 'treesit-extra-load-path
               (expand-file-name "tree-sitter" user-emacs-directory))

  (setq treesit-language-source-alist
        '((awk . ("https://github.com/Beaglefoot/tree-sitter-awk"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (bibtex . ("https://github.com/latex-lsp/tree-sitter-bibtex"))
          ;;          (blueprint . ("https://github.com/huanie/tree-sitter-blueprint"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (commonlisp . ("https://github.com/theHamsta/tree-sitter-commonlisp"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          ;;          (csharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          ;;          (dart . ("https://github.com/ast-grep/tree-sitter-dart"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          ;;          (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir"))
          ;;          (glsl . ("https://github.com/theHamsta/tree-sitter-glsl"))
          ;;          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          ;;          (heex . ("https://github.com/phoenixframework/tree-sitter-heex"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          ;;          (janet . ("https://github.com/GrayJack/tree-sitter-janet"))
          ;;          (java . ("https://github.com/tree-sitter/tree-sitter-java"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          ;;          (julia . ("https://github.com/tree-sitter/tree-sitter-julia"))
          ;;          (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
          (latex . ("https://github.com/latex-lsp/tree-sitter-latex"))
          (lua . ("https://github.com/MunifTanjim/tree-sitter-lua"))
          ;;          (magik . ("https://github.com/GIT-USERS/RobertCrosas/tree-sitter-magik"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
          ;;          (nix . ("https://github.com/nix-community/tree-sitter-nix"))
          ;;          (nu . ("https://github.com/nushell/tree-sitter-nu"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          ;;          (perl . ("https://github.com/tree-sitter-perl/tree-sitter-perl"))
          ;;          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          ;;          (proto . ("https://github.com/mitchellh/tree-sitter-proto"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          ;;          (r . ("https://github.com/r-lib/tree-sitter-r"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (scala . ("https://github.com/tree-sitter/tree-sitter-scala"))
          (scheme . ("https://github.com/6cdh/tree-sitter-scheme"))
          (sql . ("https://github.com/DerekStride/tree-sitter-sql"))
          ;;          (surface . ("https://github.com/connorlay/tree-sitter-surface"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (typst . ("https://github.com/uben0/tree-sitter-typst"))
          ;;          (verilog . ("https://github.com/tree-sitter/tree-sitter-verilog"))
          ;;          (vhdl . ("https://github.com/gdkrmr/tree-sitter-vhdl"))
          ;;          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          ;;          (wast . ("https://github.com/bytecodealliance/tree-sitter-wast"))
          ;;          (wat . ("https://github.com/bytecodealliance/tree-sitter-wat"))
          ;;          (wgsl . ("https://github.com/mehmetoguzderin/tree-sitter-wgsl"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

  ;; Major mode remapping
  (setq major-mode-remap-alist
        '((awk-mode . awk-ts-mode)
          (bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          (bibtex-mode . bibtex-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          ;;          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          ;;          (elixir-mode . elixir-ts-mode)
          ;;          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (mhtml-mode . html-ts-mode)
          ;;          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (json-mode . json-ts-mode)
          (lua-mode . lua-ts-mode)
          (makefile-mode . make-ts-mode)
          (makefile-gmake-mode . make-ts-mode)
          (markdown-mode . markdown-ts-mode)
          ;;          (php-mode . php-ts-mode)
          (python-mode . python-ts-mode)
          ;;          (r-mode . r-ts-mode)
          ;;          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (scala-mode . scala-ts-mode)
          (sql-mode . sql-ts-mode)
          (toml-mode . toml-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (tsx-mode . tsx-ts-mode)
          (typescript-mode . typescript-ts-mode)
          ;;          (verilog-mode . verilog-ts-mode)
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
    (with-eval-after-load 'rust-ts-mode
      (derived-mode-add-parents 'rust-ts-mode '(rust-mode)))
    (with-eval-after-load 'go-ts-mode
      (derived-mode-add-parents 'go-ts-mode '(go-mode)))
    (with-eval-after-load 'ruby-ts-mode
      (derived-mode-add-parents 'ruby-ts-mode '(ruby-mode)))
    (with-eval-after-load 'yaml-ts-mode
      (derived-mode-add-parents 'yaml-ts-mode '(yaml-mode))))

  ;; Optimized tree-sitter settings for Emacs 30.1
  (setq treesit-font-lock-level 4              ; Maximum highlighting
        treesit-max-buffer-size (* 8 1024 1024) ; 8MB for better performance
        treesit-defun-prefer-top-level t))     ; Better function detection

;; Use treesit-auto for automatic mode selection and fallback
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)   ; Prompt before installing grammars
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; tree-sitter languages

;; Python with tree-sitter
(use-package python
  :ensure nil
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
  :custom
  (js-indent-level 2)
  :hook
  ((js-ts-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure))

(use-package cc-mode
  :ensure nil
  :hook
  ((c-ts-mode c++-ts-mode) . eglot-ensure))

;; Rust with tree-sitter
(use-package rust-ts-mode
  :ensure nil
  :hook
  (rust-ts-mode . eglot-ensure))

;; Go with tree-sitter
(use-package go-ts-mode
  :ensure nil
  :hook
  (go-ts-mode . eglot-ensure))

;; Ruby with tree-sitter
(use-package ruby-ts-mode
  :ensure nil
  :hook
  (ruby-ts-mode . eglot-ensure))

;; ;; Elisp with tree-sitter (if available)
;; (use-package elisp-ts-mode
;;   :ensure nil
;;   :if (treesit-language-available-p 'elisp)
;;   :hook
;;   (elisp-ts-mode . (lambda ()
;;                      (setq-local treesit-defun-type-regexp
;;                                  (rx (or "function_definition"
;;                                          "macro_definition")))
;;                      (setq-local treesit-thing-settings
;;                                  `((elisp
;;                                     (defun . ,(rx (or "function_definition"
;;                                                       "macro_definition")))
;;                                     (sexp . "sexp")
;;                                     (sentence . "sexp"))))
;;                      (setq-local treesit-primary-parser (treesit-parser-create 'elisp)))))


;;; tree-sitter folding

;; Use built-in outline-minor-mode for code folding with tree-sitter
(use-package outline
  :ensure nil
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

;; Hideshow as alternative folding method (works well with tree-sitter)
(use-package hideshow
  :ensure nil
  :hook
  (prog-mode . hs-minor-mode))
;; Built-in hideshow keybindings:
;; C-c @ C-h - hide block
;; C-c @ C-s - show block
;; C-c @ ESC C-h - hide all
;; C-c @ ESC C-s - show all
;; C-c @ C-l - hide level
;; C-c @ C-c - toggle hiding

;;; so-long

(use-package so-long :ensure nil :config (global-so-long-mode 1))

;;; flymake

(use-package flymake
  :ensure nil
  :hook
  ((prog-mode . flymake-mode)
   (emacs-lisp-mode
    .
    (lambda ()
      ;; Only enable flymake-mode for file-backed buffers
      (when (buffer-file-name)
        (flymake-mode 1))))
   ;; Add tree-sitter modes
   (python-ts-mode . flymake-mode)
   (c-ts-mode . flymake-mode)
   (c++-ts-mode . flymake-mode)
   (js-ts-mode . flymake-mode)
   (typescript-ts-mode . flymake-mode)
   (rust-ts-mode . flymake-mode)
   (after-init-hook
    .
    (lambda ()
      (with-current-buffer "*scratch*"
        (flymake-mode -1)))))
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-no-changes-timeout 1)
  (add-hook 'lisp-interaction-mode-hook
            (lambda ()
              (when (string= (buffer-name) "*scratch*")
                (flymake-mode -1))))
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     ;; Only add diagnostic functions for file-backed buffers
     (when (buffer-file-name)
       (add-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile nil t)
       (add-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc nil t))))
  :bind
  (:map flymake-mode-map
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)))

(use-package
  elisp-lint
  :ensure t
  :commands (elisp-lint-buffer elisp-lint-file)
 ;;;; KEYBIND_CHANGE: C-c l conflicts with consult-lsp, using standard M-x
  :config (setq elisp-lint-ignored-validators '("package-lint")))

;;; YAsnippet

(use-package
  yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :custom
  (yas-snippet-dirs
   (list
    (expand-file-name "snippets/" user-emacs-directory)
    (expand-file-name
     "snippets/"
     (file-name-directory (find-library-name "yasnippet-snippets")))))
  (yas-prompt-functions
   '(yas-completing-prompt yas-ido-prompt yas-no-prompt))
  :hook (after-init . yas-reload-all)
  :bind (:map yas-minor-mode-map ("C-c y" . yas-insert-snippet)))

(use-package
  yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (let ((snippets-dir
         (expand-file-name "snippets/"
                           (file-name-directory
                            (find-library-name
                             "yasnippet-snippets")))))
    (unless (file-directory-p snippets-dir)
      (warn
       "yasnippet-snippets directory %s not found; reinstall the package"
       snippets-dir))))

;;; helpful

(use-package
  helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key))

;;; elisp-demos

(use-package
  elisp-demos
  :ensure t
  :config
  (advice-add
   'helpful-update
   :after #'elisp-demos-advice-helpful-update))

;;; 0x0

(use-package
  0x0
  :ensure t
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

(use-package
  eshell
  :ensure nil ;; Built-in package
  :commands (eshell eshell-command)
  :bind
  (
   :map
   eshell-mode-map
   ("C-l" . eshell/clear)
   ("C-r" . eshell-history-backward)
   ("C-s" . eshell-history-forward)
   ("M-." . eshell-find-file-at-point))
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
  (use-package
    eshell-syntax-highlighting
    :ensure t
    :after eshell
    :config (eshell-syntax-highlighting-global-mode +1))

  ;; Enable eshell-git-prompt for advanced Git-aware prompts
  (use-package
    eshell-git-prompt
    :ensure t
    :after eshell
    :config
    (eshell-git-prompt-use-theme 'powerline)) ;; Use powerline theme

  :bind (("C-c `" . 'eshell))

  :hook
  ((eshell-mode . my-eshell-disable-distractions) ;; Disable distractions
   (eshell-mode . my-eshell-setup-aliases) ;; Setup aliases
   (eshell-pre-output-filter . my-eshell-truncate-buffer) ;; Truncate buffer
   (eshell-visual-subprocess-hook . my-eshell-disable-distractions)
   (eshell-mode . eat-eshell-visual-command-mode))) ;; Subprocess distractions

;; E MA I L EMAIL

(use-package
  message
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
;;
;; Explanation: While early-init.el sets frame parameters (menu-bar-lines: 0, tool-bar-lines: 0, etc.)
;; to disable UI elements at the frame level, explicit mode disabling is still needed because:
;; 1. Some edge cases where frame parameters don't fully disable the modes
;; 2. Daemon mode creates frames after early-init.el has run
;; 3. Ensures consistent behavior across different Emacs startup scenarios
;;
;; This dual approach (frame parameters + mode disabling) provides robust UI element management.

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

;; (use-package
;;   emacs
;;   :ensure nil
;;   :config
;;   (let ((templates-dir "~/.config/emacs/latex/templates/"))
;;     (when (file-exists-p templates-dir)
;;       (dolist (file
;;                (directory-files-recursively templates-dir "\\.el$"))
;;         (load-file file)))))

;;; hl-line

(use-package
  hl-line
  :ensure nil ; Built-in, no need to install
  :commands (hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50)
  :hook
  (prog-mode . hl-line-mode)
  (occur-mode . hl-line-mode))

;;; slime

(use-package
  slime
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
   (inferior-lisp-program "sbcl")      ;; Path to SBCL
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

(use-package
  auth-source-xoauth2-plugin
  :ensure t
  :custom (auth-source-xoauth2-plugin-mode t))

;;; .mailcap

(require 'mailcap)
(mailcap-parse-mailcaps)

;;; EAT Terminal

(use-package eat
  :ensure t ;; Automatically install eat from NonGNU ELPA
  :init
  ;; Preload Eat so everything is ready on first use
  (require 'eat)

  ;; Set EAT_SHELL_INTEGRATION_DIR early
  (let ((eat-dir (file-name-directory (locate-library "eat"))))
    (when eat-dir
      (setenv "EAT_SHELL_INTEGRATION_DIR" eat-dir)))

  ;; Optionally ensure integration file is sourced in ~/.bashrc
  (let* ((eat-dir (file-name-directory (locate-library "eat")))
         (integration-file (and eat-dir (expand-file-name "integration/bash" eat-dir)))
         (bashrc (expand-file-name "~/.bashrc")))
    (when (and integration-file (file-exists-p integration-file) (file-exists-p bashrc))
      (unless (with-temp-buffer
                (insert-file-contents bashrc)
                (re-search-forward "EAT_SHELL_INTEGRATION_DIR/bash\\>" nil t))
        (with-temp-buffer
          (insert "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && \\\n")
          (insert "  source \"$EAT_SHELL_INTEGRATION_DIR/bash\"\n")
          (append-to-file (point-min) (point-max) bashrc)))))

  :custom
  (eat-shell (or (getenv "SHELL") "/sbin/bash"))
  (eat-kill-buffer-on-exit t)
  (eat-enable-blinking-text t)
  (eat-enable-mouse t)
  (eat-semi-char-non-bound-keys
   '([?\C-x] [?\C-c] [?\C-g] [?\C-h] [?\C-u] [?\M-x] [?\M-:] [?\M-&] [?\C-\M-c]))
  (eat-eshell-semi-char-non-bound-keys
   '([?\C-x] [?\C-c] [?\C-g] [?\C-h] [?\C-u] [?\M-x] [?\M-:] [?\M-&] [?\C-\M-c]))
  (eat-enable-shell-prompt-annotation t)
  (eat-term-scrollback-size 100000)
  (eat-term-resize t)

  :hook
  ((eshell-load-hook . eat-eshell-mode)
   (eshell-load-hook . eat-eshell-visual-command-mode)
   (eat-mode-hook . (lambda ()
                      (eat-update-semi-char-mode-map)
                      (eat-eshell-update-semi-char-mode-map)))
   (eat-mode-hook . eat-semi-char-mode))

  :delight
  (eat-eshell-mode nil)
  (eat-eshell-visual-command-mode nil))

;;; gptel

(use-package gptel
  :ensure t
  :bind
  (
   ("C-c w s" . gptel-send)
   ("C-c w m" . gptel-menu)
   ("C-c w a" . gptel-add)
   ("C-c w f" . gptel-add-file)
   ("C-c w r" . gptel-rewrite)
   )
  :config
  ;; Configure Anthropic (Claude) backend
  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(claude-3-7-sonnet-20250219))

  ;; Optional: Configure Claude with thinking mode
  (gptel-make-anthropic "Claude-thinking"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(claude-3-7-sonnet-20250219)
    :header (lambda ()
              (when-let* ((key (gptel--get-api-key)))
                `(("x-api-key" . ,key)
                  ("anthropic-version" . "2023-06-01")
                  ("anthropic-beta" . "pdfs-2024-09-25")
                  ("anthropic-beta" . "output-128k-2025-02-19")
                  ("anthropic-beta" . "prompt-caching-2024-07-31"))))
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                                :max_tokens 4096))
  (setq gptel-api-key-from-auth-source t))

;;; EditorConfig support (built-in since Emacs 30.1)

(use-package editorconfig
  :ensure nil  ; Built-in since Emacs 30.1
  :demand t    ; Load immediately
  :config
  ;; Enable EditorConfig support globally
  (editorconfig-mode 1)

  ;; Customize which properties to respect
  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode) ; Use ws-butler if available

  ;; Handle additional EditorConfig properties
  (add-to-list 'editorconfig-indentation-alist
               '(typescript-ts-mode typescript-indent-level))
  (add-to-list 'editorconfig-indentation-alist
               '(js-ts-mode js-indent-level))

  ;; Exclude certain modes from EditorConfig
  (setq editorconfig-exclude-modes
        '(lisp-interaction-mode
          help-mode
          magit-mode
          magit-diff-mode))

  (message "EditorConfig support enabled (built-in Emacs 30.1)"))

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
  :commands (man)
  :config
  (setq Man-notify-method 'pushy))

;;; `proced' (process monitor, similar to `top')

(use-package proced
  :ensure nil
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
  :demand t
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
  ;; Consolidated completion behavior settings
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
        minibuffer-visible-completions nil))

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

  ;; Support for OS-specific escape sequences such as what `ls
  ;; --hyperlink' uses.  I normally don't use those, but I am checking
  ;; this to see if there are any obvious advantages/disadvantages.
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output))

;;; dictionary / definitions

(use-package
  dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org")
  :bind
  ("<f6>" . dictionary-lookup-definition))

;;; log file handling

(use-package logview
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
  (add-to-list 'auto-mode-alist '("log\\'" . logview-mode))
  :hook (
         ('log-mode 'logview-mode)))

;;; pulsar

(use-package pulsar
  :ensure t
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.1)
  (pulsar-iterations 15)
  (pulsar-face 'isearch)
  (pulsar-highlight-face 'pulsar-yellow)
  :hook
  ((minibuffer-setup . pulsar-pulse-line)
   (consult-after-jump . pulsar-recenter-middle)
   (consult-after-jump . pulsar-reveal-entry))
  :config
  (pulsar-global-mode 1))

;;; volatile highlighting

(use-package
  volatile-highlights
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

;; (use-package buffer-background
;;   :vc (:url "https://github.com/theesfeld/buffer-background" :branch "opacity")
;;   :defer 1
;;   :ensure t
;;   :custom
;;   ;; Set global defaults
;;   (buffer-background-opacity 0.3)
;;   (buffer-background-auto-enable t)
;;   :config
;;   (setq buffer-background-color-alist
;;         '(;; === EXACT BUFFER NAME MATCHING ===
;;           ("*scratch*" . (:color "#2d2d2d" :opacity 0.8))
;;           ("*Messages*" . "#1a1a1a")
;;           ("*Warnings*" . (:color "#3d1a1a" :opacity 0.9))

;;           ;; === REGEXP PATTERN MATCHING ===
;;           ("\\*Help.*\\*" . (:color "#1e1e2e" :opacity 0.85))
;;           ("\\*Compile.*\\*" . (:color "#2d2d2d" :opacity 0.9))
;;           ("\\*.*shell.*\\*" . (:color "#1a1a2d" :opacity 0.8))

;;           ;; === MAJOR MODE ASSIGNMENTS ===
;;           (org-mode . (:color "#002b36" :opacity 0.8))        ; Solarized dark
;;           (python-mode . (:color "#1a1a2d" :opacity 0.8))     ; Blue tint
;;           (emacs-lisp-mode . (:color "#2d1a2d" :opacity 0.8)) ; Purple tint
;;           (c-mode . (:color "#1a1a1a" :opacity 0.85))

;;           ;; === ALTERNATIVE MODE SYNTAX ===
;;           ((mode . js-mode) . (:color "#2d2d1a" :opacity 0.75))      ; Yellow tint
;;           ((mode . typescript-mode) . (:color "#1a2d2d" :opacity 0.75)) ; Cyan tint
;;           ((mode . css-mode) . (:color "#1a2b3c" :opacity 0.75))

;;           ;; === FILE EXTENSION MATCHING ===
;;           ((file . "md") . (:color "#f8f8f2" :opacity 0.05))  ; Light for readability
;;           ((file . "txt") . (:color "#1c1c1c" :opacity 0.7))
;;           ((file . "json") . (:color "#1a1a1a" :opacity 0.7))
;;           ((file . "yaml") . (:color "#2a2a1a" :opacity 0.7))

;;           ;; === CUSTOM PREDICATE MATCHING ===
;;           ;; Remote files (TRAMP)
;;           ((lambda (buf)
;;              (file-remote-p default-directory))
;;            . (:color "#1a1a3d" :opacity 0.8))

;;           ;; All programming modes
;;           ((lambda (buf)
;;              (with-current-buffer buf
;;                (derived-mode-p 'prog-mode)))
;;            . (:color "#1a1a1a" :opacity 0.6))

;;           ;; Test files
;;           ((lambda (buf)
;;              (string-match-p "\\(test\\|spec\\)" (buffer-name buf)))
;;            . (:color "#0a2a0a" :opacity 0.85))   ; Dark green

;;           ;; Dired buffers
;;           ((lambda (buf)
;;              (with-current-buffer buf
;;                (derived-mode-p 'dired-mode)))
;;            . (:color "#2a2a2a" :opacity 0.7))))

;;   ;; Enable global mode for automatic buffer assignment
;;   (buffer-background-global-mode 1))

;; (use-package buffer-background
;;   :ensure t
;;   :defer 1
;;   :vc (:url "https://github.com/theesfeld/buffer-background" :branch "opacity")
;;   :hook (after-init . (lambda ()
;;                         (setq buffer-background-color-alist
;;                               '(("*scratch*" . "#2d2d2d")
;;                                 ("*Messages*" . "#7fdb22")
;;                                 (org-mode . (:color "#1e1e2e" :opacity 0.9))))
;;                         (buffer-background-global-mode 1))))

;;; STARTUP INFORMATION

(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs started in %.2f seconds with %d garbage collections"
                       (float-time (time-subtract after-init-time before-init-time))
                       gcs-done))))

;;; final cleanup

(put 'eshell 'disabled nil)
(provide 'init)

;;; init.el ends here
