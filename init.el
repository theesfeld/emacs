;;; init.el -*- lexical-binding: t -*-

;; Time-stamp: <Last changed 2025-06-23 08:04:16 by grim>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Early Initial Settings                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; imenu support
(setq use-package-enable-imenu-support t)

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

(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      (format ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
              'lisp-interaction-mode
              (propertize
               (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
               'face 'help-key-binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  MELPA                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CUSTOM FUNCTIONS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              (not (eq mode 'all-the-icons-completion-mode))) ; Exclude problematic mode
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
        (consult-yasnippet all-snippets)
      (message
       "No snippets available for current major/minor modes"))))

(global-set-key (kbd "C-c Y") #'my/consult-yasnippet-with-minor-modes)

;; Function to translate C-M-S-s-<key> to H-<key>
(defun my-hyper-translate (key)
  "Translate C-M-S-s-<key> to H-<key>, handling any key type."
  (let*
      ((base-key
        (if (and (integerp key) (>= key ?A) (<= key ?Z)) ;; If uppercase letter
            (downcase key) ;; Strip Shift
          key)) ;; Pass symbols/other keys as-is
       (key-str
        (cond
         ((eq base-key ?+)
          "+") ;; Explicit symbol mappings
         ((eq base-key ?-)
          "-")
         ((eq base-key ?=)
          "=")
         ((eq base-key ?_)
          "_")
         (t
          (char-to-string base-key))))) ;; Default to char
    (kbd (concat "H-" key-str))))

;; Map C-M-S-s-<key> for letters and symbols
(let ((keys
       (append
        (number-sequence ?a ?z) ;; a-z
        '(?= ?- ?+ ?_)))) ;; =, -, +, _
  (dolist (char keys)
    (define-key
     key-translation-map
     (kbd (concat "C-M-S-s-" (char-to-string char)))
     `(lambda (&optional _event)
        (interactive)
        (my-hyper-translate ,char)))))

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
    (while (re-search-forward "\\[\\[\\(http[^][]+\\)\\]\\[.*\\]\\]"
                              (org-download-image (match-string 1)))))
  (add-hook
   'org-capture-after-finalize-hook
   #'my-org-download-images-from-capture))

(defun my-get-volume ()
  "Return the current volume percentage using pactl."
  (let
      ((output
        (shell-command-to-string
         "pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n1")))
    (string-to-number (string-trim output "%"))))

;; Helper function to get current brightness from brightnessctl
(defun my-get-brightness ()
  "Return the current brightness percentage using brightnessctl."
  (let ((output (shell-command-to-string "brightnessctl get")))
    (max (string-to-number
          (shell-command-to-string "brightnessctl max"))))
  (round (* 100.0 (/ (string-to-number (string-trim output)) max))))

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

(defun my-erc-update-notifications-keywords (&rest _)
  "Update notification keywords with current nick."
  (when erc-session-user
    (setq erc-notifications-keywords (list erc-session-user))))

(defun my-erc-set-fill-column ()
  "Set ERC fill column based on display type."
  (setq-local erc-fill-column
              (if (display-graphic-p)
                  (window-width)
                (min 80 (window-width)))))

(defun my-org-capture-delete-file-after-kill (&rest _)
  "Delete file if capture is aborted."
  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (delete-file (buffer-file-name))
    (message "Deleted aborted capture file: %s" (buffer-file-name))))

(declare-function pcomplete-erc-setup "erc-pcomplete")
(declare-function completion-preview-insert "completion-preview")
(declare-function completion-preview-next-candidate
                  "completion-preview")
(declare-function completion-preview-prev-candidate
                  "completion-preview")
(declare-function completion-preview--hide "completion-preview")

;;;;; EDNC NOTIFICATIONS (DBUS)

(use-package ednc
  :ensure t
  :config
  (defun stack-notifications (&optional hide)
    (mapconcat (lambda (notification)
                 (let ((app-name (ednc-notification-app-name notification)))
                   (unless (member app-name hide)
                     (push app-name hide)
                     (ednc-format-notification notification))))
               (ednc-notifications) ""))
  (nconc global-mode-string '((:eval (stack-notifications))))
  (add-hook 'ednc-notification-presentation-functions
            (lambda (&rest _) (force-mode-line-update t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     EXWM                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (run-at-time 2 nil #'grim/run-in-background "pasystray --notify=all")
    (run-at-time 9 nil #'grim/run-in-background "mullvad-vpn --disable-gpu"))

  (defun grim/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))

  (defun grim/exwm-update-title ()
    (pcase exwm-class-name
      ("Firefox" (exwm-workspace-rename-buffer
                  (format "Firefox: %s" exwm-title)))))

  ;; fix floating window size if possible
  ;; https://dindi.garjola.net/exwm-floating.html
  (defun my/adjust-floating-frame-size ()
    "Ensure that the current floating exwm frame does not exceed the size of the screen"
    (when (and (boundp 'exwm-workspace--workareas)
               exwm-workspace--workareas
               (> (length exwm-workspace--workareas) 0))
      (condition-case err
          (let* ((frame (selected-frame))
                 (width (frame-pixel-width frame))
                 (height (frame-pixel-height frame))
                 (workarea-0 (elt exwm-workspace--workareas 0))
                 (workarea-1
                  (when (> (length exwm-workspace--workareas) 1)
                    (elt exwm-workspace--workareas 1)))
                 (w1
                  (when workarea-0
                    (elt workarea-0 2)))
                 (h1
                  (when workarea-0
                    (elt workarea-0 3)))
                 (w2
                  (when workarea-1
                    (elt workarea-1 2)))
                 (h2
                  (when workarea-1
                    (elt workarea-1 3)))
                 (max-width
                  (round
                   (* 0.75
                      (if (and w1 w2)
                          (min w1 w2)
                        (or w1 1920)))))
                 (max-height
                  (round
                   (* 0.75
                      (if (and h1 h2)
                          (min h1 h2)
                        (or h1 1080)))))
                 (final-height (min height max-height))
                 (final-width (min width max-width)))
            (when (and (> final-width 0) (> final-height 0))
              (set-frame-size frame final-width final-height t)))
        (error
         (message "Error adjusting floating frame size: %s"
                  (error-message-string err))))))

  (use-package
    exwm
    :ensure t
    :config (setq exwm-workspace-number 5)

    (add-hook 'exwm-update-class-hook #'grim/exwm-update-class)
    (add-hook 'exwm-update-title-hook #'grim/exwm-update-title)
    (add-hook 'exwm-init-hook #'grim/exwm-init-hook)
    (add-hook 'exwm-floating-setup-hook #'my/adjust-floating-frame-size
              100)

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

    (setenv "GDK_SCALE" "1")
    (setenv "QT_AUTO_SCREEN_SCALE_FACTOR" "1")
    (setenv "QT_ENABLE_HIGHDPI_SCALING" "1")

    (require 'exwm-randr)
    (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1"))
    (add-hook
     'exwm-randr-screen-change-hook
     (lambda ()
       (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
             connected-outputs)
         (with-temp-buffer
           (call-process "xrandr" nil t nil)
           (goto-char (point-min))
           (while (re-search-forward xrandr-output-regexp nil t)
             (push (match-string 1) connected-outputs)))
         (cond
          ;; Single monitor or only eDP-1 connected
          ((or (= (length connected-outputs) 1)
               (and (member "eDP-1" connected-outputs)
                    (= (length (remove "eDP-1" connected-outputs)) 0)))
           (start-process-shell-command
            "xrandr"
            nil
            "xrandr --output eDP-1 --primary --auto --scale .75 --dpi 192")
           (start-process-shell-command
            "xrdb" nil "echo 'Xft.dpi: 96' | xrdb -merge")
           (dolist (output (remove "eDP-1" connected-outputs))
             (start-process-shell-command
              "xrandr" nil
              (format "xrandr --output %s --off" output)))
           (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1")))
          ;; One or more external monitors
          ((>= (length (remove "eDP-1" connected-outputs)) 1)
           (let ((primary (car (remove "eDP-1" connected-outputs)))
                 (secondary (cadr (remove "eDP-1" connected-outputs))))
             (if secondary
                 (start-process-shell-command
                  "xrandr" nil
                  (format
                   "xrandr --output %s --primary --auto --output %s --auto --left-of %s --output eDP-1 --off"
                   primary secondary primary))
               (start-process-shell-command
                "xrandr" nil
                (format
                 "xrandr --output %s --primary --auto --output eDP-1 --off"
                 primary)))
             ;; Reset DPI to default (96) for external monitors
             (start-process-shell-command
              "xrdb" nil "echo 'Xft.dpi: 96' | xrdb -merge")
             (setq exwm-randr-workspace-monitor-plist
                   (if secondary
                       `(0 ,primary 1 ,secondary)
                     `(0 ,primary)))))))))

    (exwm-randr-mode 1)
    ;; Load the system tray before exwm-init
    (require 'exwm-systemtray)
    (setq exwm-systemtray-height 24)
    (setq exwm-systemtray-icon-gap 10)
    (exwm-systemtray-mode 1)

    ;; Input Prefix Keys
    (setq exwm-input-prefix-keys
          '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-& ?\M-: ?\C-\M-j ?\C-\ ))

    ;; xss-lock setup for autolock and suspend locking
    (when (and (executable-find "xss-lock") (executable-find "slock"))
      (start-process-shell-command "xss-lock" nil "xss-lock -- slock"))
    (when (executable-find "xset")
      (start-process-shell-command "xset" nil "xset s 300")) ; 5-minute inactivity

    ;; Global keybindings
    (setq exwm-input-global-keys
          (nconc
           `(([?\s-r] . exwm-reset)
             ([s-left] . windmove-left)
             ([s-right] . windmove-right)
             ([s-up] . windmove-up)
             ([s-down] . windmove-down)
             ([?\s-w] . exwm-workspace-switch)
             ([?\s-&]
              .
              (lambda (cmd)
                (interactive (list (read-shell-command "$ ")))
                (start-process-shell-command cmd nil cmd)))
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

    (exwm-enable))

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
                                        ;(setq exwm-edit-split 'below) ; Open edit buffer below current window
    :bind (:map exwm-mode-map ("C-c e" . exwm-edit--compose))
    :hook
    ;; Log initialization
    (exwm-init
     .
     (lambda ()
       (when (featurep 'exwm-edit)
         (message "exwm-edit initialized")))))

  (use-package
    exwm-firefox-core
    :ensure t
    :after exwm
    :init
    ;; Pre-load settings
    (setq exwm-firefox-core-classes
          '("firefox" "firefoxdeveloperedition"))
    :config
    ;; Load package safely
    (require 'exwm-firefox-core nil t)
    (when (featurep 'exwm-firefox-core)
      (message "exwm-firefox-core loaded"))
    ;; Popular keybindings
    :bind
    (:map
     exwm-mode-map
     ("C-c F n" . exwm-firefox-core-tab-new)
     ("C-c F t" . exwm-firefox-core-tab-close)
     ("C-c F <right>" . exwm-firefox-core-tab-right)
     ("C-c F <left>" . exwm-firefox-core-tab-left)
     ("C-c F h" . exwm-firefox-core-back)
     ("C-c F l" . exwm-firefox-core-forward))
    :hook
    ;; Rename buffers for Firefox windows
    (exwm-update-title
     .
     (lambda ()
       (when (string-match-p "firefox" (downcase exwm-class-name))
         (exwm-workspace-rename-buffer exwm-title)))))

  (use-package
    desktop-environment
    :ensure t
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
    (setq desktop-environment-volume-normal-increment "+5") ; Volume step up
    (setq desktop-environment-volume-normal-decrement "-5") ; Volume step down

    :config
    ;; Ensure dependencies are installed
    (desktop-environment-mode 1)
    (dolist (cmd '("scrot" "slock" "pactl" "brightnessctl"))
      (unless (executable-find cmd)
        (message
         "Warning: %s not found; desktop-environment may not work fully"
         cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Version Control for Config                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     EMACS                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  emacs
  :ensure nil ; Built-in, no need to install
  :init (server-start)
  ;; Define a variable for the temporary directory
  (defvar my-tmp-dir (expand-file-name "~/.tmp/")
    "Directory for temporary files, backups, and history files.")

  ;; Create the temporary directory if it doesn't exist
  (unless (file-exists-p my-tmp-dir)
    (make-directory my-tmp-dir t))

  ;; Create subdirectories for different types of files
  (dolist (dir
           '("backups"
             "auto-saves"
             "auto-save-list"
             "recentf"
             "eshell"
             "tramp-auto-save"
             "saveplace"
             "undos"))
    (let ((subdir (expand-file-name dir my-tmp-dir)))
      (unless (file-exists-p subdir)
        (make-directory subdir t))))

  ;; Moved from Early Initial Settings
  (setq custom-file
        (expand-file-name "custom.el" user-emacs-directory))
  ;; Basic Emacs Information and pre-load settings
  (setq
   user-full-name "TJ"
   user-mail-address "william@theesfeld.net"
   calendar-location-name "New York, NY"
   calendar-time-zone-rule "EST"
   calendar-standard-time-zone-name "EST"
   calendar-daylight-time-zone-name "EDT"
   auth-sources '("~/.authinfo.gpg")
   epg-pinentry-mode 'loopback
   create-lockfiles nil
   password-cache-expiry nil
   ;; this
   delete-pair-blink-delay 0.1
   next-error-recenter '(4)
   find-library-include-other-files nil
   remote-file-name-inhibit-delete-by-moving-to-trash t
   remote-file-name-inhibit-auto-save t
   save-interprogram-paste-before-kill t
   eval-expression-print-length nil
   scroll-error-top-bottom t
   echo-keystrokes-help nil
   delete-section-mode t
   x-stretch-cursor t
   help-window-select t
   auth-source-cache-expiry nil
   gc-cons-percentage 0.6
   truncate-string-ellipsis "…" ; Visual ellipsis for truncated lines
   scroll-margin 1
   garbage-collection-messages nil
   plstore-cache-directory my-tmp-dir
   epg-gpg-program "gpg2"
   gc-cons-threshold most-positive-fixnum ; From Garbage Collection

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

  ;; Ensure temp directories exist
  (dolist (dir
           '("backups"
             "auto-saves"
             "auto-save-list"
             "recentf"
             "eshell"
             "tramp-auto-save"
             "saveplace"
             "undos"))
    (let ((path (expand-file-name dir my-tmp-dir)))
      (unless (file-exists-p path)
        (make-directory path t))))

  ;; Ensure savehist file exists and set up history variables
  (let ((savehist-path (expand-file-name "savehist-file" my-tmp-dir)))
    (unless (file-exists-p savehist-path)
      (with-temp-file savehist-path
        (insert ""))))
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring
          extended-command-history
          file-name-history))

  (setenv "TZ" "America/New_York")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (save-place-mode 1)
  (savehist-mode 1) ; Ensure history persistence is enabled
  (setq savehist-file (expand-file-name "savehist-file" my-tmp-dir))
  (setq history-length 1000) ; Consistent with consult
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

  ;; Define a function to generate the time-stamp line with appropriate comment syntax
  (defun my/insert-time-stamp ()
    "Insert a time-stamp line with the comment syntax of the current major mode."
    (let ((comment-start (or comment-start "#")))
      (insert comment-start " Time-stamp: <>\n")
      ;; Ensure time-stamp can find the line (must be within first 8 lines)
      (when (> (line-number-at-pos) 10)
        (message
         "Warning: Time-stamp inserted beyond line 10 may not update"))))

  ;; Define auto-insert templates based on major modes
  (setq auto-insert-alist
        '(
          ;; For programming modes (derived from prog-mode)
          (prog-mode . [my/insert-time-stamp])
          ;; For org-mode
          (org-mode . [my/insert-time-stamp])
          ;; For text-mode
          (text-mode . [my/insert-time-stamp])
          ;; Default for other modes
          (t . [my/insert-time-stamp])))

  ;; Enable time-stamp updates on save
  (add-hook 'before-save-hook 'time-stamp)

  :config
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
   display-time-load-average nil
   savehist-file (expand-file-name "savehist" my-tmp-dir)
   history-length 1000
   history-delete-duplicates t
   savehist-save-minibuffer-history t
   undo-limit 800000
   isearch-lazy-count t
   lazy-count-prefix-format "(%s/%s) "
   lazy-count-suffix-format nil
   save-place-file (expand-file-name "saveplace/saveplace" my-tmp-dir))
  (fset 'yes-or-no-p 'y-or-n-p)
  (require 'auth-source)
  (require 'epa-file)
  (epa-file-enable)

  ;; UI Settings
  (set-face-attribute 'default nil :height 120)

  (set-face-attribute 'variable-pitch nil :height 130)
  (load-theme 'modus-vivendi t)
  (custom-set-faces
   '(cursor ((t (:background "#FFC107")))))
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)

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

  :hook
  ((text-mode . visual-wrap-prefix-mode)
   (before-save . whitespace-cleanup)
   (prog-mode . display-line-numbers-mode)
   (prog-mode . which-function-mode)
   (emacs-lisp-mode . display-line-numbers-mode)
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
   ("C-x K" . kill-buffer)
   ( "C-x C-i" . imenu)
   ("s-<tab>" . previous-buffer)
   ("C-x C-;" . comment-or-uncomment-region)
   ("C-x C-'" . mc/mark-all-like-this)
   ("C-x f" . find-file-at-point)))

(use-package
 windmove
 :ensure nil
 :config
 (windmove-default-keybindings 'meta)) ; Use M-<arrow> instead

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Shell Environment                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  exec-path-from-shell
  :ensure t
  :config
  (defun my-load-env-file ()
    "Load environment variables from ~/.config/emacs/.env into Emacs."
    (let ((env-file (expand-file-name ".env" user-emacs-directory)))
      (when (file-readable-p env-file)
        (with-temp-buffer
          (insert-file-contents env-file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line
                   (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))))
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
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize) ;; Run unconditionally
  ;; Explicitly add ~/.local/bin to exec-path and PATH
  (let ((local-bin (expand-file-name "~/.local/bin")))
    (unless (member local-bin exec-path)
      (add-to-list 'exec-path local-bin t) ;; Add to end of exec-path
      (setenv "PATH" (concat local-bin ":" (getenv "PATH"))) ;; Add to PATH
      (message "Added %s to exec-path and PATH" local-bin)))
  (message "exec-path-from-shell ran with shell: %s"
           exec-path-from-shell-shell-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ediff                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

 ;; Transient menu for ediff commands
 (require 'transient)
 (transient-define-prefix
  my-ediff-dispatch () "Ediff command menu."
  [["Compare"
    ("f" "Files" my-ediff-files)
    ("b" "Buffers" my-ediff-buffers)
    ("d" "Directories" ediff-directories)]
   ["Actions" ("q" "Quit and Restore" my-ediff-quit)]])

 ;; Interactive file selection with read-file-name
 (defun my-ediff-files ()
   "Compare two files selected manually."
   (interactive)
   (let ((file-a (read-file-name "File A: "))
         (file-b (read-file-name "File B: ")))
     (when (and file-a file-b)
       (my-ediff-save-window-config)
       (ediff-files file-a file-b))))

 ;; Interactive buffer selection
 (defun my-ediff-buffers ()
   "Compare two buffers with completion."
   (interactive)
   (let* ((buffer-a
           (completing-read
            "Buffer A: " (mapcar #'buffer-name (buffer-list))))
          (buffer-b
           (completing-read
            "Buffer B: " (mapcar #'buffer-name (buffer-list)))))
     (when (and buffer-a buffer-b)
       (my-ediff-save-window-config)
       (ediff-buffers buffer-a buffer-b))))

 ;; Save window configuration
 (defvar my-ediff-window-config nil
   "Store window configuration before ediff.")
 (defun my-ediff-save-window-config ()
   "Save window configuration before ediff."
   (setq my-ediff-window-config (current-window-configuration)))

 ;; Quit and restore
 (defun my-ediff-quit ()
   "Quit ediff, discard changes, kill buffers, and restore window configuration."
   (interactive)
   (when (and (boundp 'ediff-control-buffer) ediff-control-buffer)
     (with-current-buffer ediff-control-buffer
       (ediff-quit t))
     (when my-ediff-window-config
       (set-window-configuration my-ediff-window-config)
       (setq my-ediff-window-config nil))))

 ;; Define keybindings after ediff is loaded
 (with-eval-after-load 'ediff-mode
   (require 'ediff-mode)
   (define-key ediff-mode-map (kbd "C-c q") #'my-ediff-quit)
   (define-key ediff-mode-map (kbd "?") #'ediff-toggle-help))

 :bind (("C-c d" . my-ediff-dispatch)))

(with-eval-after-load 'ediff-wind
  (setq ediff-control-frame-parameters
    (cons '(unsplittable . t) ediff-control-frame-parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    tramp                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  tramp
  :ensure nil
  :defer t
  :config
  ;; Optimize TRAMP for auto-revert
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-auto-save" my-tmp-dir))
  (setq tramp-verbose 1)             ; Minimal verbosity
  (setq tramp-default-method "ssh")  ; Stable connection method
  ;; Auto-revert settings for TRAMP
  (setq auto-revert-remote-files t) ; Enable reverting for remote files
  (setq auto-revert-interval 1)     ; Poll every 1 second
  (setq auto-revert-verbose nil)    ; Silence revert messages
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package
 files
 :ensure nil
 :config
 ;; Save place settings
 (setq save-place-file
       (expand-file-name "saveplace/saveplace" my-tmp-dir))
 (save-place-mode 1) ; Enable save-place-mode

 ;; Define a minimal log-mode for .log files
 (define-derived-mode
  log-mode
  fundamental-mode
  "Log"
  "A simple mode for log files."
  (setq font-lock-defaults '(log-mode-font-lock-keywords)))

 ;; Define font-lock keywords for log levels
 (defvar log-mode-font-lock-keywords
   '(("\\bDEBUG\\b" . 'font-lock-comment-face)
     ("\\bINFO\\b" . 'font-lock-string-face)
     ("\\bWARN\\b" . 'font-lock-warning-face)
     ("\\bERROR\\b" . 'font-lock-function-name-face))
   "Font-lock keywords for `log-mode' highlighting.")

 ;; Associate .log files with log-mode
 (add-to-list 'auto-mode-alist '("\\.log\\'" . log-mode))

 :hook
 ((log-mode . auto-revert-tail-mode) ; Enable tailing for log-mode
  (auto-revert-tail-mode
   .
   (lambda ()
     (when (derived-mode-p 'log-mode)
       (goto-char (point-max))
       (when (file-remote-p default-directory)
         (setq buffer-read-only nil) ; Ensure writable for TRAMP
         (let ((tramp-connection-properties
                (cons
                 `(,(tramp-make-tramp-file-name
                     (tramp-file-name-method
                      tramp-default-remote-file-name)
                     (tramp-file-name-user
                      tramp-default-remote-file-name)
                     (tramp-file-name-host
                      tramp-default-remote-file-name)
                     nil)
                   "connection-buffer" "auto-revert")
                 tramp-connection-properties)))
           (auto-revert-set-timer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    vundo                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 vundo
 :ensure t
 :defer t
 :bind ("C-x u" . vundo)
 :config
 (setq vundo-glyph-alist vundo-unicode-symbols)
 (setq vundo-files-directory (expand-file-name "undos" my-tmp-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 deadgrep                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 deadgrep
 :ensure t
 :defer t
 :bind
 (("C-c s" . deadgrep)
  :map
  deadgrep-mode-map
  ("q" . deadgrep-kill-all-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Visual Enhancements                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rainbow Delimiters
(use-package
 rainbow-delimiters
 :ensure t
 :defer t
 :diminish rainbow-delimiters-mode
 :hook (prog-mode . rainbow-delimiters-mode)
 :custom
 (rainbow-delimiters-max-face-count 9)) ;; Default 9 faces

;; Highlight Thing at Point
(use-package
 highlight-thing
 :ensure t
 :defer t
 :custom
 (highlight-thing-delay-seconds 0.5) ; Delay before highlighting
 (highlight-thing-what-thing 'symbol) ; Highlight symbols
 :config
 (set-face-attribute 'highlight-thing nil
                     :background "#5e81ac" ; Soft blue from Modus
                     :weight 'normal)
 :hook (prog-mode . highlight-thing-mode))

(use-package
  indent-bars
  :ensure t
  :diminish indent-bars-mode
  :hook
  ((prog-mode . indent-bars-mode)
   (emacs-lisp-mode . indent-bars-mode)) ;; Ensure .el files work
  :custom
  ;; Appearance
  (indent-bars-pattern ".") ;; Solid bars
  (indent-bars-width-frac 0.2) ;; Thin bars
  (indent-bars-pad-frac 0.1) ;; Minimal padding
  (indent-bars-zigzag nil) ;; Straight bars
  (indent-bars-display-on-blank-lines t) ;; Bars on blank lines (required)
  (indent-bars-prefer-character nil) ;; Stipples for speed
  ;; Highlight current level (required)
  (indent-bars-highlight-current-depth '(:blend 0.6)) ;; Brighter current bar
  ;; Behavior
  (indent-bars-no-descend-strings t) ;; Lock depth in strings
  (indent-bars-no-descend-lists t) ;; Lock depth in lists
  (indent-bars-depth-update-delay 0.05) ;; Fast updates
  ;; Tree-sitter
  (indent-bars-treesit-support t)
  (indent-bars-treesit-scope
   '((python function_definition class_definition)
     (emacs-lisp function_definition)
     (c function_declarator compound_statement)))
  :config
  ;; Force font-lock refresh for .el files
  (defun indent-bars-refresh-font-lock ()
    (when indent-bars-mode
      (font-lock-flush)
      (font-lock-ensure)))
  (add-hook 'indent-bars-mode-hook #'indent-bars-refresh-font-lock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Mode Line Cleanup                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                        ; (global-auto-revert-mode nil "autorevert")
     (flyspell-mode " ✍" "flyspell")
     (which-key-mode nil "which-key")
     (yas-minor-mode nil "yasnippet")
     (smartparens-mode nil "smartparens"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    helm                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helm - Bare Minimum for Aidermacs
(use-package
 helm
 :ensure t
 :defer t
 :init
 (setq helm-mode nil)) ; Disable Helm globally

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Buffer Management                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ("C-M-f" . sp-forward-sexp) ; Jump to next sexp
  ("C-M-b" . sp-backward-sexp) ; Jump to prev sexp
  ("C-M-u" . sp-backward-up-sexp))) ; Up a level

(use-package
  paredit
  :ensure t
  :defer t
  :hook
  ((emacs-lisp-mode . paredit-mode)
   (lisp-mode . paredit-mode)
   (scheme-mode . paredit-mode)
   (clojure-mode . paredit-mode)
   (slime-repl-mode . paredit-mode)
   (lisp-interaction-mode . (lambda () (local-set-key (kbd "C-j") 'eval-print-last-sexp)))
   )
  :bind
  (:map
   paredit-mode-map
   ("C-j" . nil)
   ("C-)" . paredit-forward-slurp-sexp)
   ("C-}" . paredit-forward-barf-sexp)
   ("C-(" . paredit-backward-slurp-sexp)
   ("C-{" . paredit-backward-barf-sexp)))

(use-package
  vertico
  :ensure t
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-sort-function 'vertico-sort-history-length-alpha)
  :config
  (with-eval-after-load 'all-the-icons
    (defun my-consult-buffer-format (buffer)
      "Add all-the-icons to BUFFER name for consult-buffer."
      (let ((icon (all-the-icons-icon-for-buffer buffer)))
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

(use-package
  orderless
  :ensure t
  :demand t
  :after minibuffer
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults completion-category-overrides nil)
  (completion-category-overrides
   '((eglot (styles orderless)) (eglot-capf (styles orderless))))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package
  consult
  :ensure t
  :after vertico
  :demand t
  :config (require 'consult) (setq history-length 1000)
  (setq savehist-additional-variables
        (append
         savehist-additional-variables '(extended-command-history)))
  (setq consult-preview-key 'any) ;; Preview on any key
  (setq consult-narrow-key "<") ;; Narrowing key
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
  (
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("M-y" . consult-yank-pop)
   ("M-g i" . consult-imenu)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s f" . consult-find)
   ))

(use-package marginalia :ensure t :init (marginalia-mode 1))

(use-package consult-yasnippet :ensure t :after (consult yasnippet))

(use-package
  corfu
  :ensure t
  :init
  (global-corfu-mode 1) ; Enable Corfu globally
  (corfu-popupinfo-mode 1) ; Show documentation in popups
  :custom
  (corfu-cycle t) ; Cycle through candidates
  (corfu-auto t) ; Auto-show completions after typing
  (corfu-preselect 'directory)
  (corfu-auto-prefix 2) ; Show completions after 2 characters
  (corfu-auto-delay 0.1) ; Fast popup display
  (corfu-quit-at-boundary t) ; Quit if no match at word boundary
  (corfu-quit-no-match t) ; Quit if no matches
  (corfu-preselect-first t) ; Preselect first candidate
  (corfu-popupinfo-delay '(0.5 . 0.2)) ; Delay for documentation popup
  :bind
  (:map
   corfu-map
   ("TAB" . corfu-complete) ; Accept current suggestion
   ([tab] . corfu-complete) ; Ensure both TAB forms work
   ("<down>" . corfu-next) ; Cycle down with down arrow
   ("<up>" . corfu-previous) ; Cycle up with up arrow
   ("M-d" . corfu-popupinfo-toggle)) ; Toggle documentation
  :config
  (keymap-set
   corfu-map "RET"
   `(menu-item
     "" nil
     :filter
     ,(lambda (&optional _)
        (and (derived-mode-p 'eshell-mode 'comint-mode)
             #'corfu-send))))
  ;; Disable Corfu in minibuffer to avoid conflicts with Vertico
  (defun corfu-disable-in-minibuffer ()
    "Disable Corfu in minibuffer."
    (when (minibufferp)
      (corfu-mode -1)))
  (add-hook 'minibuffer-setup-hook #'corfu-disable-in-minibuffer))

(use-package
 all-the-icons
 :ensure t
 :config
 (setq all-the-icons-scale-factor 1.1) ; Similar to your nerd-icons setting
 ;; Install fonts if not already present (run once manually if needed)
 (unless (find-font (font-spec :name "all-the-icons"))
   (all-the-icons-install-fonts t)))

(use-package
  all-the-icons-completion
  :ensure t
  :after (all-the-icons marginalia)
  :config (all-the-icons-completion-mode)
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Power User Essentials                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region :ensure t :bind ("C-=" . er/expand-region))

(use-package
  multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package
  crux
  :ensure t
  :bind
  (("C-c o" . crux-open-with)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c k" . crux-kill-other-buffers)
   ("C-a" . crux-move-beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Editing Helpers                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 diff-hl
 :ensure t
 :defer t
 :hook (magit-post-refresh . diff-hl-magit-post-refresh)
 :config (global-diff-hl-mode +1))

(use-package
  which-key
  :ensure nil ; Built-in since Emacs 29, no need to ensure
  :hook (after-init . which-key-mode)
  :config
                                        ;(setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 0.2)
  (setq which-key-idle-secondary-delay 0.1)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-separator " → ")
  (setq which-key-prefix-prefix "+"))

(use-package
 avy
 :ensure t
 :defer t
 :bind (("M-j" . avy-goto-char-timer) ("C-'" . avy-goto-char-2))
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

(use-package
  avy-zap
  :ensure t
  :after avy
  :bind
  (("M-z" . avy-zap-up-to-char-dwim) ("M-Z" . avy-zap-to-char-dwim))
  :config
  (setq avy-zap-forward-only t)
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package ace-window :ensure t :after avy :bind ("M-o" . ace-window))

(global-set-key (kbd "C-x o") #'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    XKCD                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 xkcd
 :ensure t
 :defer t
 :config
 ;; Optional: Customize cache directory
 (setq xkcd-cache-dir "~/.config/emacs/xkcd/")
 ;; Hook to set cursor-type to a non-blinking state in xkcd buffers
 :hook (xkcd-mode . (lambda () (setq-local cursor-type '(bar . 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Flyspell                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 flyspell
 :ensure nil
 :hook
 ((text-mode-hook . flyspell-mode) ;; Prot enables in text modes
  (org-mode-hook . flyspell-mode) ;; Added for your Org usage
  (prog-mode-hook . flyspell-prog-mode)) ;; Comments/strings in code
 :config
 (setq ispell-program-name "aspell") ;; Prot uses aspell
 (setq ispell-dictionary "en_US") ;; Default dictionary
 (setq ispell-extra-args '("--sug-mode=ultra")) ;; Fast suggestions, Prot's style
 (setq ispell-personal-dictionary "~/.aspell.en.pws") ;; Personal words
 ;; Prot's performance tweaks
 (setq flyspell-issue-message-flag nil) ;; No chatter
 (setq flyspell-issue-welcome-flag nil) ;; No welcome
 ;; Ensure aspell is installed
 (unless (executable-find "aspell")
   (message "Aspell not found; flyspell disabled")
   (flyspell-mode -1))
 :bind
 (:map
  flyspell-mode-map
  ("C-;" . flyspell-correct-wrapper))) ;; Prot's correction key

(use-package
 flyspell-correct
 :ensure t
 :after flyspell
 :bind
 (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Eglot (LSP) Setup                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 eglot
 :ensure nil
 :hook
 ((prog-mode
   .
   (lambda ()
     (unless (or (string-match-p "^\\*.*\\*$" (buffer-name))
                 (string=
                  (buffer-file-name)
                  (expand-file-name "init.el" user-emacs-directory)))
       (eglot-ensure))))
  (eglot-managed-mode
   .
   (lambda ()
     (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
     (setq eldoc-documentation-strategy #'eldoc-documentation-default)
     (eglot-inlay-hints-mode))))
 :config
 (add-to-list
  'eglot-server-programs
  '(emacs-lisp-mode . nil)) ; No LSP server for Emacs Lisp
 ;; Disable python-flymake when eglot is active
 (add-hook
  'eglot-managed-mode-hook
  (lambda ()
    (when (derived-mode-p 'python-mode 'python-ts-mode)
      (remove-hook 'flymake-diagnostic-functions 'python-flymake
                   t)))))

(use-package
 consult-lsp
 :ensure t
 :after (eglot consult)
 :bind
 (:map
  eglot-mode-map
  ("C-c l a" . consult-lsp-code-actions)
  ("C-c l d" . consult-lsp-diagnostics)
  ("C-c l s" . consult-lsp-symbols)
  ("C-c l f" . consult-lsp-file-symbols)
  ("C-c l i" . consult-lsp-implementation)
  ("C-c l r" . consult-lsp-references)
  ("C-c l D" . consult-lsp-definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Org Mode Setup                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org Mode Setup
(use-package
  org
  :ensure nil
  :init
  ;; Load required Org extensions early
  (require 'org-protocol)
  (require 'org-download)
  (require 'org-id)

  ;; Define prefix keymap globally
  (defvar my-org-prefix-map (make-sparse-keymap)
    "Prefix keymap for Org Mode commands.")
  (define-prefix-command 'my-org-prefix-map)
  (global-set-key (kbd "C-c o") 'my-org-prefix-map)

  :config
  ;; Core Org settings
  (setq org-directory "~/.org/")
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-return-follows-link t)
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)
  (setq org-log-done 'time)
  (setq org-id-track-globally t) ; Unique IDs for sync safety
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "NEXT(n)"
           "WAITING(w@/!)"
           "|"
           "DONE(d!)"
           "CANCELED(c@)")))
  (setq org-clock-persist 'history)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  ;; Dynamically collect agenda files
  (defun my-org-agenda-files ()
    "Return a list of all Org files for agenda, including local and Denote notes."
    (delete-dups
     (append
      (directory-files "~/.org/" t "^[^b].*\\.org$" t)
      (directory-files "~/Documents/notes/" t "\\.org$" t))))

  (setq org-agenda-files (my-org-agenda-files)) ; Initial setting
  (defun my-org-update-agenda-files ()
    "Update org-agenda-files dynamically."
    (setq org-agenda-files (my-org-agenda-files)))
  (add-hook 'org-agenda-mode-hook #'my-org-update-agenda-files)

  ;; Clock persistence
  (org-clock-persistence-insinuate)

  ;; Custom key bindings under C-c o
  (define-key my-org-prefix-map (kbd "o") #'my-org-open-agenda)
  (define-key my-org-prefix-map (kbd "c") #'org-capture)
  (define-key my-org-prefix-map (kbd "r") #'my-org-refile-to-todos)
  (define-key my-org-prefix-map (kbd "O") #'org-download-clipboard)
  (define-key my-org-prefix-map (kbd "t") #'my-org-show-todo-list)
  (define-key my-org-prefix-map (kbd "n") #'my-org-capture-note-quick)
  (define-key my-org-prefix-map (kbd "d") #'my-denote-capture-note)

  ;; Agenda keymap
  (defvar my-org-agenda-map (make-sparse-keymap)
    "Keymap for Org Agenda commands.")
  (define-key my-org-prefix-map (kbd "a") my-org-agenda-map)
  (define-key my-org-agenda-map (kbd "a") #'org-agenda)
  (define-key my-org-agenda-map (kbd "t") #'my-org-agenda-today)
  (define-key
   my-org-agenda-map (kbd "c") #'my-org-agenda-goto-current-clock)

  ;; Which-key integration
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-c o"
      "org-mode"
      "C-c o a"
      "agenda"
      "C-c o d"
      "denote"
      "C-c o t"
      "todo-list"))

  ;; Utility functions
  (defun my-org-refile-to-todos ()
    "Refile current heading to todos.org under 'Tasks'."
    (interactive)
    (org-refile
     nil nil
     (list
      "Tasks" (expand-file-name "todos.org" org-directory) nil nil)))

  (defun my-org-agenda-today ()
    "Show agenda for today with open TODOs."
    (interactive)
    (org-agenda nil "d"))

  (defun my-org-show-todo-list ()
    "Show all open TODOs in an agenda buffer."
    (interactive)
    (when (get-buffer-window "*Org Agenda*")
      (delete-window (get-buffer-window "*Org Agenda*")))
    (org-switch-to-buffer-other-window "*Org Agenda*")
    (org-agenda nil "t"))

  (defun my-org-capture-note-quick ()
    "Quickly capture a note without switching buffers."
    (interactive)
    (org-capture nil "n"))

  (defun my-org-agenda-goto-current-clock ()
    "Jump to the currently clocked task in agenda."
    (interactive)
    (org-agenda nil "a")
    (org-agenda-goto (org-clock-is-active)))

  (defun my-org-open-agenda ()
    "Open the Org Agenda in a separate window."
    (interactive)
    (when (get-buffer-window "*Org Agenda*")
      (delete-window (get-buffer-window "*Org Agenda*")))
    (org-switch-to-buffer-other-window "*Org Agenda*")
    (org-agenda nil "a")
    (call-interactively #'org-agenda-day-view))

  ;; Denote integration
  (defun my-denote-capture-note ()
    "Capture a note using Denote and link it to the current Org file."
    (interactive)
    (let ((denote-file (denote-create-note)))
      (org-capture nil "n")
      (insert
       (format "[[file:%s][%s]]"
               denote-file
               (file-name-base denote-file)))
      (org-capture-finalize t)))

  :hook (org-capture-prepare-finalize . org-id-get-create))

(use-package
 org-agenda
 :ensure nil
 :after org
 :config
 ;; Custom agenda commands
 (setq org-agenda-custom-commands
       '(("u" "Unified Agenda and Tasks"
          ((agenda "" ((org-agenda-span 'week)))
           (todo
            "TODO|NEXT|WAITING"
            ((org-agenda-overriding-header "All Tasks")))))
         ("t" "All Open TODOs"
          ((todo
            "TODO|NEXT|WAITING"
            ((org-agenda-overriding-header "Current Open TODOs")))))
         ("d" "Today's Agenda with Open TODOs"
          ((agenda
            ""
            ((org-agenda-span 'day)
             (org-agenda-overriding-header "Today's Schedule")))))
         (todo
          "TODO|NEXT|WAITING"
          ((org-agenda-overriding-header
            "Open TODOs (Today or Unscheduled)")
           (org-agenda-skip-function
            '(org-agenda-skip-entry-if
              'scheduled
              'deadline
              'notregexp
              "^\\*\\s-+\\(TODO\\|NEXT\\|WAITING\\)"))))))

 (setq org-agenda-start-on-weekday 1)
 (setq org-agenda-span 'week)
 (setq org-agenda-include-diary t)
 (setq org-agenda-sorting-strategy
       '((agenda habit-down time-up priority-down tag-up)
         (todo priority-down category-keep)
         (tags priority-down category-keep)
         (search category-keep)))
 (setq org-agenda-log-mode-items '(closed clocked))
 (setq org-agenda-start-with-log-mode t)
 (setq org-agenda-prefix-format
       '((agenda . " %i %-12:c%?-12t% s [%e] ")
         (todo . " %i %-12:c")
         (tags . " %i %-12:c")
         (search . " %i %-12:c"))))

(use-package
 org-capture
 :ensure nil
 :after org
 :config
 (setq
  org-capture-templates
  `(("t" "Todo" entry
     (file+headline
      ,(expand-file-name "tasks.org" org-directory) "Tasks")
     "* TODO %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n")
    ("m" "Meeting" entry
     (file+headline
      ,(expand-file-name "calendar.org" org-directory) "Meetings")
     "* MEETING %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\nSCHEDULED: %^T\n%a")
    ("n" "Note" entry
     (file+headline
      ,(expand-file-name "notes/notes.org" org-directory) "Notes")
     "* %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:END:\n%a")
    ("w" "Web Capture" entry
     (file+headline
      ,(expand-file-name "web.org" org-directory) "Web")
     "%:initial"
     :immediate-finish t)
    ("o" "Outlook Email TODO" entry
     (file+headline
      ,(expand-file-name "tasks.org" org-directory) "Tasks")
     "* TODO %:subject\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:EMAIL_LINK: %:link\n:END:\n\n%:initial\n"
     :immediate-finish t)
    ("e"
     "Email"
     entry
     (file+headline "~/org/inbox.org" "Emails")
     "* TODO %:subject\n:PROPERTIES:\n:EMAIL: %:message-id\n:END:\n%?"
     :immediate-finish t)
    ("r"
     "RSS"
     entry
     (file+headline "~/org/inbox.org" "RSS")
     "* TODO %:title\n:PROPERTIES:\n:URL: %:url\n:END:\n%?"
     :immediate-finish t)))
 :hook
 (org-capture-after-finalize
  .
  (lambda ()
    (when (get-buffer-window "*Capture*")
      (delete-window (get-buffer-window "*Capture*"))))))

(use-package
 org-protocol
 :ensure nil
 :after org
 :config
 (setq org-protocol-default-template-key nil) ; Use template key from URL
 (add-to-list 'org-modules 'org-protocol))

(use-package
 org-download
 :ensure t
 :hook (dired-mode . org-download-enable)
 :config
 (setq org-download-image-dir
       (expand-file-name "images" org-directory)))

(use-package
 org-attach
 :ensure nil
 :after org
 :config
 (setq org-attach-dir-relative t)
 (setq org-attach-use-inheritance t)
 (setq org-attach-id-dir
       (expand-file-name "attachments" org-directory)))

(use-package
  org-modern
  :ensure t
                                        ; :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-agenda-tags-column 0
   org-ellipsis "…")
  (setq org-modern-table-vertical 2)
  (setq org-modern-table-horizontal 2)
  (global-org-modern-mode)
  )

(use-package
  org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Magit/Forge                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;; Show icons for files in the Magit status and other buffers.
  (with-eval-after-load 'magit
    (setq magit-format-file-function #'magit-format-file-nerd-icons)))

(use-package magit-repos
  :ensure nil ; part of `magit'
  :commands (magit-list-repositories)
  :init
  (setq magit-repository-directories
        '(("~/Code" . 1))))

(use-package forge :ensure t :defer t :after magit)

(use-package magit-todos :ensure t :defer t :after magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Grep Ignorance                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; DOOM MODELINE

;; (use-package
;;   doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;;;;;
;; MODE LINE
;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Aidermacs (Anthropic)                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  aidermacs
  :ensure t
  :config
  (setq aidermacs-default-model "claude-sonnet-4-20250514")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  (setq
   aidermacs-auto-commits t
   aidermacs-show-diff-after-change nil)
  (setq
   aidermacs-use-architect-mode nil
   aidermacs-auto-accept-architect nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Misc Packages                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Ibuffer                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer)) ; Override default buffer switch
  :hook
  ((ibuffer-mode . my-ibuffer-setup-hook)
   (ibuffer-mode . ibuffer-auto-mode)) ; Auto-update buffer list
  :custom
  (ibuffer-expert t) ; Skip confirmation for dangerous operations
  (ibuffer-show-empty-filter-groups nil) ; Hide empty groups
  (ibuffer-display-summary nil) ; Show summary at bottom
  (ibuffer-default-sorting-mode 'major-mode)  ; Sort by major mode initially
  (ibuffer-use-header-line t) ; Use header line for filter info
  :config
  ;; Define saved filter groups
  (setq
   ibuffer-saved-filter-groups
   '(("default" ("Emacs Lisp"
                 (derived-mode . emacs-lisp-mode)) ; Filter by derived mode
      ("Org"
       (derived-mode . org-mode)) ; Filter Org and derived modes
      ("Programming"
       (derived-mode . prog-mode)) ; All programming modes
      ("Dired" (mode . dired-mode))
      ("Special"
       (name . "^\\*.*\\*$")) ; Buffers starting and ending with *
      ("Files"
       (and (filename . ".*") ; File-backed buffers
            (not (name . "^\\*.*\\*$")))) ; Exclude special buffers
      ("EXWM" (mode . exwm-mode))
      ("Other"
       (name . ".*"))))) ; Catch-all group

  ;; Custom setup hook for ibuffer
  (defun my-ibuffer-setup-hook ()
    "Set up ibuffer with saved filters, auto-mode, and visual enhancements."
    (ibuffer-switch-to-saved-filter-groups "default")
    (ibuffer-auto-mode 1) ; Enable auto-updates
    (hl-line-mode 1) ; Highlight current line
    ;; Ensure header line is styled
    (set-face-attribute 'header-line nil
                        :background "#2e3440" ; Modus Vivendi dark
                        :foreground "#d8dee9" ; Light text
                        :box "#88c0d0")
    ;; Refresh buffer list
    (ibuffer-update nil t))

  ;; Ensure all-the-icons for visual enhancement
  (use-package
    all-the-icons-ibuffer
    :ensure t
    :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
    :config
    (setq all-the-icons-ibuffer-icon-size 1.0)
    (setq all-the-icons-ibuffer-icon-v-adjust 0.0)
    (setq all-the-icons-ibuffer-human-readable-size t) ; Readable file sizes
    ;; Ensure icons render correctly with modus-vivendi
    (set-face-attribute 'all-the-icons-ibuffer-file-face nil
                        :foreground "#88c0d0") ; Cyan for files
    (set-face-attribute 'all-the-icons-ibuffer-dir-face nil
                        :foreground "#81a1c1"
                        :weight 'bold)) ; Blue for dirs

  ;; Custom functions for ibuffer
  (defun my-ibuffer-mark-unsaved-buffers ()
    "Mark all unsaved file-visiting buffers."
    (interactive)
    (ibuffer-mark-for-delete nil) ; Clear existing marks
    (ibuffer-mark-unsaved-buffers))

  (defun my-ibuffer-mark-special-buffers ()
    "Mark all special buffers (starting with *)."
    (interactive)
    (ibuffer-mark-for-delete nil) ; Clear existing marks
    (ibuffer-mark-by-name-regexp "^\\*.*\\*$"))

  (defun my-ibuffer-mark-dired-buffers ()
    "Mark all dired buffers."
    (interactive)
    (ibuffer-mark-for-delete nil) ; Clear existing marks
    (ibuffer-mark-by-mode 'dired-mode))

  (defun my-ibuffer-toggle-filter-group-display ()
    "Toggle display of empty filter groups."
    (interactive)
    (setq ibuffer-show-empty-filter-groups
          (not ibuffer-show-empty-filter-groups))
    (ibuffer-update nil t))

  ;; Keybindings for ibuffer
  (define-key
   ibuffer-mode-map (kbd "/ u") #'my-ibuffer-mark-unsaved-buffers)
  (define-key
   ibuffer-mode-map (kbd "/ *") #'my-ibuffer-mark-special-buffers)
  (define-key
   ibuffer-mode-map (kbd "/ d") #'my-ibuffer-mark-dired-buffers)
  (define-key
   ibuffer-mode-map
   (kbd "/ t")
   #'my-ibuffer-toggle-filter-group-display)
  (define-key
   ibuffer-mode-map
   (kbd "C-c C-g")
   #'ibuffer-switch-to-saved-filter-groups)

  ;; Which-key integration
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "C-x C-b"
      "ibuffer"
      "/ u"
      "mark-unsaved"
      "/ *"
      "mark-special"
      "/ d"
      "mark-dired"
      "/ t"
      "toggle-filter-groups"
      "/ p"
      "filter-project")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Dired                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   ("C-c g" . dired-git-info-mode)
   ("C-c t" . dired-toggle-read-only)
   ("M-!" . dired-smart-shell-command)
   ("C-c o" . dired-open-externally)
   ("C-c w" . dired-copy-file-path)
   ("C-c f" . dired-consult-filter))
  :hook
  (
                                        ;(dired-mode . dired-hide-details-mode)
   (dired-mode . all-the-icons-dired-mode)
   (dired-mode . dired-preview-mode)
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
    all-the-icons-dired
    :ensure t
    :after (all-the-icons dired)
    :hook (dired-mode . all-the-icons-dired-mode)
    :config (setq all-the-icons-dired-monochrome nil)
    (set-face-attribute 'all-the-icons-dired-dir-face nil
                        :foreground "#81a1c1"))
  (use-package
    dired-preview
    :ensure t
    :custom
    (dired-preview-delay 0)
    (dired-preview-max-size (* 10 1024 1024))
    :config (dired-preview-global-mode 1))
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
    :config (setq dired-subtree-use-backgrounds nil)
    (set-face-attribute 'dired-subtree-depth-1-face nil
                        :background "#3b4252"))
  (use-package
    dired-async
    :ensure nil
    :after dired
    :config (dired-async-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     eww                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eww
  :ensure nil
  :commands (eww eww-browse-url)
  :init
  (setq browse-url-handlers
        '(("\\.pdf\\'" . my-open-remote-pdf-in-emacs)
          ("^https?://" . eww-browse-url)))
  :config
  (setq eww-auto-rename-buffer 'title)  ; Nicer buffer names
  ;; Advice EWW to launch certain URLs using the generic launcher rather than EWW.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     pdf                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  DENOTE                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ERC (IRC Client)                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 erc
 :ensure nil
 :defer t
 :config
 (setq
  erc-track-remove-disconnected-buffers t
  erc-hide-list '("PART" "QUIT" "JOIN")
  erc-interpret-mirc-color t
  erc-kill-queries-on-quit t
  erc-kill-server-buffer-on-quit t
  erc-track-shorten-start 8
  erc-kill-buffer-on-part t
  erc-auto-query 'bury
  erc-prompt
  (lambda ()
    (concat
     (propertize "ERC> "
                 'face
                 '(:foreground "cyan" :weight bold))
     (buffer-name)))
  erc-timestamp-format "[%Y-%m-%d %H:%M] "
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
  erc-log-channels-directory (expand-file-name "irc-logs" my-tmp-dir)
  erc-save-buffer-on-part t
  erc-log-write-after-insert t)
 (setq erc-modules '(networks notifications match))
 (erc-update-modules)
 (erc-timestamp-mode 1)
 (erc-track-mode 1)
 (erc-autojoin-mode 1)
 (require 'erc-button)
 (erc-button-mode 1)
 (setq erc-button-url-open-function 'eww-browse-url)
 (set-face-attribute 'erc-nick-default-face nil :foreground "#bd93f9")
 (set-face-attribute 'erc-timestamp-face nil :foreground "#6272a4")
 (set-face-attribute 'erc-my-nick-face nil
                     :foreground "#ff79c6"
                     :weight 'bold)
 ;; Custom face for nick mentions with background highlight
 (defface erc-nick-mentioned-face
   '((t
      :background "#452950"
      :foreground "#ffcc66"
      :weight bold
      :extend t))
   "Face for messages where my nick is mentioned in ERC, using Modus Vivendi colors."
   :group 'erc-faces)
 ;; Modified function to highlight message and add fringe marker
 (defun my-erc-highlight-nick-message (msg)
   "Highlight the entire message and add a fringe marker when my nick is mentioned."
   (let ((nick (erc-current-nick)))
     (when (and nick (string-match-p (regexp-quote nick) msg))
       ;; Highlight the full message
       (put-text-property
        0 (length msg) 'face 'erc-nick-mentioned-face
        msg)
       ;; Add fringe indicator
       (put-text-property 0 1 'display
                          '(left-fringe
                            erc-nick-mentioned-fringe bitmap)
                          msg))))
 ;; Add the highlight function to ERC's message insertion hook
 (add-hook
  'erc-insert-modify-hook
  (lambda ()
    (when (erc-server-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((msg (buffer-substring (point) (line-end-position))))
            (my-erc-highlight-nick-message msg))
          (forward-line 1))))))
 ;; Define fringe bitmap for red asterisk
 (define-fringe-bitmap 'erc-nick-mentioned-fringe
   [#b00000000
    #b00111000 #b01111100 #b11111110 #b01111100 #b00111000 #b00000000]
   nil nil 'center)
 ;; Define face for fringe marker
 (defface erc-nick-mentioned-fringe-face
   '((t :foreground "red" :weight bold))
   "Face for the fringe marker when nick is mentioned."
   :group 'erc-faces)
 ;; Associate face with fringe bitmap
 (put
  'erc-nick-mentioned-fringe 'face 'erc-nick-mentioned-fringe-face)
 ;; Configure ERC match for nick highlighting
 (setq erc-keywords '())
 (defun my-erc-update-keywords ()
   "Update erc-keywords with the current nick."
   (let ((nick (erc-current-nick)))
     (when nick
       (setq erc-keywords (list nick)))))
 (add-hook
  'erc-after-connect-hook
  (lambda (&rest _args) (my-erc-update-keywords)))
 (add-hook 'erc-nick-changed-hook #'my-erc-update-keywords)
 (setq erc-match-keywords-hook nil)
 (add-hook
  'erc-match-keywords-hook
  (lambda ()
    (when (and (erc-server-buffer) (erc-current-nick))
      (my-erc-highlight-nick-message
       (buffer-substring (point-min) (point-max))))))
 ;; Completion setup
 (defun my-erc-setup-completion ()
   "Set up ERC completion with pcomplete and corfu."
   (require 'erc-pcomplete)
   (pcomplete-erc-setup) ; ERC's nick completion
   (setq-local completion-at-point-functions
               (list #'erc-pcomplete)) ; Use pcomplete for completion
   (corfu-mode 1)) ; Enable corfu for dropdown UI
 ;; Disable line numbers in ERC
 (add-hook 'erc-mode-hook (lambda () (display-line-numbers-mode -1)))

 (defun my-erc-connect ()
   "Retrieve IRC credentials from authinfo.gpg and connect to the IRC server"
   (interactive)
   (let* ((host "samhain.su")
          (port "7000")
          (auth-entry
           (car
            (auth-source-search
             :host host
             :port port
             :require '(:user :secret)
             :max 1)))
          (username (plist-get auth-entry :user))
          (password
           (if (functionp (plist-get auth-entry :secret))
               (funcall (plist-get auth-entry :secret))
             (plist-get auth-entry :secret))))
     (unless (and username password)
       (error "Could not retrieve IRC credentials from authinfo.gpg"))
     (erc-tls
      :server host
      :port (string-to-number port)
      :nick username
      :password password
      :full-name "blackdream")))

 :hook
 ((erc-mode . my-erc-set-fill-column)
  (erc-nick-changed . my-erc-update-notifications-keywords)
  (erc-insert-post . erc-save-buffer-in-logs)
  (erc-mode . my-erc-setup-completion)) ; Replace old completion hook
 :bind
 (:map
  erc-mode-map
  ("C-c e" . erc-button-browse-url)
  ("C-c l" . erc-view-log-mode)
  ("TAB" . completion-at-point) ; Explicitly bind TAB
  :map
  global-map
  ("C-c E" . my-erc-connect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Native Tree-sitter Configuration                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove the treesit-auto package and use native tree-sitter
(use-package
 emacs
 :ensure nil
 :init
 ;; Tree-sitter language sources
 (setq treesit-language-source-alist
       '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
         (c . ("https://github.com/tree-sitter/tree-sitter-c"))
         (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
         (css . ("https://github.com/tree-sitter/tree-sitter-css"))
         (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
         (go . ("https://github.com/tree-sitter/tree-sitter-go"))
         (html . ("https://github.com/tree-sitter/tree-sitter-html"))
         (java . ("https://github.com/tree-sitter/tree-sitter-java"))
         (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
         (json . ("https://github.com/tree-sitter/tree-sitter-json"))
         (lua . ("https://github.com/MunifTanjim/tree-sitter-lua"))
         (make . ("https://github.com/alemuller/tree-sitter-make"))
         (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
         (python . ("https://github.com/tree-sitter/tree-sitter-python"))
         (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
         (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
         (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
         (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
         (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

 ;; Automatic installation of tree-sitter grammars
 (defun my-install-missing-treesit-grammars ()
   "Install missing tree-sitter grammars."
   (interactive)
   (dolist (lang-src treesit-language-source-alist)
     (let ((lang (car lang-src)))
       (unless (treesit-language-available-p lang)
         (when (y-or-n-p (format "Install tree-sitter grammar for %s? " lang))
           (treesit-install-language-grammar lang))))))

 ;; Major mode remapping to use tree-sitter variants
 (setq major-mode-remap-alist
       '((c-mode . c-ts-mode)
         (c++-mode . c++-ts-mode)
         (cmake-mode . cmake-ts-mode)
         (conf-toml-mode . toml-ts-mode)
         (css-mode . css-ts-mode)
         (js-mode . js-ts-mode)
         (js-json-mode . json-ts-mode)
         (python-mode . python-ts-mode)
         (sh-mode . bash-ts-mode)
         (typescript-mode . typescript-ts-mode)
         (rust-mode . rust-ts-mode)
         (go-mode . go-ts-mode)
         (yaml-mode . yaml-ts-mode)))

 ;; Font-lock level for tree-sitter modes
 (setq treesit-font-lock-level 4) ; Maximum highlighting

 ;; Tree-sitter based indentation settings
 (setq treesit-simple-indent-presets
       (append
        treesit-simple-indent-presets
        '((prot-indent-close
           (lambda (node parent bol)
             (save-excursion
               (goto-char bol)
               (looking-at-p
                (rx (or "}" "]" ")")))))))
        ))

 :config
 ;; Enhanced tree-sitter font-lock rules
 (defun my-treesit-fontify-all-strings (lang)
   "Fontify all strings in LANG modes."
   (treesit-font-lock-rules
    :language lang
    :feature 'string
    '((string) @font-lock-string-face)))

 ;; Tree-sitter structural navigation
 (defun my-treesit-beginning-of-defun (&optional arg)
   "Move to beginning of defun using tree-sitter.
With ARG, move that many defuns backward."
   (interactive "p")
   (let ((arg (or arg 1)))
     (if (< arg 0)
         (my-treesit-end-of-defun (- arg))
       (dotimes (_ arg)
         (treesit-beginning-of-defun)))))

 (defun my-treesit-end-of-defun (&optional arg)
   "Move to end of defun using tree-sitter.
With ARG, move that many defuns forward."
   (interactive "p")
   (let ((arg (or arg 1)))
     (if (< arg 0)
         (my-treesit-beginning-of-defun (- arg))
       (dotimes (_ arg)
         (treesit-end-of-defun)))))

 ;; Tree-sitter aware commenting
 (defun my-treesit-comment-or-uncomment ()
   "Comment or uncomment using tree-sitter node detection."
   (interactive)
   (let* ((node (treesit-node-at (point)))
          (start (treesit-node-start node))
          (end (treesit-node-end node)))
     (comment-or-uncomment-region start end)))

 ;; Install grammars on first run
 (add-hook 'emacs-startup-hook
           (lambda ()
             (when (and (treesit-available-p)
                        (not (file-exists-p
                              (expand-file-name "tree-sitter" user-emacs-directory))))
               (my-install-missing-treesit-grammars))))

 :hook
 ;; Enable tree-sitter modes
 ((c-ts-mode c++-ts-mode
   python-ts-mode js-ts-mode
   rust-ts-mode go-ts-mode) . eglot-ensure)

 ;; Configure tree-sitter modes
 ((prog-mode . (lambda ()
                 (when (and (treesit-available-p)
                            (treesit-language-at (point)))
                   ;; Set up tree-sitter navigation
                   (setq-local beginning-of-defun-function
                               #'my-treesit-beginning-of-defun)
                   (setq-local end-of-defun-function
                               #'my-treesit-end-of-defun)))))

 :bind
 (:map prog-mode-map
       ("C-M-a" . my-treesit-beginning-of-defun)
       ("C-M-e" . my-treesit-end-of-defun)
       ("C-c t c" . my-treesit-comment-or-uncomment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Tree-sitter Enhanced Features                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tree-sitter based code folding
(use-package
 treesit-fold
 :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
 :after treesit
 :config
 (global-treesit-fold-mode 1)
 (setq treesit-fold-indicators-priority 100)
 :bind
 (:map prog-mode-map
       ("C-c F t" . treesit-fold-toggle)
       ("C-c F T" . treesit-fold-toggle-all)
       ("C-c F o" . treesit-fold-open)
       ("C-c F O" . treesit-fold-open-all)
       ("C-c F c" . treesit-fold-close)
       ("C-c F C" . treesit-fold-close-all)))

;; Tree-sitter playground for debugging
(use-package
 treesit-explore
 :ensure nil
 :commands (treesit-explore-mode treesit-inspect-mode)
 :bind
 (:map prog-mode-map
       ("C-c t e" . treesit-explore-mode)
       ("C-c t i" . treesit-inspect-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Tree-sitter Specific Configurations                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Python with tree-sitter
(use-package
 python
 :ensure nil
 :mode ("\\.py\\'" . python-ts-mode)
 :config
 (setq python-indent-guess-indent-offset-verbose nil)
 (setq python-indent-offset 4)

 ;; Tree-sitter aware python navigation
 (defun my-python-ts-beginning-of-block ()
   "Move to beginning of Python block using tree-sitter."
   (interactive)
   (when-let ((node (treesit-parent-until
                     (treesit-node-at (point))
                     (lambda (n)
                       (member (treesit-node-type n)
                               '("function_definition"
                                 "class_definition"
                                 "if_statement"
                                 "for_statement"
                                 "while_statement"
                                 "with_statement"))))))
     (goto-char (treesit-node-start node))))

 :hook
 (python-ts-mode . (lambda ()
                     (setq-local treesit-defun-type-regexp
                                 (rx (or "function_definition"
                                         "class_definition")))
                     (treesit-major-mode-setup))))

;; JavaScript/TypeScript with tree-sitter
(use-package
 js
 :ensure nil
 :mode (("\\.js\\'" . js-ts-mode)
        ("\\.jsx\\'" . js-ts-mode))
 :config
 (setq js-indent-level 2)
 :hook
 (js-ts-mode . (lambda ()
                 (setq-local treesit-defun-type-regexp
                             (rx (or "function_declaration"
                                     "arrow_function"
                                     "method_definition"
                                     "class_declaration")))
                 (treesit-major-mode-setup))))

;; Elisp tree-sitter (experimental but available)
(when (treesit-language-available-p 'elisp)
  (define-derived-mode emacs-lisp-ts-mode emacs-lisp-mode "Elisp[TS]"
    "Major mode for Emacs Lisp with tree-sitter support."
    (when (treesit-ready-p 'elisp)
      (treesit-parser-create 'elisp)
      (setq-local treesit-font-lock-settings
                  (treesit-font-lock-rules
                   :language 'elisp
                   :feature 'comment
                   '((comment) @font-lock-comment-face)
                   :language 'elisp
                   :feature 'string
                   '((string) @font-lock-string-face)
                   :language 'elisp
                   :feature 'keyword
                   '((defun) @font-lock-keyword-face
                     (defvar) @font-lock-keyword-face
                     (defcustom) @font-lock-keyword-face
                     (defmacro) @font-lock-keyword-face
                     (let) @font-lock-keyword-face
                     (lambda) @font-lock-keyword-face)))
      (setq-local treesit-defun-type-regexp "\\(defun\\|defmacro\\|defvar\\|defcustom\\)")
      (treesit-major-mode-setup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Tree-sitter Debugging                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-treesit-debug-node-at-point ()
  "Show tree-sitter node information at point."
  (interactive)
  (when (treesit-available-p)
    (let* ((node (treesit-node-at (point)))
           (parent (treesit-node-parent node))
           (type (treesit-node-type node))
           (text (treesit-node-text node))
           (start (treesit-node-start node))
           (end (treesit-node-end node)))
      (message "Node: %s [%s-%s]\nParent: %s\nText: %s"
               type start end
               (and parent (treesit-node-type parent))
               (truncate-string-to-width text 50)))))

(global-set-key (kbd "C-c t d") #'my-treesit-debug-node-at-point)

;; Which-key descriptions
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c t" "tree-sitter"
    "C-c t c" "comment node"
    "C-c t d" "debug node"
    "C-c t e" "explore"
    "C-c t i" "inspect"
    "C-c F" "folding"
    "C-c F t" "toggle fold"
    "C-c F T" "toggle all"
    "C-c F o" "open fold"
    "C-c F O" "open all"
    "C-c F c" "close fold"
    "C-c F C" "close all"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                So-long-mode                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package so-long :ensure nil :config (global-so-long-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Flymake Setup                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  flymake
  :ensure nil
  :hook
  ((prog-mode . flymake-mode)
   (emacs-lisp-mode
    .
    (lambda ()
      ;; Only enable flymake-mode for file-backed buffers
      (when (buffer-file-name)
        (flymake-mode 1))))
   (after-init-hook
    .
    (lambda ()
      (with-current-buffer "*scratch*"
        (flymake-mode -1)))))
  :config (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-no-changes-timeout 1) ; Faster feedback after typing
  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     ;; Only add diagnostic functions for file-backed buffers
     (when (buffer-file-name)
       (add-hook
        'flymake-diagnostic-functions #'elisp-flymake-byte-compile
        nil t)
       (add-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc
                 nil
                 t))))
  :bind
  (:map
   flymake-mode-map
   ("C-c ! l" . flymake-show-buffer-diagnostics)
   ("C-c ! n" . flymake-goto-next-error)
   ("C-c ! p" . flymake-goto-prev-error)))

(use-package
 elisp-lint
 :ensure t
 :commands (elisp-lint-buffer elisp-lint-file)
 :bind (("C-c l" . elisp-lint-buffer))
 :config (setq elisp-lint-ignored-validators '("package-lint")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        snippets (Yasnippet)                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 :config (add-hook 'after-init-hook #'yas-reload-all)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                elisp coding                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 helpful
 :ensure t
 :bind
 ([remap describe-function] . helpful-callable)
 ([remap describe-variable] . helpful-variable)
 ([remap describe-symbol] . helpful-symbol)
 ([remap describe-key] . helpful-key))

(use-package
  elisp-demos
  :ensure t
  :defer t
  :config
  (advice-add
   'helpful-update
   :after #'elisp-demos-advice-helpful-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   0x0.st                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c 0" "0x0-upload")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   eshell                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          "top"
          "less"
          "more"
          "vim"
          "nano"
          "ssh"
          "tail"
          "watch"
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
    (eshell/alias "rg" "rg --color=always") ;; Ripgrep with color
    )

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
   ('eshell-mode . eat-eshell-visual-command-mode))) ;; Subprocess distractions

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   calc                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  calc
  :ensure nil ;; calc is built-in, no need to install
  :bind (("C-c \\" . 'calc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Fix Emacsclient                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-after-make-frame-setup (&optional frame)
  "Initialize UI settings for new FRAMEs on Xorg, including daemon clients."
  (when (display-graphic-p) ; Only for graphical frames
    (with-selected-frame (or frame (selected-frame))
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))))

(unless (daemonp)
  (when (display-graphic-p)
    (my-after-make-frame-setup)))
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (when (display-graphic-p frame)
     (my-after-make-frame-setup frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 LaTeX templates                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  emacs
  :ensure nil
  :config
  (let ((templates-dir "~/.config/emacs/latex/templates/"))
    (when (file-exists-p templates-dir)
      (dolist (file
               (directory-files-recursively templates-dir "\\.el$"))
        (load-file file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 pgmacs Setup                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  pgmacs
  :vc (:url "https://github.com/emarsden/pgmacs" :rev :newest)
  :defer t
  :init
  ;; Load dependency 'pg' from GitHub
  (use-package
    pg
    :vc (:url "https://github.com/emarsden/pg-el" :rev :newest))

  ;; Define connection functions
  (defun pgmacs-connect ()
    "Connect to a PostgreSQL database using credentials from authinfo.gpg."
    (interactive)
    (let* ((connections (auth-source-search :port "5432" :max 10))
           (choices
            (mapcar
             (lambda (entry)
               (format "%s@%s/%s"
                       (plist-get entry :user)
                       (plist-get entry :host)
                       (or (plist-get entry :database) "unknown")))
             connections))
           (selection
            (completing-read "Select connection (or type 'manual'): "
                             (append choices '("manual"))
                             nil
                             t)))
      (if (string= selection "manual")
          (call-interactively #'pgmacs-connect-manual)
        (let* ((index (cl-position selection choices :test #'string=))
               (entry (nth index connections)))
          (when entry
            (let ((conn-string
                   (format
                    "user=%s port=%s dbname=%s host=%s password=%s"
                    (plist-get entry :user)
                    (or (plist-get entry :dbport) "5432")
                    (plist-get entry :database)
                    (plist-get entry :host)
                    (if (functionp (plist-get entry :secret))
                        (funcall (plist-get entry :secret))
                      (plist-get entry :secret)))))
              (pgmacs-open-string conn-string)
              (message "Connected to %s@%s/%s"
                       (plist-get entry :user)
                       (plist-get entry :host)
                       (plist-get entry :database))))))))

  (defun pgmacs-connect-manual ()
    "Manually enter PostgreSQL connection details."
    (interactive)
    (let ((conn-string
           (format "user=%s port=%s dbname=%s host=%s password=%s"
                   (read-string "User: ")
                   (read-string "Port (default 5432): " nil nil "5432")
                   (read-string "Database: ")
                   (read-string "Host: " nil nil "localhost")
                   (read-passwd "Password: "))))
      (pgmacs-open-string conn-string)))

  ;; Set up keymap
  (defvar pgmacs-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "c") #'pgmacs-connect)
      (define-key map (kbd "m") #'pgmacs-connect-manual)
      map)
    "Keymap for pgmacs commands.")
  (global-set-key (kbd "C-c p") pgmacs-map)

  :config
  ;; Ensure pgmacs is loaded
  (require 'pgmacs)

  ;; Enable which-key descriptions
  (when (featurep 'which-key)
    (which-key-add-key-based-replacements
      "C-c p"
      "pgmacs"
      "C-c p c"
      "connect (authinfo)"
      "C-c p m"
      "connect (manual)"))

  :hook (pgmacs-mode . (lambda () (display-line-numbers-mode -1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    LINE NUMBERS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;
;; SLIME
;;;
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
   ("C-c C-c" . slime-compile-defun) ;; Compile defun
   ("C-c C-k" . slime-compile-and-load-file) ;; Compile and load file
   ("C-c C-s" . slime-complete-form) ;; Complete form at point
   ("C-c C-d d" . slime-describe-symbol) ;; Describe symbol
   ("C-c C-d h" . slime-documentation-lookup) ;;  Lookup in CLHS
   ("M-." . slime-edit-definition) ;; Go to definition
   ("M-," . slime-pop-find-definition-stack)) ;; Return from definition
  :custom
  ((slime-default-lisp 'sbcl) ;; Default to SBCL
   (slime-contribs
    '(slime-fancy ;; Load essential contribs
      slime-repl slime-asdf slime-fuzzy slime-autodoc))
   (slime-complete-symbol-function 'slime-fuzzy-complete-symbol) ;; Fuzzy completion
   (slime-fuzzy-completion-in-place t) ;; Complete in buffer
   (slime-autodoc-use-multiline-p t) ;; Better autodoc display
   (slime-enable-evaluate-in-emacs t) ;; Allow Emacs to evaluate Lisp
   (inferior-lisp-program "sbcl") ;; Path to SBCL
   (slime-lisp-implementations ;; Support multiple Lisps
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

;;;;;
;; xoauth2
;;;;;
(use-package
 auth-source-xoauth2-plugin
 :ensure t
 :custom (auth-source-xoauth2-plugin-mode t))

;;;;;
;; mailcap
;;;;;
(require 'mailcap)
(mailcap-parse-mailcaps)

;;;;;
;; VTERM
;;;;;
(use-package vterm
  :ensure t
  :custom
  (vterm-kill-buffer-on-exit t "Automatically kill the vterm buffer when the shell exits.")
  (vterm-always-compile-module t "Always compile the vterm module when needed.")
  :config
  ;; Additional configurations or hooks can be placed here.
  )

;;;;; EAT EAT EAT
(use-package eat
  :ensure t  ;; Automatically install eat from NonGNU ELPA
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

;;;;;
;; GPTEL
;;;;;

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
  ;; Set xAI as the default backend
  (setq gptel-default-backend
        (gptel-make-openai "xAI"
          :host "api.x.ai"
          :endpoint "/v1/chat/completions"
          :stream t
          :key #'gptel-api-key-from-auth-source
          :models '(grok-3-latest)))

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

  ;; Set default model for xAI
  (setq-default gptel-model 'grok-3-latest)

  ;; Ensure API keys are read from ~/.authinfo.gpg
  (setq gptel-api-key-from-auth-source t))

;;;;;
;; AGGRESSIVE INDENT
;;;;;

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  (prog-mode . aggressive-indent-mode)
  (org-mode . aggressive-indent-mode))

;;;; Tooltips (tooltip-mode)
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

;;;; `man' (manpages)
(use-package man
  :ensure nil
  :commands (man)
  :config
  (setq Man-notify-method 'pushy))

;;;; `proced' (process monitor, similar to `top')
(use-package proced
  :ensure nil
  :commands (proced)
  :config
  (setq proced-auto-update-flag 'visible) ; Emacs 30 supports more the `visible' value
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user))

;;;; Emacs server (allow emacsclient to connect to running session)
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;;; Pass interface (password-store)
(use-package password-store
  :ensure t
  ;; Mnemonic is the root of the "code" word (κώδικας).  But also to add
  ;; the password to the kill-ring.  Other options are already taken.
  :bind ("C-c k" . password-store-copy)
  :config
  (setq password-store-time-before-clipboard-restore 30))

(use-package pass
  :ensure t
  :commands (pass))

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
  (setq isearch-wrap-pause t)
  (setq isearch-repeat-on-direction-change t)
  (setq list-matching-lines-jump-to-current-line nil)
  :bind
  ( :map global-map
    ("C-." . isearch-forward-symbol-at-point)
    :map minibuffer-local-isearch-map
    ("M-/" . isearch-complete-edit)
    :map occur-mode-map
    ("t" . toggle-truncate-lines)
    :map isearch-mode-map
    ("C-g" . isearch-cancel)
    ("M-/" . isearch-complete)))

;;; General window and buffer configurations
(use-package uniquify
  :ensure nil
  :config

;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; Window history (winner-mode)
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind
  (("C-x <right>" . winner-redo)
   ("C-x <left>" . winner-undo)))

;;; Directional window motions (windmove)
(use-package windmove
  :ensure nil
  :bind
  ;; Those override some commands that are already available with
  ;; C-M-u, C-M-f, C-M-b.
  (("C-M-<up>" . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left))
  :config
  (setq windmove-create-window nil)) ; Emacs 27.1

;;; Show the name of the current definition or heading for context (which-function-mode)
(use-package which-func
  :ensure nil
  :hook (after-init . which-function-mode)
  :config
  (setq which-func-modes '(prog-mode org-mode))
  (setq which-func-display 'mode) ; Emacs 30
  (setq which-func-unknown "")
  (setq which-func-format
        '((:propertize which-func-current
                       face italic
                       mouse-face mode-line-highlight))))

;;; General minibuffer settings
(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq completion-styles '(basic substring initials flex orderless))
  (setq completion-pcm-leading-wildcard t)
  (setq completion-category-defaults nil)
  (setq completion-auto-deselect nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select 'second-tab)
  (setq completion-show-help nil)
  (setq completion-show-inline-help nil)
  (setq completions-detailed t)
  (setq completions-format 'one-column)
  (setq completions-header-format "")
  (setq completions-highlight-face 'completions-highlight)
  (setq completions-max-height 10)
  (setq completions-sort 'historical)
  (setq completion-eager-display 'auto)
  (setq minibuffer-completion-auto-choose t)
  (setq minibuffer-visible-completions nil))

;;; Shell (M-x shell)
(use-package shell
  :ensure nil
  :bind
  (
   ("C-c E" . shell)
   :map shell-mode-map
   ("C-c C-k" . comint-clear-buffer)
   ("C-c C-w" . comint-write-output))
  :config
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t) ; Emacs 29.1
  (setq shell-has-auto-cd nil) ; Emacs 29.1
  (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
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

;;;;;
;; definitions
;;;;;
(use-package
  dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org")
  ;;(dictionary-server "localhost")
  :bind
  ("<f6>" . dictionary-lookup-definition))

;;;;;
;; log viewing
;;;;;
(use-package logview
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
  (add-to-list 'auto-mode-alist '("log\\'" . logview-mode)))

;;;;;
;; pulsar
;;;;;
(use-package pulsar
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.1)
  (pulsar-iterations 15)
  (pulsar-face 'isearch)
  (pulsar-highlight-face 'pulsar-yellow)
  :init
  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-middle)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  :config
  (
   pulsar-global-mode 1))

;;;;;
;; volatile highlighting
;;;;;
(use-package
  volatile-highlights
  :ensure t
  :init (volatile-highlights-mode 1))

;; SMTP configuration for sending mail
(use-package smtpmail
  :ensure nil
  :config
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587
        smtpmail-debug-info t
        smtpmail-debug-verb t))

;;;;; mastodon
(use-package mastodon
  :ensure t
  :defer t
  :config
  (setq mastodon-active-user "blackdream"
        mastodon-instance-url "https://defcon.social")
  (mastodon-discover))

;;;;; HACKERNEWS
(use-package hnreader
  :ensure t
  :defer t)



;;;;; stupid fucking emojis
;; im tired of the squares
(use-package emojify
  :config
  (when (member "Noto Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Noto Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  :hook (after-init . global-emojify-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Final Cleanup                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'eshell 'disabled nil)
(provide 'init)
;;; init.el ends here
