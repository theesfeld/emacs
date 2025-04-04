;;; init.el -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Early Initial Settings                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gnus-home-directory
  (expand-file-name "gnus" user-emacs-directory)
  "Gnus home directory.")
(defvar gnus-startup-file
  (expand-file-name ".gnus.el" gnus-home-directory)
  "Gnus startup file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  MELPA                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 package
 :ensure nil
 :init
 (add-to-list
  'package-archives '("melpa" . "https://melpa.org/packages/")
  t)
 :config (package-initialize)
 (unless package-archive-contents
   (package-refresh-contents))
 (setq use-package-always-ensure t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CUSTOM FUNCTIONS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun grim/emacs-everywhere-wayland-app-info ()
  "Return a dummy app info struct for Wayland."
  (make-emacs-everywhere-app
   :id "wayland"
   :class "wayland-app"
   :title "Unknown"
   :geometry '(0 0 800 600)))

;; Basic screenshot function (Wayland example) - Untouched core with enhancements
(defun grim/screenshot (&optional type)
  "Export current frame as screenshot to clipboard using TYPE format (png/svg/pdf/postscript)."
  (interactive (list
                (intern
                 (completing-read
                  "Screenshot type: " '(png svg pdf postscript)))))
  (let* ((extension
          (pcase type
            ('png ".png")
            ('svg ".svg")
            ('pdf ".pdf")
            ('postscript ".ps")
            (_ (error "Unsupported screenshot type: %s" type))))
         (filename (make-temp-file "Emacs-" nil extension))
         (data (x-export-frames nil type)))
    (with-temp-file filename
      (insert data))
    (cond
     ((executable-find "wl-copy") ; Wayland
      (with-temp-buffer
        (insert-file-contents filename)
        (call-process-region (point-min) (point-max) "wl-copy"
                             nil nil nil "-t"
                             (format "image/%s"
                                     (substring extension 1)))))
     ((executable-find "xclip") ; X11 fallback
      (with-temp-buffer
        (insert-file-contents filename)
        (call-process-region (point-min) (point-max) "xclip"
                             nil nil nil "-selection" "clipboard" "-t"
                             (format "image/%s"
                                     (substring extension 1)))))
     (t
      (message "No clipboard tool found (wl-copy/xclip)")))
    (set-register ?s filename)
    (when (or (executable-find "wl-copy") (executable-find "xclip"))
      (alert
       (format "Screenshot (%s) copied to clipboard and saved to %s"
               type filename)
       :title "Screenshot Taken"
       :severity 'normal))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     ICONS                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 all-the-icons
 :ensure t
 :if (display-graphic-p) ; Only load in GUI mode
 :config
 (setq all-the-icons-scale-factor 1.1) ; Similar to your nerd-icons setting
 ;; Install fonts if not already present (run once manually if needed)
 (unless (find-font (font-spec :name "all-the-icons"))
   (all-the-icons-install-fonts t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     EXWM                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq window-system 'x)
  ;; Core EXWM package with all modules
  (use-package
   exwm
   :ensure t
   :config
   ;; Basic EXWM settings
   (setq
    exwm-workspace-show-all-buffers t
    exwm-layout-show-all-buffers t
    exwm-manage-force-tiling t
    mouse-autoselect-window t
    focus-follows-mouse t)
   ;; Natural scrolling attempt
   (setq mouse-wheel-scroll-amount '(5 ((shift) . 1)))
   (setq mouse-wheel-progressive-speed t)
   ;; Clipboard integration with X11
   (setq
    x-select-enable-clipboard t
    x-select-enable-primary t
    select-enable-clipboard t)
   (global-set-key (kbd "M-w") 'kill-ring-save) ;; M-w copies globally
   (global-set-key (kbd "C-y") 'yank) ;; C-y pastes globally
   ;; Mouse cursor size fix
   (setq xterm-set-cursor-size nil)
   (start-process-shell-command
    "xsetroot" nil "xsetroot -cursor_name left_ptr")
   ;; Keybinding setup
   (setq exwm-input-prefix-keys
         '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-& ?\M-: ?\C-\M-j ?\C-\ ))
   ;; Global keybindings (s-tab removed)
   (setq
    exwm-input-global-keys
    `(([?\s-r] . exwm-reset)
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
      ([?\s-e]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "yazi" nil "footclient -e yazi")))
      ([?\s-\ ]
       .
       (lambda ()
         (interactive)
         (counsel-linux-app)))
      ([?\s-v] . consult-yank-pop) ;; Assumes consult is elsewhere
      ([?\s-q]
       .
       (lambda ()
         (interactive)
         (kill-buffer-and-window)))
      ;; Media keys with volume/brightness popups
      ([XF86AudioRaiseVolume]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "pactl-up" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%")
         (message
          "Volume: %s"
          (shell-command-to-string
           "pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n1"))))
      ([XF86AudioLowerVolume]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "pactl-down" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%")
         (message
          "Volume: %s"
          (shell-command-to-string
           "pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n1"))))
      ([XF86AudioMute]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "pactl-mute"
          nil
          "pactl set-sink-mute @DEFAULT_SINK@ toggle")
         (message
          "Volume: %s"
          (if (string-match
               "yes"
               (shell-command-to-string
                "pactl get-sink-mute @DEFAULT_SINK@"))
              "Muted"
            (shell-command-to-string
             "pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n1")))))
      ([XF86AudioMicMute]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "pactl-mic-mute"
          nil
          "pactl set-source-mute @DEFAULT_SOURCE@ toggle")))
      ([XF86MonBrightnessUp]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "brightnessctl-up" nil "brightnessctl set +10%")
         (message "Brightness: %s"
                  (shell-command-to-string
                   "brightnessctl -m | cut -d, -f4"))))
      ([XF86MonBrightnessDown]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "brightnessctl-down" nil "brightnessctl set 10%-")
         (message "Brightness: %s"
                  (shell-command-to-string
                   "brightnessctl -m | cut -d, -f4"))))
      ([XF86AudioPlay]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "playerctl-play" nil "playerctl play-pause")))
      ([XF86AudioPause]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "playerctl-pause" nil "playerctl play-pause")))
      ([XF86AudioNext]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "playerctl-next" nil "playerctl next")))
      ([XF86AudioPrev]
       .
       (lambda ()
         (interactive)
         (start-process-shell-command
          "playerctl-prev" nil "playerctl previous")))
      ([?\s-1]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 1)))
      ([?\s-2]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 2)))
      ([?\s-3]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 3)))
      ([?\s-4]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 4)))
      ([?\s-5]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 5)))
      ([?\s-6]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 6)))
      ([?\s-7]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 7)))
      ([?\s-8]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 8)))
      ([?\s-9]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 9)))
      ([?\s-0]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-switch-create 0)))
      ([?\M-\s-1]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 1)))
      ([?\M-\s-2]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 2)))
      ([?\M-\s-3]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 3)))
      ([?\M-\s-4]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 4)))
      ([?\M-\s-5]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 5)))
      ([?\M-\s-6]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 6)))
      ([?\M-\s-7]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 7)))
      ([?\M-\s-8]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 8)))
      ([?\M-\s-9]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 9)))
      ([?\M-\s-0]
       .
       (lambda ()
         (interactive)
         (exwm-workspace-move-window 0)))))
   ;; Simulation keys for passing common Emacs bindings to applications
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
           ([?\M-w] . [?\C-c]) ;; Map M-w to Ctrl+C for X apps
           ([?\C-y] . [?\C-v]))) ;; Map C-y to Ctrl+V for X apps
   ;; Multi-monitor setup with dynamic workspace count and eDP-1 scaling
   (defun my-exwm-update-displays ()
     "Update EXWM workspaces and frames for each monitor, with eDP-1 scaled."
     (interactive)
     (let*
         ((xrandr-output
           (shell-command-to-string "xrandr --current"))
          (monitors
           (cl-loop
            for
            line
            in
            (split-string xrandr-output "\n")
            when
            (string-match
             "\\(.*\\) connected.*\\([0-9]+x[0-9]+\\+[-0-9]+\\+[-0-9]+\\)"
             line)
            collect
            (list (match-string 1 line) (match-string 2 line))))
          (monitor-count (length monitors))
          (x-position 0))
       (message "Detected monitors: %s" monitors) ;; Debug output
       ;; Sort monitors with eDP-1 last
       (setq monitors
             (sort monitors
                   (lambda (a b)
                     (if (string= (car a) "eDP-1")
                         nil
                       (if (string= (car b) "eDP-1")
                           t
                         (string< (car a) (car b)))))))
       ;; Configure monitors and create workspace mapping
       (setq exwm-randr-workspace-monitor-plist nil)
       (dotimes (i monitor-count)
         (let* ((monitor (nth i monitors))
                (name (car monitor))
                (geometry (cadr monitor))
                (width
                 (string-to-number (car (split-string geometry "x"))))
                (height
                 (string-to-number
                  (cadr (split-string geometry "x\\|+")))))
           (if (string= name "eDP-1")
               ;; Scale eDP-1 (1.5x scaling, adjust as needed)
               (progn
                 (start-process-shell-command
                  "xrandr" nil
                  (format
                   "xrandr --output %s --mode %s --pos %dx0 --scale 1.5x1.5 --auto"
                   name (car (split-string geometry "+")) x-position))
                 (setq width (* width 1.5)) ;; Adjust width for scaling
                 (setq height (* height 1.5)))
             ;; Other monitors without scaling
             (start-process-shell-command
              "xrandr" nil
              (format
               "xrandr --output %s --mode %s --pos %dx0 --auto"
               name (car (split-string geometry "+")) x-position)))
           (setq exwm-randr-workspace-monitor-plist
                 (plist-put
                  exwm-randr-workspace-monitor-plist i name))
           (setq x-position (+ x-position width))))
       ;; Set workspace number to match monitor count
       (setq exwm-workspace-number monitor-count)
       (message "Workspace count set to: %d" monitor-count) ;; Debug output
       ;; Remove excess workspaces
       (when (> (length exwm-workspace--list) monitor-count)
         (dolist (frame (nthcdr monitor-count exwm-workspace--list))
           (delete-frame frame)))
       ;; Ensure frames exist for each workspace and set geometry
       (dotimes (i monitor-count)
         (let* ((monitor (nth i monitors))
                (name (car monitor))
                (geometry (cadr monitor))
                (width
                 (string-to-number (car (split-string geometry "x"))))
                (height
                 (string-to-number
                  (cadr (split-string geometry "x\\|+"))))
                (x-pos
                 (string-to-number
                  (car
                   (split-string (cadr (split-string geometry "+"))
                                 "+")))))
           (unless (nth i exwm-workspace--list)
             (exwm-workspace--add-frame-as-workspace (make-frame)))
           (let ((frame (nth i exwm-workspace--list)))
             (set-frame-parameter frame 'exwm-randr-monitor name)
             (if (string= name "eDP-1")
                 (progn
                   (set-frame-width frame (floor (* width 1.5)) nil t) ;; Adjust for scaling
                   (set-frame-height frame (floor (* height 1.5))
                                     nil
                                     t))
               (set-frame-width frame width nil t)
               (set-frame-height frame height nil t))
             (set-frame-position frame x-pos 0)
             (message "Frame %d set for %s at %dx%d+%d+0"
                      i
                      name
                      width
                      height
                      x-pos))))
       (message "Workspace monitor plist: %s"
                exwm-randr-workspace-monitor-plist)) ;; Debug output
     ;; Autostart applications with 5-second delay
     (defun my-exwm-autostart ()
       "Start applications on EXWM initialization."
       (interactive)
       (run-at-time
        5 nil
        (lambda ()
          (start-process "udiskie" nil "udiskie"
                         "-as"
                         "2>/tmp/udiskie.log")))
       (run-at-time
        5 nil
        (lambda ()
          (start-process "blueman-applet" nil "blueman-applet")))
       (run-at-time
        5 nil
        (lambda () (start-process "nm-applet" nil "nm-applet")))
       (run-at-time
        5 nil
        (lambda () (start-process "mullvad-vpn" nil "mullvad-vpn"))))
     ;; System tray settings
     (setq exwm-systemtray-height 16)
     (exwm-systemtray-enable)
     ;; RandR settings
     (exwm-randr-enable)
     ;; Enable EXWM
     (exwm-enable)
     :hook
     ((exwm-update-class-hook
       .
       (lambda ()
         (exwm-workspace-rename-buffer
          (concat exwm-class-name ": " exwm-title))))
      (exwm-update-title-hook
       .
       (lambda ()
         (exwm-workspace-rename-buffer
          (concat exwm-class-name ": " exwm-title))))
      (exwm-init-hook
       .
       (lambda ()
         (my-exwm-autostart)
         (my-exwm-update-displays)))
      (exwm-manage-finish-hook
       .
       (lambda ()
         (when (and (boundp 'exwm-workspace-current-index)
                    (integerp exwm-workspace-current-index))
           (exwm-workspace-move-window
            exwm-workspace-current-index))))
      (exwm-randr-screen-change-hook
       .
       (lambda ()
         (start-process-shell-command "xrandr" nil "xrandr --auto")
         (my-exwm-update-displays)))))

   ;; EXWM Modeline
   (use-package
    exwm-modeline
    :ensure t
    :after exwm
    :config
    (setq-default mode-line-format
                  `("%e"
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
                    mode-line-format-right-align
                    ,(format-time-string "%Y-%m-%d %H:%M ")))
    :hook (exwm-init-hook . exwm-modeline-mode))

   ;; Counsel for app launcher
   (use-package counsel :ensure t :config (ivy-mode 1))))

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
 :init
 ;; Moved from Early Initial Settings
 (setq inhibit-startup-message t)
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
  password-cache-expiry nil
  auth-source-cache-expiry nil
  gc-cons-percentage 0.6
  truncate-string-ellipsis "…" ; Visual ellipsis for truncated lines
  scroll-margin 1
  garbage-collection-messages nil
  plstore-cache-directory "~/.config/emacs/"
  epg-gpg-program "gpg2"
  gc-cons-threshold most-positive-fixnum) ; From Garbage Collection
 (setenv "TZ" "America/New_York")
 (prefer-coding-system 'utf-8)
 (set-default-coding-systems 'utf-8)
 (set-terminal-coding-system 'utf-8)
 (set-keyboard-coding-system 'utf-8)
 (set-language-environment "UTF-8")
 (save-place-mode 1)
 (savehist-mode 1) ; Ensure history persistence is enabled
 (setq savehist-file "~/.config/emacs/savehist")
 (setq history-length 1000) ; Consistent with consult
 (setq history-delete-duplicates t)
 (setq savehist-save-minibuffer-history 1)
 (setq savehist-additional-variables
       '(kill-ring
         search-ring regexp-search-ring extended-command-history)) ; Add command history
 (require 'all-the-icons)
 :config
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
  savehist-file "~/.config/emacs/savehist"
  history-length t
  history-delete-duplicates t
  savehist-save-minibuffer-history 1
  savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
  undo-limit 800000
  isearch-lazy-count t
  lazy-count-prefix-format nil
  lazy-count-suffix-format "   (%s/%s)"
  save-place-file (expand-file-name ".saveplace" user-emacs-directory))
 (fset 'yes-or-no-p 'y-or-n-p)
 (require 'auth-source)
 (require 'epa-file)
 (epa-file-enable)

 ;; UI Settings
 (menu-bar-mode -1)
 (tool-bar-mode -1)
 (scroll-bar-mode -1)
 (set-face-attribute 'default nil :height 120)

 (set-face-attribute 'variable-pitch nil :height 130)
 (load-theme 'modus-vivendi t)
 (setq modus-themes-bold-constructs t)
 (setq modus-themes-italic-constructs t)

 (when (find-font (font-spec :name "Berkeley Mono"))
   (set-face-attribute 'default nil
                       :font "Berkeley Mono"
                       :height 140))
 (when (find-font (font-spec :name "Berkeley Mono Variable"))
   (set-face-attribute 'variable-pitch nil
                       :font "Berkeley Mono Variable"
                       :height 160))
 (set-face-attribute 'font-lock-comment-face nil
                     :slant 'italic
                     :weight 'light)
 (set-face-attribute 'font-lock-keyword-face nil :weight 'black)
 ;; Garbage Collection Functions
 (defun my-adjust-gc-threshold ()
   "Set a reasonable GC threshold after startup and adjust dynamically."
   (setq gc-cons-threshold (* 100 1024 1024)) ; 100 MB default
   (setq gc-cons-percentage 0.1))
 (defun my-increase-gc-during-minibuffer ()
   "Increase GC threshold while in minibuffer."
   (setq gc-cons-threshold most-positive-fixnum))
 (defun my-restore-gc-after-minibuffer ()
   "Restore GC threshold after exiting minibuffer."
   (setq gc-cons-threshold (* 100 1024 1024)))

 :hook
 ((text-mode . visual-wrap-prefix-mode)
  (before-save . whitespace-cleanup)
  (emacs-startup
   .
   (lambda ()
     (global-display-line-numbers-mode 1)
     (global-hl-line-mode 1)
     (pixel-scroll-precision-mode 1)
     (line-number-mode 1)
     (column-number-mode 1)
     (size-indication-mode 1)
     (global-auto-revert-mode 1)
     (display-time-mode 1)))
  (emacs-startup . my-adjust-gc-threshold)
  (minibuffer-setup . my-increase-gc-during-minibuffer)
  (minibuffer-exit . my-restore-gc-after-minibuffer))

 :bind
 (("C-x k" . kill-current-buffer)
  ("C-x K" . kill-buffer)
  ("H-s" . #'grim/screenshot)))

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
 (setq exec-path-from-shell-shell-name "/usr/bin/zsh") ;; Explicitly use Zsh
 (setq exec-path-from-shell-arguments '("-l")) ;; -l makes it a login shell, sourcing .zshrc
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
 :config
 (setq
  ediff-split-window-function 'split-window-right
  ediff-keep-variants nil)

 (set-face-foreground 'ediff-current-diff-A "red")
 (set-face-foreground 'ediff-fine-diff-A "red")
 (set-face-foreground 'ediff-current-diff-B "green")
 (set-face-foreground 'ediff-fine-diff-B "green")

 (set-face-foreground 'diff-added "green4")
 (set-face-foreground 'diff-removed "red3")

 (defvar my-ediff-window-config nil
   "Store window configuration before ediff.")
 (defun my-ediff-save-window-config ()
   "Save the current window configuration before starting ediff."
   (interactive)
   (setq my-ediff-window-config (current-window-configuration))
   (ediff-files
    (read-file-name "File A: ") (read-file-name "File B: ")))

 (defun my-ediff-quit ()
   "Quit ediff, discard changes, kill buffers, and restore window configuration."
   (interactive)
   (when (and (boundp 'ediff-control-buffer) ediff-control-buffer)
     (with-current-buffer ediff-control-buffer
       (ediff-quit t)) ;; Quit ediff, discard changes, kill buffers
     (when my-ediff-window-config
       (set-window-configuration my-ediff-window-config)
       (setq my-ediff-window-config nil))))

 :bind (("C-c d" . my-ediff-save-window-config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    tramp                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 tramp
 :ensure nil
 :config
 ;; Optimize TRAMP for auto-revert
 (setq tramp-auto-save-directory "~/.config/emacs/tramp-auto-save/")
 (setq tramp-verbose 1) ; Minimal verbosity
 (setq tramp-default-method "ssh") ; Stable connection method
 ;; Auto-revert settings for TRAMP
 (setq auto-revert-remote-files t) ; Enable reverting for remote files
 (setq auto-revert-interval 1) ; Poll every 1 second
 (setq auto-revert-verbose nil) ; Silence revert messages
 (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


(use-package
 files
 :ensure nil
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

;; Save place settings
(setq save-place-file
      (expand-file-name ".saveplace" user-emacs-directory))
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
  "Font-lock keywords for log-mode highlighting.")

;; Associate .log files with log-mode
(add-to-list 'auto-mode-alist '("\\.log\\'" . log-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    vundo                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 vundo
 :ensure t
 :bind ("C-x u" . vundo)
 :config (setq vundo-glyph-alist vundo-unicode-symbols))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 deadgrep                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 deadgrep
 :ensure t
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
 :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight Thing at Point
(use-package
 highlight-thing
 :ensure t
 :custom
 (highlight-thing-delay-seconds 0.5) ; Delay before highlighting
 (highlight-thing-what-thing 'symbol) ; Highlight symbols
 :config
 (set-face-attribute 'highlight-thing nil
                     :background "#5e81ac" ; Soft blue from Modus
                     :weight 'normal)
 :hook (prog-mode . highlight-thing-mode))

;; Indent bars
(use-package
 indent-bars
 :custom
 (indent-bars-no-descend-lists t)
 (indent-bars-treesit-support t)
 (indent-bars-prefer-character t)
 (indent-bars-treesit-scope
  '((python
     function_definition
     class_definition
     for_statement
     if_statement
     with_statement
     while_statement)))
 (indent-bars-color '(highlight :face-bg t :blend 0.15))
 (indent-bars-starting-column 0)
 (indent-bars-color-by-depth
  '(:regexp "outline-\\([0-9]+\\)" :blend 1))
 (indent-bars-highlight-current-depth '(:blend 0.5))
 (indent-bars-display-on-blank-lines t)
 :hook (prog-mode . indent-bars-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Mode Line Cleanup                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 delight
 :ensure t
 :config
 (delight
  '((global-hl-line-mode nil "hl-line")
    (save-place-mode nil "saveplace")
    (global-auto-revert-mode nil "autorevert")
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
 vertico
 :ensure t
 :init (vertico-mode 1)
 :custom
 (vertico-cycle t) ; Cycle through candidates
 (vertico-count 10) ; Show 10 candidates max, Prot’s default
 (vertico-sort-function 'vertico-sort-history-alpha) ; History then alpha
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
 consult
 :ensure t
 :after vertico
 :demand t
 :config
 (setq history-length 1000) ; Long history, per Prot
 (setq savehist-additional-variables
       (append
        savehist-additional-variables '(extended-command-history)))
 (savehist-mode 1) ; Persist history
 (setq consult-buffer-sources
       '(consult--source-buffer
         consult--source-file consult--source-bookmark
         consult--source-project-buffer)) ; Prot-style sources
 :bind
 (("C-c m" . consult-M-x) ; Your existing M-x replacement
  ("C-x b" . consult-buffer) ; Prot’s primary buffer switcher
  ("C-x 4 b" . consult-buffer-other-window) ; Open in other window
  ("C-x p b" . consult-project-buffer))) ; Project-specific buffers

(use-package
 orderless
 :ensure t
 :custom
 (completion-styles '(orderless basic)) ; Prot’s preferred styles
 (completion-category-defaults nil)
 (completion-category-overrides
  '((file (styles basic partial-completion)))))

(use-package marginalia :ensure t :init (marginalia-mode 1))

(use-package
 all-the-icons-completion
 :ensure t
 :after (all-the-icons marginalia)
 :config (all-the-icons-completion-mode)
 :hook
 (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package
 alert
 :ensure t
 :commands (alert)
 :init (setq alert-default-style 'notifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Editing Helpers                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 emacs
 :ensure nil
 :bind (("C-x C-;" . comment-or-uncomment-region)))

(use-package
 diff-hl
 :ensure t
 :hook (magit-post-refresh . diff-hl-magit-post-refresh)
 :config (global-diff-hl-mode +1))

(use-package
 which-key
 :ensure nil ; Built-in since Emacs 29, no need to ensure
 :config (setq which-key-idle-delay 0.1) (which-key-mode))

(use-package
 avy
 :ensure t
 :bind (("M-j" . avy-goto-char-timer) ("C-’" . avy-goto-char-2))
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
 :bind
 (("M-z" . avy-zap-up-to-char-dwim) ("M-Z" . avy-zap-to-char-dwim))
 :config
 (setq avy-zap-forward-only t)
 (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    XKCD                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 xkcd
 :ensure t
 :config
 ;; Optional: Customize cache directory
 (setq xkcd-cache-dir "~/.config/emacs/xkcd/")
 ;; Hook to set cursor-type to a non-blinking state in xkcd buffers
 :hook (xkcd-mode . (lambda () (setq-local cursor-type '(bar . 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 Popup Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 popup
 :config
 ;; If you want the popup library to compute columns more optimally:
 (setq popup-use-optimized-column-computation t)

 ;; Example: limit maximum width of a popup-tip
 (setq popup-tip-max-width 80))


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
 (setq ispell-extra-args '("--sug-mode=ultra")) ;; Fast suggestions, Prot’s style
 (setq ispell-personal-dictionary "~/.aspell.en.pws") ;; Personal words
 ;; Prot’s performance tweaks
 (setq flyspell-issue-message-flag nil) ;; No chatter
 (setq flyspell-issue-welcome-flag nil) ;; No welcome
 ;; Ensure aspell is installed
 (unless (executable-find "aspell")
   (message "Aspell not found; flyspell disabled")
   (flyspell-mode -1))
 :bind
 (:map
  flyspell-mode-map
  ("C-;" . flyspell-correct-wrapper))) ;; Prot’s correction key

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
   "Return a list of all Org files for agenda, including local, Beorg, and Denote notes."
   (delete-dups
    (append
     (directory-files "~/.org/" t "^[^b].*\\.org$" t)
     (directory-files "~/.org/" t "b-.*\\.org$" t)
     (directory-files "~/.org/notes/" t "\\.org$" t)
     (directory-files "/home/grim/Nextcloud/org/notes/"
                      t "\\.org$" t))))
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
 (define-key my-org-prefix-map (kbd "b") #'my-org-refile-to-beorg)
 (define-key my-org-prefix-map (kbd "O") #'org-download-clipboard)
 (define-key my-org-prefix-map (kbd "t") #'my-org-show-todo-list)
 (define-key my-org-prefix-map (kbd "n") #'my-org-capture-note-quick)
 (define-key my-org-prefix-map (kbd "d") #'my-denote-capture-note)
 (define-key my-org-prefix-map (kbd "s") #'my-sync-denote-to-beorg)
 (define-key my-org-prefix-map (kbd "i") #'my-denote-link-beorg-notes)

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
    "C-c o b"
    "refile-to-beorg"
    "C-c o d"
    "denote"
    "C-c o i"
    "index-beorg-notes"
    "C-c o s"
    "sync-to-beorg"
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

 (defun my-org-refile-to-beorg (target-file headline)
   "Refile current heading to TARGET-FILE under HEADLINE for Beorg sync."
   (interactive (list
                 (completing-read
                  "Target file: " '("b-calendar.org" "b-tasks.org"))
                 (read-string "Headline: ")))
   (org-refile
    nil nil
    (list
     headline (expand-file-name target-file org-directory) nil nil)))

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

 (defun my-sync-denote-to-beorg ()
   "Copy Denote notes to Beorg's notes directory for syncing."
   (interactive)
   (shell-command
    "rsync -av --exclude='.*' ~/.org/notes/ /home/grim/Nextcloud/org/notes/")
   (message "Denote notes synced to Beorg directory"))

 (defun my-denote-link-beorg-notes ()
   "Link all Beorg notes into a notes-index.org file."
   (interactive)
   (find-file (expand-file-name "notes-index.org" org-directory))
   (erase-buffer)
   (insert "#+TITLE: Notes Index\n\n")
   (denote-add-links "/home/grim/Nextcloud/org/notes/")
   (save-buffer))

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
         ("b" "Beorg Tasks"
          ((todo
            "TODO|NEXT|WAITING"
            ((org-agenda-overriding-header "Beorg Tasks")
             (org-agenda-files
              (list
               (expand-file-name "b-tasks.org" org-directory)))))))
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
    ("r" "Read Later (RSS)" entry
     (file+headline
      ,(expand-file-name "tasks.org" org-directory) "Read Later")
     "* TODO Read: %:description\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:LINK: %:link\n:END:\n"
     :immediate-finish t)
    ("e" "Email TODO" entry
     (file+headline
      ,(expand-file-name "tasks.org" org-directory) "Tasks")
     "* TODO %:subject\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:CREATED: %U\n:EMAIL_LINK: %:link\n:END:\n"
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
 :hook (org-mode . org-modern-mode)
 :config
 (setq org-modern-star '("◉" "○" "✸" "✿"))
 (setq org-modern-list '((43 . "•") (45 . "–") (42 . "◦")))
 (setq org-modern-table-vertical 2)
 (setq org-modern-table-horizontal 2))

(use-package
 org-auto-tangle
 :ensure t
 :hook (org-mode . org-auto-tangle-mode))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Magit/Forge                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 magit
 :ensure t
 :defer t
 :config (setq magit-prefer-remote-upstream t)
 :bind ("C-c G" . magit-status))

(use-package forge :ensure t :defer t :after magit)

(use-package magit-todos :ensure t :after magit)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Aidermacs (Anthropic)                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 aidermacs
 :ensure t
 :config
 (setq aidermacs-default-model "sonnet")
 (global-set-key (kbd "C-c A") 'aidermacs-transient-menu)
 (setq aidermacs-auto-commits t)
 ;(setq aidermacs-use-architect-mode t)
 ;(setq aidermacs-architect-model "sonnet")
 ;(setq aidermacs-editor-model "deepseek/deepseek-chat"))
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Misc Packages                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 recentf
 :ensure t
 :init (recentf-mode 1)
 :config
 (setq
  recentf-max-saved-items 100
  recentf-max-menu-items 25
  recentf-exclude '("/tmp/" "/ssh:"))
 :bind (("C-c r" . consult-recent-file)))

(use-package
 ssh-deploy
 :ensure t
 :demand t
 :after hydra
 :hook
 ((after-save . ssh-deploy-after-save)
  (find-file . ssh-deploy-find-file))
 :config
 (ssh-deploy-line-mode)
 (ssh-deploy-add-menu)
 (ssh-deploy-hydra "C-c C-z")
 (add-hook
  'ssh-deploy-after-save-hook
  (lambda ()
    (message "SSH Deploy: File %s synced" (buffer-file-name)))))

(use-package
 emacs-everywhere
 :ensure t
 :config
 (setq
  emacs-everywhere-app-info-function
  #'grim/emacs-everywhere-wayland-app-info
  emacs-everywhere-copy-command '("wl-copy")
  emacs-everywhere-paste-command '("wl-paste"))
 :hook (emacs-everywhere-init . whitespace-cleanup))

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
  ("M-!" . dired-smart-shell-command))
 :hook
 ((dired-mode . dired-hide-details-mode)
  (dired-mode . all-the-icons-dired-mode)
  (dired-mode . dired-preview-mode)
  (dired-mode . hl-line-mode))
 :custom
 (dired-listing-switches "-lah --group-directories-first")
 (dired-dwim-target t)
 (dired-recursive-copies 'always)
 (dired-recursive-deletes 'always)
 (dired-auto-revert-buffer t)
 (dired-hide-details-hide-symlink-targets nil)
 (dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open")))
 (dired-use-ls-dired t)
 :config
 (put 'dired-find-alternate-file 'disabled nil)
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
  :custom (dgi-auto-hide-details-p nil)
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
  :config (dired-async-mode 1))
 (defun dired-open-externally ()
   "Open file under cursor with xdg-open."
   (interactive)
   (let ((file (dired-get-file-for-visit)))
     (start-process "dired-open" nil "xdg-open" file)))
 (define-key dired-mode-map (kbd "C-c o") 'dired-open-externally)
 (defun dired-copy-file-path ()
   "Copy the full path of the file under cursor to kill ring."
   (interactive)
   (let ((path (dired-get-file-for-visit)))
     (kill-new path)
     (message "Copied path: %s" path)))
 (define-key dired-mode-map (kbd "C-c w") 'dired-copy-file-path)
 (defun dired-consult-filter ()
   "Filter Dired buffer using Consult narrowing."
   (interactive)
   (consult-focus-lines
    (lambda (file)
      (string-match-p
       (regexp-quote (consult--read "Filter: ")) file))))
 (define-key dired-mode-map (kbd "C-c f") 'dired-consult-filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     eww                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 eww
 :ensure nil
 :commands (eww eww-browse-url)
 :init
 (setq browse-url-handlers
       '(("\\.pdf\\'" . my-open-remote-pdf-in-emacs)
         ("^https?://" . eww-browse-url)))
 :config
 (setq eww-auto-rename-buffer 'title) ; Nicer buffer names
 :hook (eww-mode . (lambda () (display-line-numbers-mode -1))))

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
 (unless (featurep 'pdf-tools) ; Install only if not already loaded
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
  ("C-c n d" . denote-sort-dired)
  ("C-c n s" . denote-search)
  ;; If you intend to use Denote with a variety of file types, it is
  ;; easier to bind the link-related commands to the `global-map', as
  ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
  ;; `markdown-mode-map', and/or `text-mode-map'.
  ("C-c n l" . denote-link)
  ("C-c n L" . denote-add-links)
  ("C-c n b" . denote-backlinks)
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
 (setq denote-directory (expand-file-name "~/.org/notes"))
 (setq denote-save-buffers nil)
 (setq denote-known-keywords '("emacs" "research"))
 (setq denote-infer-keywords t)
 (setq denote-sort-keywords t)
 (setq denote-prompts '(title keywords))
 (setq denote-excluded-directories-regexp nil)
 (setq denote-excluded-keywords-regexp nil)
 (setq denote-rename-confirmations
       '(rewrite-front-matter modify-file-name))

 ;; Pick dates, where relevant, with Org's advanced interface:
 (setq denote-date-prompt-use-org-read-date t)

 ;; By default, we do not show the context of links.  We just display
 ;; file names.  This provides a more informative view.
 (setq denote-backlinks-show-context t)

 ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
 (denote-rename-buffer-mode 1))

(use-package
 denote-org
 :ensure t
 ;; :command
 ;; ;; I list the commands here so that you can discover them more
 ;; ;; easily.  You might want to bind the most frequently used ones to
 ;; ;; the `org-mode-map'.
 ;; ( denote-org-link-to-heading
 ;;   denote-org-backlinks-for-heading

 ;;   denote-org-extract-org-subtree

 ;;   denote-org-convert-links-to-file-type
 ;;   denote-org-convert-links-to-denote-type

 ;;   denote-org-dblock-insert-files
 ;;   denote-org-dblock-insert-links
 ;;   denote-org-dblock-insert-backlinks
 ;;   denote-org-dblock-insert-missing-links
 ;;   denote-org-dblock-insert-files-as-headings))
 )

(use-package
 denote-journal
 :ensure t
 ;; Bind those to some key for your convenience.
 ;; :commands ( denote-journal-new-entry
 ;;             denote-journal-new-or-existing-entry
 ;;             denote-journal-link-or-create-entry )
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
  erc-log-channels-directory "~/.config/emacs/irc-logs/"
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
  ("TAB" . completion-at-point)) ; Explicitly bind TAB
 :init
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
 :bind (:map global-map ("C-c E" . my-erc-connect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          Tree-sitter-based Modes                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (boundp 'treesit-language-source-alist) ;; Emacs 29+ check
  (use-package
   treesit-auto
   :ensure t
   :config
   (setq treesit-auto-install 'prompt) ; Prompt to install grammars
   (global-treesit-auto-mode) ; Auto-switch to Tree-sitter modes
   ;; Add custom recipes if needed (e.g., for org-mode)
   (add-to-list
    'treesit-auto-recipe-list
    (make-treesit-auto-recipe
     :lang 'org
     :ts-mode 'org-ts-mode
     :remap 'org-mode
     :url "https://github.com/emacs-tree-sitter/tree-sitter-org"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                So-long-mode                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package so-long :ensure nil :config (global-so-long-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Electric-quote-mode                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 electric
 :ensure nil
 :config
 (setq electric-quote-context-sensitive t)
 (electric-quote-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           calibre / nov.el                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 calibredb
 :ensure t
 :defer t
 :config
 (setq calibredb-format-all-the-icons t)
 (setq calibredb-format-character-icons t)
 (setq calibredb-root-dir "~/.calibre-library")
 (setq calibredb-db-dir
       (expand-file-name "metadata.db" calibredb-root-dir))
 (setq calibredb-library-alist
       '(("~/.calibre-library" (name . "Calibre")))))

(use-package esxml :ensure t)

(use-package
 nov
 :ensure t
 :after esxml
 :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
 :config
 (setq
  nov-unzip-program (executable-find "bsdtar")
  nov-unzip-args '("-xC" directory "-f" filename))
 (setq nov-verbose t))

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
 :config
 (advice-add
  'helpful-update
  :after #'elisp-demos-advice-helpful-update))

(use-package
 elisp-autofmt
 :ensure t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook
 ((emacs-lisp-mode . elisp-autofmt-mode)
  (before-save
   .
   (lambda ()
     (when (eq major-mode 'emacs-lisp-mode)
       (elisp-autofmt-buffer-background)))))
 :config
 (defun elisp-autofmt-buffer-background ()
   "Format buffer in background without moving cursor."
   (interactive)
   (save-excursion (elisp-autofmt-buffer)))
 :bind
 (:map
  emacs-lisp-mode-map ("C-c f" . elisp-autofmt-buffer-background)))

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
 :ensure nil
 :bind
 (("C-`" .
   (lambda ()
     (interactive)
     (my/toggle-buffer "*eshell*" 'eshell))))
 :init
 (setq
  eshell-scroll-to-bottom-on-input 'all
  eshell-error-if-no-glob t
  eshell-hist-ignoredups t
  eshell-save-history-on-exit t
  eshell-prefer-lisp-functions nil
  eshell-destroy-buffer-when-process-dies t
  eshell-visual-commands
  (if (boundp 'eshell-visual-commands)
      eshell-visual-commands
    '("less" "more")))
 :config
 (setq eshell-prompt-function
       (lambda () (concat (abbreviate-file-name (eshell/pwd)) " $ ")))
 (setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
 (defun eshell/clear ()
   "Clear the eshell buffer."
   (interactive)
   (let ((inhibit-read-only t))
     (erase-buffer)
     (eshell-send-input)))
 ;; Function to disable visual distractions in Eshell and subprocesses
 (defun my-eshell-disable-distractions ()
   "Disable line numbers and highlighting in Eshell and subprocess buffers."
   (display-line-numbers-mode -1)
   (hl-line-mode -1))
 :hook
 ((eshell-mode . my-eshell-disable-distractions)
  (eshell-mode
   .
   (lambda ()
     (add-to-list 'eshell-visual-commands "htop")
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (eshell/alias "ff" "find-file $1")
     (eshell/alias "ll" "ls -lh")
     (eshell/alias "clear" "eshell/clear")))
  ;; Hook for visual subprocesses
  (eshell-visual-subprocess-hook . my-eshell-disable-distractions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Gnus Setup                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 gnus
 :ensure nil
 :commands (gnus gnus-unplugged)
 :init
 ;; Primary select method for mailbox.org IMAP
 (setq gnus-select-method
       '(nnimap
         "mailbox.org"
         (nnimap-address "imap.mailbox.org")
         (nnimap-server-port 993)
         (nnimap-stream ssl)
         (nnimap-authenticator login)))

 ;; No secondary methods by default; RSS added manually with G R
 (setq gnus-secondary-select-methods nil)

 ;; SMTP configuration for sending mail
 (setq
  smtpmail-smtp-server "smtp.mailbox.org"
  smtpmail-smtp-service 587
  smtpmail-stream 'starttls
  smtpmail-smtp-user "theesfeld@mailbox.org"
  send-mail-function 'smtpmail-send-it
  message-send-mail-function 'smtpmail-send-it)

 ;; Use auth-source for credentials
 (setq
  auth-sources '("~/.authinfo.gpg")
  smtpmail-auth-credentials 'auth-source)

 ;; Custom keymap for Gnus commands
 (defvar my-gnus-map (make-sparse-keymap)
   "Keymap for Gnus commands.")
 (global-set-key (kbd "C-c g") my-gnus-map)
 (define-key my-gnus-map (kbd "g") 'gnus)
 (define-key my-gnus-map (kbd "u") 'gnus-unplugged)

 :hook
 ;; Enable topic mode if available
 (gnus-group-mode
  .
  (lambda ()
    (when (require 'gnus-topic nil t)
      (gnus-topic-mode)
      (message "Gnus topic mode enabled"))))

 ;; Initial setup on startup
 (gnus-started-hook
  .
  (lambda ()
    ;; Subscribe to all mailbox.org groups at level 2, except Junk at 6
    (dolist (group (gnus-group-list-all-groups 6))
      (when (string-match-p "^nnimap\\+mailbox\\.org:" group)
        (if (string-match-p "Junk$" group)
            (gnus-group-change-level group 6)
          (gnus-group-change-level group 2)
          (gnus-activate-group group t)
          (unless (gnus-group-subscribed-p group)
            (gnus-subscribe-group group)))))
    ;; Fetch new news
    (gnus-group-get-new-news)
    ;; Show all subscribed groups (up to level 5) in the main view
    (gnus-group-list-groups 5 t))) ; 5 includes all typical mail levels, t forces update

 :custom
 ;; Directory and file settings
 (gnus-home-directory "~/.config/emacs/gnus/")
 (gnus-startup-file "~/.config/emacs/gnus/.newsrc")
 (gnus-cache-directory "~/.config/emacs/gnus/cache/")
 (gnus-cache-active-file "~/.config/emacs/gnus/cache/active")
 (gnus-use-dribble-file nil)
 (gnus-always-read-dribble-file nil)

 ;; Performance and behavior
 (gnus-check-new-newsgroups 'always)
 (gnus-asynchronous t)
 (gnus-use-cache t)
 (gnus-use-header-prefetch t)

 ;; Display settings
 (gnus-use-full-window nil)
 (gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n")
 (gnus-group-sort-function
  '(gnus-group-sort-by-alphabet gnus-group-sort-by-rank))
 (gnus-summary-line-format "%U%R %B %s\n")
 (gnus-summary-display-arrow t)
 (gnus-generate-tree-function 'gnus-generate-horizontal-tree)
 (gnus-tree-minimize-window nil)

 ;; Threading
 (gnus-show-threads t)
 (gnus-fetch-old-headers nil)
 (gnus-build-sparse-threads 'some)
 (gnus-summary-thread-gathering-function
  'gnus-gather-threads-by-subject)
 (gnus-thread-sort-functions '(gnus-thread-sort-by-date))
 (gnus-sum-thread-tree-root "├▶")
 (gnus-sum-thread-tree-false-root " ◇ ")
 (gnus-sum-thread-tree-single-indent "")
 (gnus-sum-thread-tree-vertical "│")
 (gnus-sum-thread-tree-leaf-with-child "├▶")
 (gnus-sum-thread-tree-indent "  ")
 (gnus-sum-thread-tree-single-leaf "└▶")

 ;; Ensure all subscribed groups are visible by default
 (gnus-level-subscribed 5) ; Show groups up to level 5 in main view
 (gnus-level-unsubscribed 6) ; Hide unsubscribed groups (e.g., Junk) unless explicitly requested
 (gnus-level-zombie 7) ; Higher levels for zombie/dead groups

 :custom-face
 ;; Faces aligned with Modus Vivendi theme
 (gnus-group-mail-3 ((t (:foreground "#00bcff" :weight bold))))
 (gnus-group-mail-3-empty ((t (:foreground "#666699"))))
 (gnus-summary-normal-unread
  ((t (:foreground "#ffcc66" :weight bold))))
 (gnus-summary-normal-read ((t (:foreground "#a0a0a0"))))
 (gnus-header-name ((t (:foreground "#ff66ff" :weight bold))))
 (gnus-header-content ((t (:foreground "#88c0d0"))))

 :config
 ;; Ensure cache directory exists
 (make-directory gnus-cache-directory t)

 ;; Posting styles for different contexts
 (setq gnus-posting-styles
       '((".*"
          (address "theesfeld@mailbox.org")
          (signature "Best,\nWilliam"))
         ("nnimap\\+mailbox\\.org:INBOX/SAMHAIN.*"
          (address "grim@samhain.su")
          (signature "Regards,\nGrim"))
         ("nnimap\\+mailbox\\.org:INBOX/THEESFELD.*"
          (address "william@theesfeld.net")
          (signature "Cheers,\nTJ"))))

 ;; Group parameters
 (setq gnus-parameters
       '(("nnimap\\+mailbox\\.org:.*"
          (display . all)
          (visible . t)
          (level . 2))
         ("nnimap\\+mailbox\\.org:Junk"
          (display . nil)
          (visible . nil)
          (level . 6))
         ("nnrss:.*" (display . all) (visible . t) (level . 2))))

 ;; Behavioral tweaks
 (setq gnus-article-browse-delete-temp 'ask)
 (setq gnus-expert-user t)

 ;; Custom quit function
 (defun my-gnus-group-quit ()
   "Quit Gnus and switch to a valid buffer."
   (interactive)
   (gnus-group-quit)
   (if (get-buffer "*scratch*")
       (switch-to-buffer "*scratch*")
     (switch-to-buffer (other-buffer))))

 ;; RSS and email capture functions
 (defun my-gnus-capture-rss-to-org ()
   "Capture current Gnus RSS article as an Org TODO."
   (interactive)
   (when (eq major-mode 'gnus-article-mode)
     (let*
         ((url (gnus-article-get-url))
          (title (gnus-article-get-title))
          (org-capture-link
           (when (and url title)
             (format
              "org-protocol://capture?template=r&link=%s&description=%s"
              (url-encode-url url) (url-encode-url title)))))
       (if org-capture-link
           (progn
             (org-protocol-capture org-capture-link)
             (message "Captured RSS: %s" title))
         (message "No URL or title found in this article")))))

 (defun my-gnus-capture-email-to-org ()
   "Capture current Gnus email as an Org TODO."
   (interactive)
   (when (eq major-mode 'gnus-article-mode)
     (let* ((message-id (gnus-fetch-field "Message-ID"))
            (subject (gnus-fetch-field "Subject"))
            (email-link
             (when message-id
               (format "gnus:nnimap+mailbox.org:INBOX#%s"
                       (substring message-id 1 -1))))
            (org-capture-link
             (when (and email-link subject)
               (format
                "org-protocol://capture?template=e&link=%s&subject=%s"
                (url-encode-url email-link)
                (url-encode-url subject)))))
       (if org-capture-link
           (progn
             (org-protocol-capture org-capture-link)
             (message "Captured email: %s" subject))
         (message "No Message-ID or subject found in this email")))))

 ;; Helper functions for URL and title extraction
 (defun gnus-article-get-url ()
   "Extract URL from the current Gnus article."
   (save-excursion
     (goto-char (point-min))
     (when (re-search-forward "<a href=\"\\(http[^\"]+\\)\"" nil t)
       (match-string 1))))

 (defun gnus-article-get-title ()
   "Extract title from the current Gnus article."
   (save-excursion
     (goto-char (point-min))
     (when (re-search-forward "^Subject: \\(.*\\)$" nil t)
       (match-string 1))))

 :bind
 (:map gnus-group-mode-map ("q" . my-gnus-group-quit))
 (:map
  gnus-article-mode-map
  ("C-c r" . my-gnus-capture-rss-to-org)
  ("C-c e" . my-gnus-capture-email-to-org)))

(use-package
 gnus-art
 :ensure nil
 :after gnus
 :config
 (setq gnus-inhibit-images nil) ; Show images
 (setq gnus-blocked-images nil) ; Don’t block any images
 (setq gnus-article-mode-line-format "Gnus: %S")
 (setq gnus-visible-headers
       '("^From:"
         "^Subject:"
         "^To:"
         "^Cc:"
         "^Date:"
         "^Newsgroups:"
         "^Followup-To:"))
 (setq gnus-article-sort-functions '(gnus-article-sort-by-date)))

(use-package
 message
 :ensure nil
 :after gnus
 :config
 (setq message-citation-line-format "On %a, %b %d %Y, %N wrote:\n")
 (setq message-citation-line-function
       'message-insert-formatted-citation-line)
 (setq message-kill-buffer-on-exit t)
 (setq message-wide-reply-confirm-recipients t)
 (setq message-default-charset 'utf-8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   calc                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 calc
 :ensure nil ;; calc is built-in, no need to install
 :bind (("C-c c" . 'calc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Fix Emacsclient                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-after-make-frame-setup (&optional frame)
  "Initialize UI settings for new frames, including daemon clients."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p) ; Only for graphical frames
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1))))

;; Run setup for the initial frame if not in daemon mode
(unless (daemonp)
  (my-after-make-frame-setup))

;; Hook for new frames created by emacsclient
(add-hook 'after-make-frame-functions #'my-after-make-frame-setup)

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
;;                                Mastodon                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 mastodon
 :ensure t
 :config
 ;; Core settings from latest mastodon.el README
 (setq
  mastodon-instance-url "https://defcon.social"
  mastodon-active-user "blackdream@defcon.social"
  mastodon-client--media-directory (expand-file-name "mastodon-media" user-emacs-directory)
  mastodon-tl--enable-relative-timestamps t ;; Relative timestamps
  mastodon-tl--show-avatars t ;; Show avatars
  mastodon-tl--highlight-current-toot t ;; Highlight current toot
  mastodon-tl--display-media-p t ;; Enable image display
  mastodon-media--enable-image-cache t ;; Cache images
  mastodon-media--preview-max-height 300) ;; Reasonable image size

 ;; Load required libraries (per current source)
 (require 'mastodon-async) ;; For smoother timeline loading
 (require 'mastodon-media) ;; For image support
 (require 'mastodon-toot) ;; For toot composition

 ;; Enable inline images after timeline refresh
 (add-hook
  'mastodon-tl--buffer-refreshed-hook
  (lambda () (mastodon-media--inline-images (point-min) (point-max))))

 ;; Enhance toot composition buffer
 (add-hook
  'mastodon-toot-mode-hook
  (lambda ()
    (auto-fill-mode 1)
    (setq fill-column 500) ;; Mastodon’s character limit
    (visual-line-mode 1)))

 ;; Use default keybindings as defined in mastodon.el
 ;; No custom keymap here—relying on package defaults
 )

;; Which-key integration for visibility (optional but helpful)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c m" "mastodon-prefix"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 pgmacs Setup                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 pgmacs
 :vc (:url "https://github.com/emarsden/pgmacs" :rev :newest)
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
;;                                  SES (Spreadsheet)                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
 ses
 :ensure nil ; SES is built-in, no need to install
 :commands (ses-mode new-ses)
 :init
 ;; Pre-configure SES defaults before loading
 (setq ses-initial-size '(5 . 5)) ; Default 5 rows x 5 columns
 (setq ses-after-entry-functions '(ses-recalculate-cell)) ; Recalculate after entry
 (setq ses-initial-column-widths 10) ; Default column width

 :config
 ;; Core SES settings
 (setq ses-header-row 0) ; First row as header
 (setq ses-default-printer #'ses-default-printer) ; Default printer function
 (setq ses-mode-hook nil) ; Clear default hooks to customize below

 ;; Enhance readability and aesthetics
 (set-face-attribute
  'ses-header-row-face nil
  :background "#3b4252" ; Modus Vivendi dark blue-gray
  :foreground "#88c0d0" ; Light cyan for contrast
  :weight 'bold)
 (set-face-attribute 'ses-cell-face nil
                     :background "#2e3440" ; Darker background
                     :foreground "#d8dee9" ; Light foreground
                     :box '(:line-width 1 :color "#4c566a")) ; Subtle border
 (set-face-attribute 'ses-formula-face nil
                     :foreground "#f9e2af" ; Yellow for formulas
                     :slant 'italic)

 ;; Integrate with all-the-icons for buffer names
 (when (featurep 'all-the-icons)
   (defun my-ses-buffer-name ()
     "Rename SES buffers with an icon."
     (rename-buffer
      (concat
       (all-the-icons-faicon
        "table"
        :face '(:foreground "#81a1c1"))
       " " (buffer-name))))
   (add-hook 'ses-mode-hook #'my-ses-buffer-name))

 ;; Custom functions for usability
 (defun my-ses-new-spreadsheet ()
   "Create a new SES spreadsheet with a prompted filename."
   (interactive)
   (let ((file
          (read-file-name "New spreadsheet file: "
                          "~/.org/"
                          nil
                          nil
                          ".ses")))
     (find-file file)
     (unless (eq major-mode 'ses-mode)
       (new-ses ses-initial-size))))

 (defun my-ses-insert-sum-column ()
   "Insert a formula to sum the current column."
   (interactive)
   (ses-insert-formula
    (format "=SUM(%s%d:%s%d)"
            (ses-column-letter (ses-current-column)) 1
            (ses-column-letter
             (ses-current-column))
            (1- (ses-row-number)))))

 (defun my-ses-insert-sum-row ()
   "Insert a formula to sum the current row."
   (interactive)
   (ses-insert-formula
    (format "=SUM(%s%d:%s%d)"
            "A"
            (ses-row-number)
            (ses-column-letter (1- (ses-column-number)))
            (ses-row-number))))

 (defun my-ses-toggle-read-only ()
   "Toggle read-only mode for the current cell."
   (interactive)
   (let ((cell (ses-get-cell (ses-row-number) (ses-current-column))))
     (if (ses-cell-property :read-only cell)
         (ses-set-cell
          (ses-row-number)
          (ses-current-column)
          :read-only nil)
       (ses-set-cell
        (ses-row-number)
        (ses-current-column)
        :read-only t))
     (ses-recalculate-cell)
     (message "Cell %s%d read-only: %s"
              (ses-column-letter (ses-current-column))
              (ses-row-number)
              (if (ses-cell-property :read-only cell)
                  "off"
                "on"))))

 :bind
 (("C-c S n" . my-ses-new-spreadsheet) ; New spreadsheet
  ("C-c S c" . ses-insert-column) ; Insert column
  ("C-c S r" . ses-insert-row) ; Insert row
  ("C-c S s c" . my-ses-insert-sum-column) ; Sum column
  ("C-c S s r" . my-ses-insert-sum-row) ; Sum row
  ("C-c S t" . my-ses-toggle-read-only) ; Toggle read-only
  :map ses-mode-map
  ("C-c C-c" . ses-recalculate-all) ; Recalculate all
  ("C-c C-f" . ses-insert-formula) ; Insert formula
  ("C-c C-d" . ses-delete-column) ; Delete column
  ("C-c C-r" . ses-delete-row)) ; Delete row

 :config
 ;; Which-key integration
 (with-eval-after-load 'which-key
   (which-key-add-key-based-replacements
    "C-c S"
    "ses-spreadsheet"
    "C-c S n"
    "new-spreadsheet"
    "C-c S c"
    "insert-column"
    "C-c S r"
    "insert-row"
    "C-c S s c"
    "sum-column"
    "C-c S s r"
    "sum-row"
    "C-c S t"
    "toggle-read-only"))

 :hook
 ((ses-mode
   .
   (lambda ()
     (display-line-numbers-mode -1) ; Disable line numbers
     (hl-line-mode 1) ; Highlight current line
     (visual-line-mode -1) ; Ensure no wrapping
     (set-fringe-style '(8 . 8)) ; Consistent fringe width
     (buffer-face-mode 1) ; Apply buffer-wide face
     (set-face-attribute 'buffer-face-mode-face nil
                         :family "Berkeley Mono" ; Match your font
                         :height 120)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Final Cleanup                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
;;; init.el ends here
