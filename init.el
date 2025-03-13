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
;;                                  MELPA                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Force a refresh and install missing packages
(when (not package-archive-contents)
  (package-refresh-contents))
;; Install all packages marked with :ensure t
(dolist (pkg package-selected-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                CUSTOM FUNCTIONS                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun my-elfeed-start-auto-update ()
  "Start auto-updating Elfeed every 30 minutes if search buffer is visible."
  (interactive)
  (unless (bound-and-true-p my-elfeed-update-timer)
    (setq my-elfeed-update-timer
          (run-at-time t 1800
                       (lambda ()
                         (when (get-buffer-window "*elfeed-search*" t)
                           (elfeed-update)
                           (my-elfeed-update-title)))))))

(defun my-elfeed-stop-auto-update ()
  "Stop Elfeed auto-update timer."
  (interactive)
  (when (bound-and-true-p my-elfeed-update-timer)
    (cancel-timer my-elfeed-update-timer)
    (setq my-elfeed-update-timer nil)))

(defun my-elfeed-update-title ()
  "Update Elfeed search buffer title with new article count."
  (interactive)
  (with-current-buffer "*elfeed-search*"
    (let ((new-count (length (elfeed-search-filter "unread"))))
      (rename-buffer (format "*elfeed-search* (%d new)" new-count)))))

(defun my-elfeed-quit-and-stop-timer ()
  "Quit Elfeed and stop the auto-update timer."
  (interactive)
  (elfeed-search-quit-window)
  (my-elfeed-stop-auto-update))

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
    (cond
     ((executable-find "wl-copy")  ; Wayland
      (with-temp-buffer
        (insert-file-contents filename)
        (call-process-region (point-min) (point-max)
                             "wl-copy" nil nil nil "-t"
                             (format "image/%s" (substring extension 1)))))
     ((executable-find "xclip")  ; X11 fallback
      (with-temp-buffer
        (insert-file-contents filename)
        (call-process-region (point-min) (point-max)
                             "xclip" nil nil nil "-selection" "clipboard" "-t"
                             (format "image/%s" (substring extension 1)))))
     (t (message "No clipboard tool found (wl-copy/xclip)")))
    (set-register ?s filename)
    (when (or (executable-find "wl-copy") (executable-find "xclip"))
      (alert (format "Screenshot (%s) copied to clipboard and saved to %s" type filename)
             :title "Screenshot Taken"
             :severity 'normal))))

(defun my-org-capture-delete-file-after-kill (&rest _)
  "Delete file if capture is aborted."
  (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (delete-file (buffer-file-name))
    (message "Deleted aborted capture file: %s" (buffer-file-name))))

(defun my-project-find-root (dir)
  "Identify project roots in ~/Code and ~/.config/."
  (let ((roots '("~/Code" "~/.config")))
    (when (member (file-truename (expand-file-name dir))
                  (mapcar #'file-truename (mapcar #'expand-file-name roots)))
      (cons 'transient dir))))

(declare-function pcomplete-erc-setup "erc-pcomplete")
(declare-function completion-preview-insert "completion-preview")
(declare-function completion-preview-next-candidate "completion-preview")
(declare-function completion-preview-prev-candidate "completion-preview")
(declare-function completion-preview--hide "completion-preview")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;                                     EXWM                                  ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (when (eq window-system 'x)
;;   ;; Load required EXWM modules first to ensure symbols are defined
;;   (require 'exwm)
;;   (require 'exwm-randr)
;;   (require 'exwm-systemtray)

;;   ;; Basic EXWM settings
;;   (setq exwm-workspace-show-all-buffers t
;;         exwm-layout-show-all-buffers t
;;         exwm-manage-force-tiling t
;;         mouse-autoselect-window t
;;         focus-follows-mouse t)

;;   ;; Natural scrolling attempt
;;   (setq mouse-wheel-scroll-amount '(5 ((shift) . 1)))  ;; Positive for natural scroll
;;   (setq mouse-wheel-progressive-speed t)               ;; Progressive speed enabled

;;   ;; Clipboard integration with X11
;;   (setq x-select-enable-clipboard t)
;;   (setq x-select-enable-primary t)
;;   (setq select-enable-clipboard t)
;;   (global-set-key (kbd "M-w") 'kill-ring-save)  ;; M-w copies globally
;;   (global-set-key (kbd "C-y") 'yank)            ;; C-y pastes globally

;;   ;; Mouse cursor size fix
;;   (setq xterm-set-cursor-size nil)  ;; Prevent EXWM from overriding cursor size
;;   (start-process-shell-command "xsetroot" nil "xsetroot -cursor_name left_ptr")  ;; Set standard cursor

;;   ;; Keybinding setup
;;   (setq exwm-input-prefix-keys
;;         '(?\C-x ?\C-u ?\C-h ?\M-x ?\M-& ?\M-: ?\C-\M-j ?\C-\ ))

;;   ;; Global keybindings
;;   (setq exwm-input-global-keys
;;         `(([?\s-r] . exwm-reset)
;;           ([?\s-w] . exwm-workspace-switch)
;;           ([?\s-&] . (lambda (cmd)
;;                        (interactive (list (read-shell-command "$ ")))
;;                        (start-process-shell-command cmd nil cmd)))
;;           ([?\s-x] . (lambda () (interactive) (save-buffers-kill-emacs)))  ;; Improved exit
;;           ([?\s-e] . (lambda ()
;;                        (interactive)
;;                        (start-process-shell-command "yazi" nil "footclient -e yazi")))
;;           ([?\s-\ ] . (lambda ()
;;                         (interactive)
;;                         (require 'counsel)
;;                         (ivy-mode 1)  ;; Ensure Ivy is active for minibuffer
;;                         (counsel-linux-app)))
;;           ([?\s-v] . consult-yank-pop)
;;           ([?\s-q] . (lambda ()
;;                           (interactive)
;;                             (kill-buffer-and-window)))  ;; s-Q to kill current window
;;           ;; Media keys with volume/brightness popups
;;           ([XF86AudioRaiseVolume] . (lambda ()
;;                                       (interactive)
;;                                       (start-process-shell-command "pactl-up" nil "pactl set-sink-volume @DEFAULT_SINK@ +5%")
;;                                       (message "Volume: %s" (shell-command-to-string "pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n1"))))
;;           ([XF86AudioLowerVolume] . (lambda ()
;;                                       (interactive)
;;                                       (start-process-shell-command "pactl-down" nil "pactl set-sink-volume @DEFAULT_SINK@ -5%")
;;                                       (message "Volume: %s" (shell-command-to-string "pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n1"))))
;;           ([XF86AudioMute] . (lambda ()
;;                                (interactive)
;;                                (start-process-shell-command "pactl-mute" nil "pactl set-sink-mute @DEFAULT_SINK@ toggle")
;;                                (message "Volume: %s" (if (string-match "yes" (shell-command-to-string "pactl get-sink-mute @DEFAULT_SINK@"))
;;                                                        "Muted"
;;                                                      (shell-command-to-string "pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n1")))))
;;           ([XF86AudioMicMute] . (lambda ()
;;                                   (interactive)
;;                                   (start-process-shell-command "pactl-mic-mute" nil "pactl set-source-mute @DEFAULT_SOURCE@ toggle")))
;;           ([XF86MonBrightnessUp] . (lambda ()
;;                                      (interactive)
;;                                      (start-process-shell-command "brightnessctl-up" nil "brightnessctl set +10%")
;;                                      (message "Brightness: %s" (shell-command-to-string "brightnessctl -m | cut -d, -f4"))))
;;           ([XF86MonBrightnessDown] . (lambda ()
;;                                        (interactive)
;;                                        (start-process-shell-command "brightnessctl-down" nil "brightnessctl set 10%-")
;;                                        (message "Brightness: %s" (shell-command-to-string "brightnessctl -m | cut -d, -f4"))))
;;           ([XF86AudioPlay] . (lambda ()
;;                                (interactive)
;;                                (start-process-shell-command "playerctl-play" nil "playerctl play-pause")))
;;           ([XF86AudioPause] . (lambda ()
;;                                 (interactive)
;;                                 (start-process-shell-command "playerctl-pause" nil "playerctl play-pause")))
;;           ([XF86AudioNext] . (lambda ()
;;                                (interactive)
;;                                (start-process-shell-command "playerctl-next" nil "playerctl next")))
;;           ([XF86AudioPrev] . (lambda ()
;;                                (interactive)
;;                                (start-process-shell-command "playerctl-prev" nil "playerctl previous")))

;;           ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
;;           ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-create 2)))
;;           ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-create 3)))
;;           ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-create 4)))
;;           ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-create 5)))
;;           ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch-create 6)))
;;           ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch-create 7)))
;;           ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch-create 8)))
;;           ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch-create 9)))
;;           ([?\s-0] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
;;           ([?\M-\s-1] . (lambda () (interactive) (exwm-workspace-move-window 1)))
;;           ([?\M-\s-2] . (lambda () (interactive) (exwm-workspace-move-window 2)))
;;           ([?\M-\s-3] . (lambda () (interactive) (exwm-workspace-move-window 3)))
;;           ([?\M-\s-4] . (lambda () (interactive) (exwm-workspace-move-window 4)))
;;           ([?\M-\s-5] . (lambda () (interactive) (exwm-workspace-move-window 5)))
;;           ([?\M-\s-6] . (lambda () (interactive) (exwm-workspace-move-window 6)))
;;           ([?\M-\s-7] . (lambda () (interactive) (exwm-workspace-move-window 7)))
;;           ([?\M-\s-8] . (lambda () (interactive) (exwm-workspace-move-window 8)))
;;           ([?\M-\s-9] . (lambda () (interactive) (exwm-workspace-move-window 9)))
;;           ([?\M-\s-0] . (lambda () (interactive) (exwm-workspace-move-window 0)))
;;           ([s tab] . next-buffer)))

;;           ;; ([?\s-1] . exwm-workspace-switch-create 1)
;;           ;; ([?\s-2] . exwm-workspace-switch-create 2)
;;           ;; ([?\s-3] . exwm-workspace-switch-create 3)
;;           ;; ([?\s-4] . exwm-workspace-switch-create 4)
;;           ;; ([?\s-5] . exwm-workspace-switch-create 5)
;;           ;; ([?\s-6] . exwm-workspace-switch-create 6)
;;           ;; ([?\s-7] . exwm-workspace-switch-create 7)
;;           ;; ([?\s-8] . exwm-workspace-switch-create 8)
;;           ;; ([?\s-9] . exwm-workspace-switch-create 9)
;;           ;; ([?\s-0] . exwm-workspace-switch-create 0)
;;           ;; ([?\S-\s-1] . exwm-workspace-move-window 1)
;;           ;; ([?\S-\s-2] . exwm-workspace-move-window 2)
;;           ;; ([?\S-\s-3] . exwm-workspace-move-window 3)
;;           ;; ([?\S-\s-4] . exwm-workspace-move-window 4)
;;           ;; ([?\S-\s-5] . exwm-workspace-move-window 5)
;;           ;; ([?\S-\s-6] . exwm-workspace-move-window 6)
;;           ;; ([?\S-\s-7] . exwm-workspace-move-window 7)
;;           ;; ([?\S-\s-8] . exwm-workspace-move-window 8)
;;           ;; ([?\S-\s-9] . exwm-workspace-move-window 9)
;;           ;; ([?\S-\s-0] . exwm-workspace-move-window 0)

;;   ;; Simulation keys for passing common Emacs bindings to applications
;;   (setq exwm-input-simulation-keys
;;         '(([?\C-b] . [left])
;;           ([?\C-f] . [right])
;;           ([?\C-p] . [up])
;;           ([?\C-n] . [down])
;;           ([?\C-a] . [home])
;;           ([?\C-e] . [end])
;;           ([?\M-v] . [prior])
;;           ([?\C-v] . [next])
;;           ([?\C-d] . [delete])
;;           ([?\C-k] . [S-end delete])
;;           ([?\M-w] . [?\C-c])  ;; Map M-w to Ctrl+C for X apps
;;           ([?\C-y] . [?\C-v])))  ;; Map C-y to Ctrl+V for X apps

;;   ;; Multi-monitor setup with dynamic workspace count
;;   (defun my-exwm-update-displays ()
;;     "Update EXWM workspaces and frames based on connected monitors with eDP-1 on far right."
;;     (interactive)
;;     (let* ((xrandr-output (shell-command-to-string "xrandr --current"))
;;            (monitors (cl-loop for line in (split-string xrandr-output "\n")
;;                             when (string-match "\\(.*\\) connected.*\\([0-9]+x[0-9]+\\+[-0-9]+\\+[-0-9]+\\)" line)
;;                             collect (list (match-string 1 line)
;;                                         (match-string 2 line))))
;;            (monitor-count (length monitors))
;;            (x-position 0))
;;       ;; Sort monitors with eDP-1 last
;;       (setq monitors (sort monitors
;;                           (lambda (a b)
;;                             (if (string= (car a) "eDP-1")
;;                                 nil
;;                               (if (string= (car b) "eDP-1")
;;                                   t
;;                                 (string< (car a) (car b)))))))
;;       ;; Configure monitors and create workspace mapping
;;       (setq exwm-randr-workspace-monitor-plist nil)
;;       (dotimes (i monitor-count)
;;         (let* ((monitor (nth i monitors))
;;                (name (car monitor))
;;                (geometry (cadr monitor))
;;                (width (string-to-number (car (split-string geometry "x")))))
;;           (start-process-shell-command "xrandr" nil
;;                                      (format "xrandr --output %s --mode %s --pos %dx0 --auto"
;;                                             name
;;                                             (car (split-string geometry "+"))
;;                                             x-position))
;;           (setq exwm-randr-workspace-monitor-plist
;;                 (plist-put exwm-randr-workspace-monitor-plist i name))
;;           (setq x-position (+ x-position width))))
;;       ;; Set workspace number to match monitor count
;;       (setq exwm-workspace-number monitor-count)
;;       ;; Remove excess workspaces
;;       (when (> (length exwm-workspace--list) monitor-count)
;;         (dolist (frame (nthcdr monitor-count exwm-workspace--list))
;;           (delete-frame frame)))
;;       ;; Ensure frames exist for each workspace
;;       (dotimes (i exwm-workspace-number)
;;         (unless (nth i exwm-workspace--list)
;;           (exwm-workspace--add-frame-as-workspace (make-frame)))
;;         (let ((frame (nth i exwm-workspace--list)))
;;           (set-frame-parameter frame 'exwm-randr-monitor
;;                               (plist-get exwm-randr-workspace-monitor-plist i))
;;           (exwm-workspace-switch i)
;;           (set-frame-parameter frame 'fullscreen 'fullboth)))))

;;   ;; Autostart applications with 5-second delay
;;   (defun my-exwm-autostart ()
;;     "Start applications on EXWM initialization."
;;     (interactive)
;;     (run-at-time 5 nil (lambda ()
;;                          (start-process "udiskie" nil "udiskie" "-as" "2>/tmp/udiskie.log")))
;;     (run-at-time 5 nil (lambda ()
;;                          (start-process "blueman-applet" nil "blueman-applet")))
;;     (run-at-time 5 nil (lambda ()
;;                          (start-process "nm-applet" nil "nm-applet")))
;;     (run-at-time 5 nil (lambda ()
;;                          (start-process "mullvad-vpn" nil "mullvad-vpn"))))


;;   ;; MODELINE
;;   (use-package exwm-modeline
;;     :ensure t
;;     :after (exwm)
;;     :init (add-hook 'exwm-init-hook #'exwm-modeline-mode))

;;   (setq exwm-systemtray-height 16)
;;   (setq-default mode-line-format
;;                 `("%e" mode-line-front-space
;;                   mode-line-mule-info mode-line-client mode-line-modified
;;                   mode-line-remote mode-line-frame-identification
;;                   mode-line-buffer-identification "   "
;;                   mode-line-position
;;                   (vc-mode vc-mode) "  "
;;                   mode-line-modes
;;                   mode-line-format-right-align

;;                   ,(format-time-string "%Y-%m-%d %H:%M ")))

;;   ;; Better buffer naming for X applications
;;   (add-hook 'exwm-update-class-hook
;;             (lambda ()
;;               (exwm-workspace-rename-buffer (concat exwm-class-name ": " exwm-title))))
;;   (add-hook 'exwm-update-title-hook
;;             (lambda ()
;;               (exwm-workspace-rename-buffer (concat exwm-class-name ": " exwm-title))))

;;   ;; Hooks
;;   (add-hook 'exwm-randr-screen-change-hook
;;             (lambda ()
;;               (start-process-shell-command "xrandr" nil "xrandr --auto")
;;               (my-exwm-update-displays)))

;;   (add-hook 'exwm-init-hook
;;             (lambda ()
;;               (my-exwm-autostart)
;;               (my-exwm-update-displays)
;;               (set-frame-parameter nil 'fullscreen 'fullboth)))

;;   ;; Ensure new applications open on current workspace
;;   (add-hook 'exwm-manage-finish-hook
;;             (lambda ()
;;               (when (and (boundp 'exwm-workspace-current-index)
;;                          (integerp exwm-workspace-current-index))
;;                 (exwm-workspace-move-window exwm-workspace-current-index))))

;;   ;; Enable EXWM components with updated syntax
;;   (exwm-systemtray-mode 1)
;;   (exwm-randr-mode 1)
;;   (exwm-enable))

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
;;                                     EMACS                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :ensure nil  ; Built-in, no need to install
  :init
  ;; Basic Emacs Information and pre-load settings
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
        truncate-string-ellipsis "…"  ; Visual ellipsis for truncated lines
        scroll-margin 1
        garbage-collection-messages nil
        plstore-cache-directory "~/.config/emacs/"
        epg-gpg-program "gpg2"
        gc-cons-threshold most-positive-fixnum)  ; From Garbage Collection
  (setenv "TZ" "America/New_York")
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (save-place-mode 1)

  :config
  ;; Global Emacs Settings
  (setq-default default-directory '~
                kill-ring-max 5000
                indent-tabs-mode nil)  ; Use spaces instead of tabs
  (setq tab-always-indent 'complete
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
  ;; UI Settings
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-face-attribute 'default nil :height 120)
  (set-face-attribute 'variable-pitch nil :height 130)
  (load-theme 'modus-vivendi t)
  (when (find-font (font-spec :name "Berkeley Mono"))
    (set-face-attribute 'default nil :font "Berkeley Mono" :height 130))
  (when (find-font (font-spec :name "Berkeley Mono Variable"))
    (set-face-attribute 'variable-pitch nil :font "Berkeley Mono Variable" :height 140))
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic
                      :weight 'light)
  (set-face-attribute 'font-lock-keyword-face nil
                      :weight 'black)
  ;; Garbage Collection Functions
  (defun my-adjust-gc-threshold ()
    "Set a reasonable GC threshold after startup and adjust dynamically."
    (setq gc-cons-threshold (* 100 1024 1024))  ; 100 MB default
    (setq gc-cons-percentage 0.1))
  (defun my-increase-gc-during-minibuffer ()
    "Increase GC threshold while in minibuffer."
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my-restore-gc-after-minibuffer ()
    "Restore GC threshold after exiting minibuffer."
    (setq gc-cons-threshold (* 100 1024 1024)))

  :hook
  ;; Mode activations and hooks
  ((text-mode . visual-wrap-prefix-mode)
   (before-save . whitespace-cleanup)
   (emacs-startup . (lambda ()
                      (global-display-line-numbers-mode 1)
                      (global-hl-line-mode 1)
                      (pixel-scroll-precision-mode 1)
                      (line-number-mode 1)
                      (column-number-mode 1)
                      (size-indication-mode 1)
                      (global-auto-revert-mode 1)
                      (display-time-mode 1)))
   (emacs-startup . my-adjust-gc-threshold)  ; From Garbage Collection
   (minibuffer-setup . my-increase-gc-during-minibuffer)  ; From Garbage Collection
   (minibuffer-exit . my-restore-gc-after-minibuffer))  ; From Garbage Collection

  :bind
  (("C-x k" . kill-current-buffer)
   ("C-x K" . kill-buffer)
   ("s-s" . #'grim/screenshot)))

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
  (message "exec-path-from-shell ran with shell: %s" exec-path-from-shell-shell-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   ediff                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ediff
  :ensure nil
  :config
  (setq ediff-split-window-function 'split-window-right
        ediff-keep-variants nil)

  (set-face-foreground 'ediff-current-diff-A "red")
  (set-face-foreground 'ediff-fine-diff-A "red")
  (set-face-foreground 'ediff-current-diff-B "green")
  (set-face-foreground 'ediff-fine-diff-B "green")

  (set-face-foreground 'diff-added "green4")
  (set-face-foreground 'diff-removed "red3")

  (defvar my-ediff-window-config nil "Store window configuration before ediff.")
  (defun my-ediff-save-window-config ()
    "Save the current window configuration before starting ediff."
    (interactive)
    (setq my-ediff-window-config (current-window-configuration))
    (ediff-files (read-file-name "File A: ") (read-file-name "File B: ")))

  (defun my-ediff-quit ()
    "Quit ediff, discard changes, kill buffers, and restore window configuration."
    (interactive)
    (when (and (boundp 'ediff-control-buffer) ediff-control-buffer)
      (with-current-buffer ediff-control-buffer
        (ediff-quit t))  ;; Quit ediff, discard changes, kill buffers
      (when my-ediff-window-config
        (set-window-configuration my-ediff-window-config)
        (setq my-ediff-window-config nil))))

  :bind (("C-c d" . my-ediff-save-window-config)
         :map ediff-mode-map
         ("q" . my-ediff-quit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Backup and Auto-Save                         ;;
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
;;                                 deadgrep                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package deadgrep
  :ensure t
  :bind (("C-c s" . deadgrep)
         :map deadgrep-mode-map
         ("q" . deadgrep-kill-all-buffers))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Visual Enhancements                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight Thing at Point
(use-package highlight-thing
  :ensure t
  :custom
  (highlight-thing-delay-seconds 0.5)  ; Delay before highlighting
  (highlight-thing-what-thing 'symbol) ; Highlight symbols
  :config
  (set-face-attribute 'highlight-thing nil
                      :background "#5e81ac"  ; Soft blue from Modus
                      :foreground nil
                      :weight 'normal)
  :hook (prog-mode . highlight-thing-mode))

;; Beacon - Flash cursor position on jumps
(use-package beacon
  :ensure t
  :custom
  ;;(beacon-color "#bf616a")  ; Reddish from Modus for flash
  (beacon-blink-when-point-moves-vertically 1)  ; Flash on vertical movement
  (beacon-blink-duration 0.3)  ; Short flash
  :config
  (beacon-mode 1))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Mode Line Cleanup                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package delight
  :ensure t
  :config
  (delight '((global-hl-line-mode nil "hl-line")
             (save-place-mode nil "saveplace")
             (global-auto-revert-mode nil "autorevert")
             (flyspell-mode " ✍" "flyspell")
             (which-key-mode nil "which-key")
             (yas-minor-mode nil "yasnippet")
             (smartparens-mode nil "smartparens"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Completion Setup                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Completion-Preview with Escape to dismiss
(add-hook 'prog-mode-hook #'completion-preview-mode)
(add-hook 'text-mode-hook #'completion-preview-mode)
(with-eval-after-load 'comint
  (add-hook 'comint-mode-hook #'completion-preview-mode))
(with-eval-after-load 'completion-preview
  (keymap-set completion-preview-active-mode-map "<escape>" #'completion-preview-dismiss)
  (keymap-set completion-preview-active-mode-map "<tab>" #'completion-preview-insert)
  (keymap-set completion-preview-active-mode-map "<down>" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "<up>" #'completion-preview-prev-candidate)
  (setq completion-preview-minimum-symbol-length 3)
  (setq completion-preview-exact-match-only t)
  (push 'org-self-insert-command completion-preview-commands)
  (push 'paredit-backward-delete completion-preview-commands))
(defun completion-preview-dismiss ()
  "Dismiss the current completion preview."
  (interactive)
  (when completion-preview--overlay
    (completion-preview--hide)))

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
         ("Firefox" (and (mode . exwm-mode)  ;; EXWM-managed windows
                        (predicate . (string-match-p "firefox" (downcase (or (buffer-local-value 'exwm-class-name (current-buffer)) ""))))))
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
;;                                    helm                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helm - Bare Minimum for Aidermacs
(use-package helm
  :ensure t
  :defer t  ; Load only when aidermacs needs it
  :init
  (setq helm-mode nil)  ; Disable Helm globally
  :config
  ;; No bindings, no interference
  (global-unset-key (kbd "C-x C-f"))  ; Ensure Vertico owns this
  (global-unset-key (kbd "M-x"))
  (global-unset-key (kbd "C-x b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Vertico + Consult                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smartparens with custom bindings
(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode)
         (markdown-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)    ; Jump to next sexp
              ("C-M-b" . sp-backward-sexp)   ; Jump to prev sexp
              ("C-M-u" . sp-backward-up-sexp))) ; Up a level

;; Vertico with posframe for GUI flair
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-posframe
  :ensure t
  :after vertico
  :if (display-graphic-p)  ; Only in GUI
  :config
  (vertico-posframe-mode 1)
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)
          (background-color . "#2e3440"))))  ; Dark base from modus-vivendi

;; Persist minibuffer history
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;; Consult with extra commands
(use-package consult
  :ensure t
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
         ("M-g f"     . consult-flymake)         ; New: Flymake errors
         ("M-g g"     . consult-goto-line)
         ("M-g M-g"   . consult-goto-line)
         ("M-g o"     . consult-outline)
         ("M-g h"     . consult-org-heading)     ; New: Org headings
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
         ;; New: s-<tab> for buffer scrolling
         ("s-<tab>"   . my-consult-buffer-scroll)
         :map isearch-mode-map
         ("M-e"       . consult-isearch-history)
         ("M-s e"     . consult-isearch-history)
         ("M-s l"     . consult-line))
  :config
  (consult-customize
   consult-buffer
   :sort t
   :history 'buffer-name-history)

  ;; Custom function for s-<tab> buffer scrolling with posframe
  (defun my-consult-buffer-scroll ()
    "Scroll through open buffers in a posframe popup, styled like Dired preview."
    (interactive)
    (let ((vertico-posframe-parameters
           `((left-fringe . 8)
             (right-fringe . 8)
             (background-color . "#2e3440") ; Match your vertico-posframe
             (min-width . 50)
             (min-height . 10)))
          (vertico-posframe-show-delay 0.3)) ; Match dired-preview delay
      (consult-buffer)))

  ;; Enhance buffer display with Nerd Icons
  (with-eval-after-load 'nerd-icons
    (defun my-consult-buffer-format (buffer)
      "Add Nerd Icon to BUFFER name for consult-buffer."
      (let ((icon (nerd-icons-icon-for-buffer buffer)))
        (concat icon " " (buffer-name buffer))))
    (advice-add 'consult-buffer :filter-return
                (lambda (buffers)
                  (mapcar #'my-consult-buffer-format buffers))))

  ;; Add s-<tab> and S-s-<tab> cycling in Vertico popup
  (with-eval-after-load 'vertico
    (bind-key "s-<tab>" 'vertico-next vertico-map)
    (bind-key "S-s-<tab>" 'vertico-previous vertico-map)))

;; Embark with Avy integration
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (defun embark-avy-jump (pt)
    "Jump to PT with avy and trigger embark-act."
    (save-excursion
      (goto-char pt)
      (embark-act)))
  (add-to-list 'embark-target-finders 'avy--generic-jump))

(use-package embark-consult
  :ensure t
  :after (embark consult))

;; Marginalia
(use-package marginalia
  :init
  (marginalia-mode))

;; Nerd Icons support
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  (nerd-icons-scale-factor 1.1))  ; Slight upscale for visibility

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Alerts
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Editing Helpers                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-format Emacs Lisp files on save
(defun my-elisp-format-buffer (&optional quiet)
  "Format the current Emacs Lisp buffer if syntactically valid.
If QUIET is non-nil, suppress messages."
  (interactive)
  (when (derived-mode-p 'emacs-lisp-mode)  ; Covers emacs-lisp-mode and derived modes
    (save-excursion
      (save-restriction
        (widen)  ; Ensure we format the whole buffer
        (if (check-parens)
            (progn
              (indent-region (point-min) (point-max))
              (whitespace-cleanup)
              (when (called-interactively-p 'interactive)
                (message "Formatted Elisp buffer")))
          (unless quiet
            (message "Skipping format: Unbalanced parentheses")))))))

;; Hook for auto-formatting on save
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook (lambda () (my-elisp-format-buffer t)) nil t)))

;; Manual formatting keybinding
(global-set-key (kbd "C-c f") #'my-elisp-format-buffer)
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

;; show uncommitted changes in the gutter
(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode +1))

(use-package which-key
  :ensure nil  ; Built-in since Emacs 29, no need to ensure
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
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (org-mode  . flyspell-mode))
  :init
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra" "--camel-case" "--run-together")
        ispell-silently-savep t)
  :custom
  (flyspell-default-dictionary "american")
  :config
  ;; Prevent hanging by checking aspell availability
  (unless (executable-find ispell-program-name)
    (message "Aspell not found; disabling flyspell")
    (remove-hook 'text-mode-hook 'flyspell-mode)
    (remove-hook 'org-mode-hook 'flyspell-mode))
  ;; Reduce startup load
  (setq flyspell-issue-message-flag nil)  ;; Disable verbose messages
  (setq flyspell-issue-welcome-flag nil))

(use-package flyspell-popup
  :ensure t
  :after flyspell
  :hook (flyspell-mode . flyspell-popup-auto-correct-mode)
  :config
  (setq flyspell-popup-auto-correct-delay 2)  ;; Delay before popup
  (define-key flyspell-mode-map (kbd "C-c TAB") #'flyspell-popup-correct))

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Project                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package project
  :ensure nil
  :config
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
;;                             Org Mode Setup                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core Org configuration
(use-package org
  :ensure nil  ; Built-in package
  :bind (:map global-map
         ("C-c o" . (lambda () (interactive)
                      (progn
                        (when (get-buffer-window "*Org Agenda*")
                          (delete-window (get-buffer-window "*Org Agenda*")))
                        (org-switch-to-buffer-other-window "*Org Agenda*")
                        (org-agenda nil "a")
                        (call-interactively #'org-agenda-day-view))))
  :bind-keymap ("C-c o" . my-org-prefix-map)
  :init
  ;; Define prefix maps before use
  (defvar my-org-prefix-map (make-sparse-keymap) "Prefix map for Org-mode commands.")
  (defvar my-org-agenda-map (make-sparse-keymap) "Prefix map for Org-agenda commands.")
  (defvar my-org-journal-map (make-sparse-keymap) "Prefix map for Org-journal commands.")
  ;; Define sub-prefixes
  (define-key my-org-prefix-map (kbd "a") my-org-agenda-map)
  (define-key my-org-prefix-map (kbd "j") my-org-journal-map)
  ;; Set org-directory early
  (setq org-directory "~/org/")
  :config
  ;; Basic Org settings
  (setq org-startup-indented t
        org-startup-folded t
        org-return-follows-link t
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  ;; Add IDs for stable linking
  (add-hook 'org-capture-prepare-finalize-hook #'org-id-get-create)
  ;; Bindings for my-org-prefix-map
  (define-key my-org-prefix-map (kbd "r") #'my-org-refile-to-todos))

;; Org-agenda sub-config (nested because it’s a built-in part of org)
(use-package org-agenda
  :ensure nil
  :after org
  :bind (:map my-org-agenda-map
         ("a" . org-agenda)
         ("t" . my-org-agenda-today)
         ("c" . my-org-agenda-goto-current-clock))
  :config
  (setq org-agenda-start-on-weekday 1
        org-agenda-span 'week
        org-agenda-include-diary t
        org-agenda-sorting-strategy '((agenda habit-down time-up priority-down tag-up)
                                      (todo priority-down category-keep)
                                      (tags priority-down category-keep)
                                      (search category-keep))
        org-agenda-log-mode-items '(closed)
        org-agenda-start-with-log-mode t
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil))

;; Org-capture sub-config (nested because it’s a built-in part of org)
(use-package org-capture
  :ensure nil
  :after org
  :bind (:map my-org-prefix-map
         ("c" . org-capture)
         ("n" . my-org-capture-note-quick))
  :config
  (setq org-journal-dir (expand-file-name "journal/" org-directory))
  (setq org-capture-templates
        `(("j" "Journal Entry" entry
           (file+function ,(lambda () (expand-file-name (format-time-string "%Y/%m-%Y.org") org-journal-dir))
                          org-journal-find-location)
           "* %<%H:%M> %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n" :empty-lines 1)
          ("t" "General To-Do" entry
           (file+function ,(lambda () (expand-file-name (format-time-string "%Y/%m-%Y.org") org-journal-dir))
                          org-journal-find-location)
           "* TODO [#B] %?\n:Created: %T\n" :empty-lines 0)
          ("c" "Code To-Do" entry
           (file+function ,(lambda () (expand-file-name (format-time-string "%Y/%m-%Y.org") org-journal-dir))
                          org-journal-find-location)
           "* TODO [#B] [#code] %?\n:Created: %T\n%i\n%a\nProposed Solution: " :empty-lines 0)
          ("m" "Meeting" entry
           (file+function ,(lambda () (expand-file-name (format-time-string "%Y/%m-%Y.org") org-journal-dir))
                          org-journal-find-location)
           "* MEETING %^{Title} %^g\nSCHEDULED: %^{Date and Time}T\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n%?" :empty-lines 1)))
  (advice-add 'org-capture-kill :after #'my-org-capture-delete-file-after-kill)
  (add-hook 'org-capture-after-finalize-hook
            (lambda ()
              (when (get-buffer-window "*Capture*")
                (delete-window (get-buffer-window "*Capture*"))))))

;; Utility functions (standalone, not tied to a specific package)
(defun my-org-refile-to-todos ()
  "Refile current heading to todos.org under 'Tasks'."
  (interactive)
  (org-refile nil nil (list "Tasks" (expand-file-name "todos.org" org-directory) nil nil)))

(defun my-org-agenda-today ()
  "Show agenda for today only."
  (interactive)
  (org-agenda nil "a")
  (org-agenda-day-view))

(defun my-org-capture-note-quick ()
  "Quickly capture a note without switching buffers."
  (interactive)
  (org-capture nil "n"))

(defun my-org-agenda-goto-current-clock ()
  "Jump to the currently clocked task in agenda."
  (interactive)
  (org-agenda nil "a")
  (org-agenda-goto (org-clock-is-active)))

;; Org-super-agenda
(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :init (org-super-agenda-mode 1)
  :bind (:map my-org-prefix-map
         ("A" . (lambda () (interactive) (org-agenda nil "A"))))
  :config
  (setq org-agenda-custom-commands
        '(("A" "Super Agenda View"
           ((agenda "" ((org-agenda-remove-tags t)
                        (org-agenda-span 7)))
            (alltodo "" ((org-agenda-remove-tags t)
                         (org-agenda-prefix-format "  %t  %s")
                         (org-agenda-overriding-header "CURRENT STATUS")
                         (org-super-agenda-groups
                          '((:name "Critical Tasks" :tag "CRITICAL" :order 0)
                            (:name "Currently Working" :todo "IN-PROGRESS" :order 1)
                            (:name "Planning Next Steps" :todo "PLANNING" :order 2)
                            (:name "Problems & Blockers" :todo "BLOCKED" :tag "obstacle" :order 3)
                            (:name "Research Required" :tag "@research" :order 7)
                            (:name "Meeting Action Items" :and (:tag "meeting" :priority "A") :order 8)
                            (:name "Other Important Items" :and (:todo "TODO" :priority "A" :not (:tag "meeting")) :order 9)
                            (:name "General Backlog" :and (:todo "TODO" :priority "B") :order 10)
                            (:name "Non Critical" :priority<= "C" :order 11)
                            (:name "Currently Being Verified" :todo "VERIFYING" :order 20)
                            (:name "General Unscheduled" :and (:not (:tag ("personal" "work")) :not (:scheduled t)))
                            (:name "Overdue" :deadline past)
                            (:name "Completed Today" :and (:todo "DONE" :scheduled today)))))))))

;; Org-journal
(use-package org-journal
  :ensure t
  :bind (:map my-org-journal-map
         ("j" . my-org-journal-new-entry)
         ("s" . my-org-journal-search))
  :config
  (setq org-journal-file-type 'monthly)
  (setq org-journal-file-format "%m-%Y.org")
  (setq org-journal-date-format "%Y-%m-%d, %A")
  (setq org-journal-file-pattern "[0-1][0-9]-[0-9]\\{4\\}\\.org")
  (setq org-journal-enable-agenda-integration t)
  (defun org-journal-find-file ()
    "Find or create the journal file for the current month in a yearly folder."
    (let* ((year (format-time-string "%Y"))
           (month-file (format-time-string "%m-%Y.org"))
           (year-dir (expand-file-name year org-journal-dir))
           (full-path (expand-file-name month-file year-dir)))
      (unless (file-exists-p year-dir)
        (make-directory year-dir t))
      full-path))
  (defun org-journal-find-location ()
    "Find or create the current day's heading in the journal file."
    (let ((today (format-time-string org-journal-date-format)))
      (goto-char (point-min))
      (unless (re-search-forward (concat "^\\* " (regexp-quote today)) nil t)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " today "\n"))
      (goto-char (point-max))))
  (defun my-org-journal-new-entry ()
    "Open journal for today and allow easy closing."
    (interactive)
    (let ((buf (find-file-noselect (org-journal-find-file))))
      (switch-to-buffer-other-window buf)
      (org-journal-find-location)
      (org-show-entry))
    (local-set-key (kbd "C-c C-q")
                   (lambda () (interactive) (kill-buffer-and-window))))
  (defun my-org-journal-search ()
    "Search journal files using deadgrep."
    (interactive)
    (require 'deadgrep)
    (let ((default-directory org-journal-dir))
      (call-interactively #'deadgrep)))
  (defun my-org-auto-refile-from-journal ()
    "Automatically refile TODOs and scheduled items from journal to todos.org."
    (interactive)
    (let ((journal-dir org-journal-dir)
          (target-file (expand-file-name "todos.org" org-directory)))
      (dolist (file (directory-files-recursively journal-dir org-journal-file-pattern))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-heading-regexp nil t)
             (when (or (org-entry-is-todo-p)
                       (org-get-scheduled-time (point))
                       (org-get-deadline-time (point)))
               (org-refile nil nil (list "Tasks" target-file nil nil) t)))
           (save-buffer)))))
  (add-hook 'org-capture-after-finalize-hook #'my-org-auto-refile-from-journal)
  (run-at-time t 3600 #'my-org-auto-refile-from-journal))

;; Org-download
(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :bind (:map my-org-prefix-map
         ("O" . org-download-clipboard))
  :config
  (setq org-download-image-dir (expand-file-name "images" org-journal-dir))
  (setq org-download-method
        (lambda (link)
          "Save downloaded files to daily journal entry."
          (let ((filename (org-download--fullname link)))
            (org-download--insert-image filename)
            (org-capture nil "j")
            (insert (format "[[file:%s]]" filename))
            (org-capture-finalize t)))))

;; Org-attach
(use-package org-attach
  :ensure nil
  :after org
  :bind (:map my-org-prefix-map
         ("a" . my-org-attach-to-journal))
  :config
  (setq org-attach-dir-relative t)
  (setq org-attach-use-inheritance t)
  (setq org-attach-id-dir (expand-file-name "attachments" org-journal-dir))
  (defun my-org-attach-to-journal ()
    "Attach a file to the current journal entry."
    (interactive)
    (org-capture nil "j")
    (call-interactively #'org-attach-attach)
    (org-capture-finalize t)))

;; Org-protocol
(use-package org-protocol
  :ensure nil
  :demand t
  :config
  (setq org-protocol-default-template-key "j")
  (add-to-list 'org-protocol-protocol-alist
               '("store-link-to-journal"
                 :protocol "store-link"
                 :function my-org-protocol-store-link))
  (add-to-list 'org-protocol-protocol-alist
               '("capture-to-journal"
                 :protocol "capture"
                 :function my-org-protocol-capture-to-journal))
  (defun my-org-protocol-store-link (data)
    "Store a link in the journal."
    (org-capture nil "j")
    (insert (org-link-make-string (cadr (split-string data "://")) "External Link"))
    (org-capture-finalize t))
  (defun my-org-protocol-capture-to-journal (data)
    "Capture arbitrary data to journal."
    (org-capture nil "j")
    (insert (cadr (split-string data "://")))
    (org-capture-finalize t)))

;; Org-timeblock
(use-package org-timeblock
  :ensure t
  :vc (:url "https://github.com/ichernyshovvv/org-timeblock")
  :bind (:map my-org-prefix-map
         ("w" . org-timeblock)))

;; Org-modern
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-table-vertical 2
        org-modern-table-horizontal 2
        org-modern-star ["●" "○" "✸" "✿"]
        org-modern-list '((43 . "•") (45 . "–") (42 . "•"))))

;; Org-auto-tangle
(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Magit/Forge                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prefer remote upstream for magit
(setq magit-prefer-remote-upstream t)

(use-package magit :defer t)
(use-package forge :defer t)

(keymap-global-set "C-c g"  'magit)

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
;;                           AEmacs (Anthropic)                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aidermacs
  :vc (:url "https://github.com/MatthewZMD/aidermacs")
  :config
  (setq aidermacs-default-model "anthropic/claude-3-7-sonnet-20250219")
  (global-set-key (kbd "C-c A") 'aidermacs-transient-menu)
  ; See the Configuration section below
  (setq aidermacs-auto-commits t)

  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-architect-model "o1-mini")
  (setq aidermacs-editor-model "deepseek/deepseek-chat"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Misc Packages                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Recentf with Consult integration
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 25
        recentf-exclude '("/tmp/" "/ssh:"))
  :bind (("C-c r" . consult-recent-file)))  ; Vertico-powered recent files

;; SSH Deploy with status feedback
(use-package ssh-deploy
  :ensure t
  :demand t
  :after hydra
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode)
  (ssh-deploy-add-menu)
  (ssh-deploy-hydra "C-c C-z")
  (add-hook 'ssh-deploy-after-save-hook
            (lambda ()
              (message "SSH Deploy: File %s synced" (buffer-file-name)))))

;; Emacs-everywhere with Wayland tweaks and binding
(use-package emacs-everywhere
  :ensure t
  :config
  (setq emacs-everywhere-app-info-function #'grim/emacs-everywhere-wayland-app-info
        emacs-everywhere-copy-command '("wl-copy")
        emacs-everywhere-paste-command '("wl-paste"))
  (add-hook 'emacs-everywhere-init-hook #'whitespace-cleanup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Dired                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :ensure nil  ; Built-in, no need to install
  :bind (("C-x C-d" . dired)  ; Quick access to Dired
         :map dired-mode-map
         ("RET" . dired-find-alternate-file)  ; Replace buffer instead of opening new
         ("<backspace>" . dired-up-directory) ; Intuitive up-directory
         ("C-c C-e" . wdired-change-to-wdired-mode) ; Editable Dired
         ("C-c g" . dired-git-info-mode) ; Toggle git info
         ("C-c t" . dired-toggle-read-only) ; Quick toggle read-only
         ("M-!" . dired-smart-shell-command)) ; Enhanced shell command
  :hook ((dired-mode . dired-hide-details-mode) ; Start with details hidden
         (dired-mode . nerd-icons-dired-mode) ; Icon goodness
         (dired-mode . dired-preview-mode) ; Auto-preview always on
         (dired-mode . hl-line-mode)) ; Highlight current line
  :custom
  (dired-listing-switches "-lah --group-directories-first") ; Human-readable, dirs first
  (dired-dwim-target t) ; Smart target directory guessing
  (dired-recursive-copies 'always) ; Recursive copies without asking
  (dired-recursive-deletes 'always) ; Recursive deletes without asking
  (dired-auto-revert-buffer t) ; Auto-refresh on revisit
  (dired-hide-details-hide-symlink-targets nil) ; Show symlink targets
  (dired-guess-shell-alist-user '(("\\.pdf\\'" "xdg-open"))) ; Custom file openers
  (dired-use-ls-dired t) ; Use ls emulation for better compatibility
  :config
  ;; Add colors to Dired
  (use-package diredfl
    :ensure t
    :config
    (diredfl-global-mode 1))

  ;; Nerd Icons for Dired
  (use-package nerd-icons-dired
    :ensure t
    :after nerd-icons
    :config
    (setq nerd-icons-dired-v-adjust -0.1) ; Fine-tune icon alignment
    (set-face-attribute 'nerd-icons-dired-dir-face nil :foreground "#81a1c1")) ; Nordic blue dirs

  ;; Dired Preview (always on, no toggle)
  (use-package dired-preview
    :ensure t
    :custom
    (dired-preview-delay 0) ; Faster preview popup
    (dired-preview-max-size (* 10 1024 1024)) ; 10MB max for previews
    :config
    ;; Enable globally so it’s always on, no need for per-buffer toggle
    (dired-preview-global-mode 1))

  ;; Dired Git Info (polished)
  (use-package dired-git-info
    :ensure t
    :custom
    (dgi-auto-hide-details-p nil) ; Keep details visible with git info
    :config
    (setq dired-git-info-format " (%s)")) ; Customize format if desired

  ;; Dired Subtree for tree-like navigation
  (use-package dired-subtree
    :ensure t
    :bind (:map dired-mode-map
                ("<tab>" . dired-subtree-toggle) ; Expand/collapse subdirs
                ("<C-tab>" . dired-subtree-cycle)) ; Cycle through subdirs
    :config
    (setq dired-subtree-use-backgrounds nil) ; Cleaner look with your theme
    (set-face-attribute 'dired-subtree-depth-1-face nil :background "#3b4252")) ; Subtle depth

  ;; Async operations for speed
  (use-package dired-async
    :ensure nil ; Part of dired-aux
    :after dired
    :config
    (dired-async-mode 1))

  ;; Integrate with Treemacs for project awareness
  (with-eval-after-load 'treemacs
    (add-hook 'treemacs-select-hook
              (lambda ()
                (when (treemacs-current-workspace)
                  (dired (treemacs-get-local-project-root))))))

  ;; Custom function: Open in external app with C-c o
  (defun dired-open-externally ()
    "Open file under cursor with xdg-open."
    (interactive)
    (let ((file (dired-get-file-for-visit)))
      (start-process "dired-open" nil "xdg-open" file)))
  (define-key dired-mode-map (kbd "C-c o") 'dired-open-externally)

  ;; Custom function: Quick copy file path
  (defun dired-copy-file-path ()
    "Copy the full path of the file under cursor to kill ring."
    (interactive)
    (let ((path (dired-get-file-for-visit)))
      (kill-new path)
      (message "Copied path: %s" path)))
  (define-key dired-mode-map (kbd "C-c w") 'dired-copy-file-path)

  ;; Custom function: Quick filter with Consult
  (defun dired-consult-filter ()
    "Filter Dired buffer using Consult narrowing."
    (interactive)
    (consult-focus-lines
     (lambda (file)
       (string-match-p (regexp-quote (consult--read "Filter: ")) file))))
  (define-key dired-mode-map (kbd "C-c f") 'dired-consult-filter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     eww                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eww
  :commands (eww eww-browse-url)  ; Add 'eww' for manual use
 ;; :bind (("C-c w" . eww))         ; Easy access to EWW
  :init
  (setq browse-url-handlers
        '(("\\.pdf\\'" . my-open-remote-pdf-in-emacs)
          ("^https?://" . eww-browse-url)))
  :config
  (setq eww-auto-rename-buffer 'title)  ; Nicer buffer names
  (add-hook 'eww-mode-hook (lambda () (display-line-numbers-mode -1))))  ; No line numbers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     pdf                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pdf-tools
  :ensure t
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
         (message "Failed to open PDF from %s: %s" url (error-message-string err))))
      (when (file-exists-p temp-file)
        (delete-file temp-file))))
  :config
  (unless (featurep 'pdf-tools)  ; Install only if not already loaded
    (pdf-tools-install :no-query))
  (setq pdf-view-display-size 'fit-page
        pdf-view-continuous t
        pdf-view-use-scaling t)
  (add-to-list 'pdf-view-incompatible-modes 'display-line-numbers-mode)
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (hl-line-mode -1))))  ; Match your eat config

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ERC (IRC Client)                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare-function pcomplete-erc-setup "erc-pcomplete")
(use-package erc
  :defer t
  :config
  (add-hook 'erc-mode-hook #'my-erc-set-fill-column)
  (setq erc-track-remove-disconnected-buffers t
        ;; ... other settings ...
        erc-notifications-keywords nil)  ; Keep this
  (setq erc-modules '(networks notifications))
  (erc-update-modules)
  (add-hook 'erc-nick-changed-functions #'my-erc-update-notifications-keywords)
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
  ;; Enable ERC modules, including 'networks' but excluding 'nickbar'
  (setq erc-modules '(networks notifications))  ; Add 'networks' explicitly
  (erc-update-modules)

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
                                      (elfeed-update)
                                      (my-elfeed-start-auto-update)))
         (elfeed-show-mode . (lambda () (setq shr-external-browser 'eww-browse-url))))
  :bind (:map elfeed-search-mode-map
              ("q" . my-elfeed-quit-and-stop-timer)
              :map elfeed-show-mode-map
              ("RET" . shr-browse-url)
              ("v" . elfeed-tube-mpv))
  :config
  (setq shr-external-browser 'eww-browse-url)
  (defvar my-elfeed-update-timer nil "Timer for Elfeed auto-updates.")  ; Keep this here
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
(when (boundp 'treesit-language-source-alist)  ;; Emacs 29+ check
  (use-package treesit-auto
    :ensure t
    :config
    (setq treesit-auto-install 'prompt)  ; Prompt to install grammars
    (global-treesit-auto-mode)           ; Auto-switch to Tree-sitter modes
    ;; Add custom recipes if needed (e.g., for org-mode)
    (add-to-list 'treesit-auto-recipe-list
                 (make-treesit-auto-recipe
                  :lang 'org
                  :ts-mode 'org-ts-mode
                  :remap 'org-mode
                  :url "https://github.com/emacs-tree-sitter/tree-sitter-org"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                So-long-mode                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :ensure nil
  :hook ((prog-mode . flymake-mode)
         (emacs-lisp-mode . flymake-mode))  ; Explicitly enable for Elisp
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-no-changes-timeout 1)  ; Faster feedback after typing
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile nil t)
              (add-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc nil t)))
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-buffer-diagnostics)  ; List errors in buffer
              ("C-c ! n" . flymake-goto-next-error)          ; Jump to next error
              ("C-c ! p" . flymake-goto-prev-error)))        ; Jump to previous error


;; Optional: stricter linting with elisp-lint
(use-package elisp-lint
  :ensure t
  :commands (elisp-lint-buffer elisp-lint-file)
  :bind (("C-c l" . elisp-lint-buffer))
  :config
  (setq elisp-lint-ignored-validators '("package-lint")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNUS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load required packages
;; (require 'auth-source)  ;; For reading authinfo.gpg
;; (require 'nnimap)
;; (require 'oauth2)

;; (defvar nnimap-xoauth2-running nil
;;   "Flag to prevent reentrant calls to nnimap-xoauth2.")

;; (defun nnimap-xoauth2 (username &optional password)
;;   "Authenticate to IMAP with XOAUTH2, avoiding plstore."
;;   (when nnimap-xoauth2-running
;;     (message "nnimap-xoauth2 already running, skipping recursive call")
;;     (return-from nnimap-xoauth2))
;;   (let ((nnimap-xoauth2-running t))
;;     (message "nnimap-xoauth2 called for %s" username)
;;     (let* ((auth-info (car (auth-source-search :host "outlook.office365.com" :port 993 :user username :max 1)))
;;            (auth-url (plist-get auth-info :xoauth2-auth-url))
;;            (token-url (plist-get auth-info :xoauth2-token-url))
;;            (scope (plist-get auth-info :xoauth2-scope))
;;            (client-id (plist-get auth-info :xoauth2-client-id))
;;            (client-secret (plist-get auth-info :xoauth2-client-secret))
;;            (redirect-uri (plist-get auth-info :xoauth2-redirect-uri))
;;            (token (oauth2-auth auth-url token-url scope client-id client-secret redirect-uri)))
;;       (message "Raw token from oauth2-auth: %s" token)
;;       (if (oauth2-token-p token)
;;           (let ((access-token (oauth2-token-access-token token)))
;;             (if access-token
;;                 (progn
;;                   (message "Extracted access token: %s" access-token)
;;                   (message "Opening IMAP server outlook.office365.com")
;;                   (nnimap-open-server "outlook.office365.com"
;;                                       '((nnimap-address "outlook.office365.com")
;;                                         (nnimap-server-port 993)
;;                                         (nnimap-stream ssl)))
;;                   (message "Sending XOAUTH2 token for %s" username)
;;                   (nnimap-send-command "AUTHENTICATE XOAUTH2 %s"
;;                                        (base64-encode-string
;;                                         (format "user=%s\1auth=Bearer %s\1\1"
;;                                                 username
;;                                                 access-token))))
;;               (message "No access token in token object: %s" token)
;;               (error "No access token extracted")))
;;         (message "Token retrieval failed or invalid token: %s" token)
;;         (error "XOAUTH2 token retrieval failed")))))

;; ;; Force XOAUTH2 for Gnus with guard
;; (defadvice nnimap-open-server (before force-xoauth2 activate)
;;   "Force XOAUTH2 authentication for outlook.office365.com."
;;   (when (and (string= (ad-get-arg 0) "outlook.office365.com")
;;              (not nnimap-xoauth2-running))
;;     (message "Forcing XOAUTH2 for %s" (ad-get-arg 0))
;;     (nnimap-xoauth2 "tj.theesfeld@citywide.io")))

;; ;; Gnus config
;; (setq oauth2-token-file "~/.config/emacs/gnus/oauth2.plstore")
;; (setq gnus-select-method
;;       '(nnimap "outlook.office365.com"
;;                (nnimap-address "outlook.office365.com")
;;                (nnimap-server-port 993)
;;                (nnimap-stream ssl)
;;                (nnimap-authenticator xoauth2)
;;                (nnimap-user "tj.theesfeld@citywide.io")))

;; (require 'gnus)

;; ;; Auth-source settings
;; (setq auth-sources '("~/.authinfo.gpg"))  ;; Only use authinfo.gpg
;; (setq auth-source-do-cache t)             ;; Cache credentials
;; (setq auth-source-cache-expiry nil)       ;; Never expire cache
;; (setq auth-source-debug t)                ;; Debug auth-source
;; (setq nnimap-record-commands t)  ;; Log IMAP commands
;; (setq gnus-agent t)              ;; Enable agent for better logging
;; ;; Gnus and nnimap debug settings
;; (setq gnus-verbose 10)                    ;; High verbosity for Gnus
;; (setq gnus-verbose-backends t)            ;; Debug backends
;; (setq nnimap-verbose t)                   ;; Debug nnimap specifically
;; (setq oauth2-debug t)
;; (setq nnimap-record-commands t)
;; ;; Gnus configuration
;; (setq gnus-select-method
;;       '(nnimap "outlook.office365.com"
;;                (nnimap-address "outlook.office365.com")
;;                (nnimap-server-port 993)
;;                (nnimap-stream ssl)
;;                (nnimap-authenticator xoauth2)  ;; Built-in XOAUTH2
;;                (nnimap-user "tj.theesfeld@citywide.io")
;;                (nnimap-expunge t)))

;; (setq gnus-secondary-select-methods '((nnrss "feeds")))

;; ;; Other Gnus settings
;; (setq gnus-summary-line-format "%U%R%z %d %I%(%[%4L: %-23,23f%]%) %s\n"
;;       gnus-group-line-format "%M%S%p%P%5y: %(%g%)%l\n"
;;       gnus-asynchronous t
;;       gnus-use-adaptive-scoring t
;;       gnus-use-cache t
;;       gnus-cache-directory "~/.config/emacs/gnus/cache/"
;;       gnus-article-save-directory "~/.config/emacs/gnus/saved/"
;;       gnus-read-active-file 'some
;;       gnus-check-new-newsgroups nil
;;       gnus-save-newsrc-file t
;;       gnus-read-newsrc-file t
;;       gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date gnus-thread-sort-by-number)
;;       gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
;;       gnus-treat-fill-long-lines t
;;       gnus-treat-display-smileys t
;;       gnus-treat-emphasize t
;;       gnus-startup-file "~/.config/emacs/gnus/newsrc"
;;       gnus-save-killed-list nil
;;       gnus-use-dribble-file nil)

;; ;; SMTP configuration for sending mail
;; (setq smtpmail-smtp-user "tj.theesfeld@citywide.io"
;;       smtpmail-smtp-server "smtp.office365.com"
;;       smtpmail-smtp-service 587
;;       smtpmail-stream-type 'starttls
;;       smtpmail-auth-supported '(xoauth2)
;;       send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it)

;; ;; Drafts
;; (setq gnus-draft-mode 'nnimap
;;       gnus-drafts-directory "nnimap+outlook.office365.com:Drafts"
;;       browse-url-browser-function 'ignore)

;; ;; OAuth2 token storage


;; ;; Key bindings
;; (add-hook 'gnus-group-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "g") 'gnus-group-get-new-news)))

;; (add-hook 'gnus-summary-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "m") 'gnus-summary-mail-other-window)))

;; ;; Force clear auth cache before starting Gnus and ensure OAuth2 prompt
;; (defun gnus-oauth2-reload-auth (&rest _)
;;   "Force reload of auth-source and clear oauth2.plstore before starting Gnus."
;;   (auth-source-forget-all-cached)
;; ;;  (when (file-exists-p oauth2-token-file)
;; ;;    (delete-file oauth2-token-file)))  ;; Remove this line later for token reuse
;; )
;; (advice-add 'gnus :before #'gnus-oauth2-reload-auth)

;; ;; Debugging function for auth-source
;; (defun test-auth-source-complete ()
;;   "Test if auth-source can find complete credentials for Outlook365."
;;   (interactive)
;;   (message "Searching for auth info...")
;;   (let* ((auth-info (car (auth-source-search :host "outlook.office365.com"
;;                                              :port 993
;;                                              :user "tj.theesfeld@citywide.io"
;;                                              :max 1)))
;;          (user (plist-get auth-info :user))
;;          (host (plist-get auth-info :host))
;;          (secret-fn (plist-get auth-info :secret))
;;          (xoauth2-client-id (plist-get auth-info :xoauth2-client-id))
;;          (xoauth2-client-secret (plist-get auth-info :xoauth2-client-secret)))
;;     (message "Found auth info: %s" (if auth-info "Yes" "No"))
;;     (when auth-info
;;       (message "Host: %s, User: %s" host user)
;;       (message "Has password/secret function: %s" (if secret-fn "Yes" "No"))
;;       (message "Has client ID: %s" (if xoauth2-client-id "Yes" "No"))
;;       (message "Has client secret: %s" (if xoauth2-client-secret "Yes" "No"))
;;       (when secret-fn
;;         (condition-case err
;;             (let ((secret (funcall secret-fn)))
;;               (message "Was able to retrieve secret: %s" (if secret "Yes" "No"))
;;               (when (and secret (stringp secret))
;;                 (message "Secret type: %s" (if (string= secret "xoauth2") "xoauth2" "other"))))
;;           (error (message "Error retrieving secret: %s" (error-message-string err))))))))

;; ;; Token fetch function
;; (defun test-get-oauth2-token ()
;;   "Test OAuth2 token retrieval for Outlook."
;;   (interactive)
;;   (auth-source-forget-all-cached)
;;   (let* ((auth-info (car (auth-source-search :host "outlook.office365.com" :port 993 :user "tj.theesfeld@citywide.io" :max 1)))
;;          (client-id (plist-get auth-info :xoauth2-client-id))
;;          (client-secret (plist-get auth-info :xoauth2-client-secret))
;;          (auth-url (plist-get auth-info :xoauth2-auth-url))
;;          (token-url (plist-get auth-info :xoauth2-token-url))
;;          (scope (plist-get auth-info :xoauth2-scope))
;;          (redirect-uri (plist-get auth-info :xoauth2-redirect-uri)))
;;     (if (and client-id client-secret auth-url token-url scope)
;;         (progn
;;           (message "Fetching token...")
;;           (let ((token (oauth2-auth-and-store auth-url token-url scope client-id client-secret redirect-uri)))
;;             (message "Token stored: %s" (if token "Yes" "No"))
;;             (when token
;;               (with-temp-file oauth2-token-file
;;                 (insert (pp-to-string token))))))  ;; Force plain-text write
;;       (message "Missing OAuth2 parameters"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   0x0.st                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package 0x0
  :ensure t
  :bind (:map global-map
              ("C-c u" . '0x0-dwim)
              ("C-c U" . '0x0-upload-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   eat                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eat
  :ensure t ;; Automatically install it, because who has time for manual bullshit?
  :defer t  ;; Lazy-load this beast, we’re not savages
  :commands (eat) ;; Autoload the main entry point
  :bind (("C-`" . (lambda () (interactive) (my/toggle-buffer "*eat*" 'eat)))
         :map eat-mode-map
         ("C-c C-k" . eat-kill-process) ;; Extra flair for killing the process
         ("C-c C-r" . eat-reset)) ;; Reset the terminal like a boss
 :custom
  (eat-kill-buffer-on-exit t) ;; Clean up after yourself, you animal
  (eat-enable-auto-scrolling t) ;; Scroll like it’s 2025
  (eat-term-name "xterm-256color") ;; Full color glory
  :config
  ;; Add some fucking awesome tweaks
  (add-hook 'eat-mode-hook #'turn-on-auto-fill)) ;; Keep shit tidy

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   calc                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (use-package calc
   :ensure nil  ;; calc is built-in, no need to install
   :bind (("C-c c" . 'calc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                 python venv                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-virtualenv
  :ensure t
  :config
  (setq auto-virtualenv-verbose t)
  (setq auto-virtualenv-global-dirs
      '("~/.virtualenvs/" "~/.pyenv/versions/" "~/.envs/" "~/.conda/" "~/.conda/envs/"))
(setq auto-virtualenv-python-project-files
      '("requirements.txt" "Pipfile" "pyproject.toml" "setup.py" "manage.py" "tox.ini" ".flake8"))
  (auto-virtualenv-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Final Cleanup                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
;;; init.el ends here
