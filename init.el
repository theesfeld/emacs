;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-
;;; version: 0.7.3

;; Copyright (C) 2024 William Theesfeld <william@theesfeld.net>

;; Author: William Theesfeld <william@theesfeld.net>
;; Maintainer: William Theesfeld <william@theesfeld.net>
;; URL: https://github.com/theesfeld/emacs-config
;; Keywords: convenience, config
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Email: tj@emacs.su

;;; Code:

;;; SYSTEM DETECTION AND DYNAMIC OPTIMIZATION

(defun my/get-system-memory ()
  "Get system memory in GB."
  (if (eq system-type 'gnu/linux)
      (/ (string-to-number
          (shell-command-to-string
           "grep MemTotal /proc/meminfo | awk '{print $2}'"))
         1048576.0)
    32)) ; Default fallback

(defun my/get-cpu-count ()
  "Get number of CPU cores."
  (or (num-processors) 4)) ; Default fallback

(defconst my/system-memory (my/get-system-memory)
  "System memory in GB.")

(defconst my/cpu-count (my/get-cpu-count)
  "Number of CPU cores.")

(defconst my/high-spec-system-p
  (and (>= my/system-memory 32) (>= my/cpu-count 8))
  "Non-nil if system has high specifications.")

(defconst my/ultra-high-spec-system-p
  (and (>= my/system-memory 64) (>= my/cpu-count 12))
  "Non-nil if system has ultra-high specifications.")

;;; vc stuff

(setq package-vc-register-as-project nil)

;;; Garbage Collection Magic Hack (gcmh) - Dynamically Optimized
(use-package gcmh
  :ensure t
  :demand t
  :if (not (bound-and-true-p byte-compile-current-file))
  :custom
  (gcmh-idle-delay 10)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold
   (cond (my/ultra-high-spec-system-p (* 2048 1024 1024))
         (my/high-spec-system-p (* 512 1024 1024))
         (t (* 128 1024 1024))))
  (gcmh-low-cons-threshold
   (cond (my/ultra-high-spec-system-p (* 256 1024 1024))
         (my/high-spec-system-p (* 64 1024 1024))
         (t (* 20 1024 1024))))
  :config
  (gcmh-mode 1)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-percentage
                    (cond (my/ultra-high-spec-system-p 0.15)
                          (my/high-spec-system-p 0.2)
                          (t 0.1)))
              (garbage-collect))))

;;; pinentry

(setenv "GPG_AGENT_INFO" nil)
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)
(when (fboundp 'pinentry-start)
  (pinentry-start))

;;; CUSTOM FUNCTIONS

(defun my-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let* ((source (auth-source-search :host host)))
    (if (eq prop :secret)
        (funcall (plist-get (car source) prop))
      (plist-get (flatten-list source) prop))))

(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general \=`keyboard-quit'."
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

(keymap-global-set "<remap> <keyboard-quit>" #'my/keyboard-quit-dwim)

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
          '("firefox" "mpv"))
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
    (when (> (length (window-list)) 1)
      (enlarge-window-horizontally
       (round (* (window-width) (- scale-factor 1)))))))

(defun decrease-text-and-pane ()
  "Decrease text size and adjust window width proportionally."
  (interactive)
  (let* ((orig-scale (or text-scale-mode-amount 0))
         (scale-factor (/ 1.0 text-scale-mode-step)))
    (text-scale-decrease 1)
    (when (> (length (window-list)) 1)
      (shrink-window-horizontally
       (round (* (window-width) (- 1 scale-factor)))))))

(keymap-global-set "s-=" #'increase-text-and-pane)
(keymap-global-set "s--" #'decrease-text-and-pane)

(declare-function completion-preview-insert "completion-preview")
(declare-function completion-preview-next-candidate
                  "completion-preview")
(declare-function completion-preview-prev-candidate
                  "completion-preview")
(declare-function completion-preview--hide "completion-preview")

;;; FONT CONFIGURATION

;; Set default font
(cond
 ((find-font (font-spec :name "AporeticSansMono Nerd Font"))
  (set-face-attribute 'default nil
                      :font "AporeticSansMono Nerd Font"
                      :height 170))
 ;; ((find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
 ;;  (set-face-attribute 'default nil
 ;;                      :font "BerkeleyMonoVariable Nerd Font Mono"
 ;;                      :height 140))
 (t
  (set-face-attribute 'default nil :height 170)))

;; Set variable-pitch font
(cond
 ((find-font (font-spec :name "AporeticSerif Nerd Font"))
  (set-face-attribute 'variable-pitch nil
                      :font "AporeticSerif Nerd Font"
                      :height 150))
 ;; ((find-font (font-spec :name "BerkeleyMonoVariable Nerd Font"))
 ;;  (set-face-attribute 'variable-pitch nil
 ;;                      :font "BerkeleyMonoVariable Nerd Font"
 ;;                      :height 150))
 (t
  (set-face-attribute 'variable-pitch nil :height 150)))

;;; VARIABLE-PITCH COMMENTS IN PROG-MODE

(defun my/prog-mode-variable-pitch-comments ()
  "Use variable-pitch font for comments in programming modes."
  (face-remap-add-relative 'font-lock-comment-face
                           '(:inherit (font-lock-comment-face variable-pitch)))
  (face-remap-add-relative 'font-lock-comment-delimiter-face
                           '(:inherit (font-lock-comment-delimiter-face variable-pitch)))
  (face-remap-add-relative 'font-lock-doc-face
                           '(:inherit (font-lock-doc-face variable-pitch))))

(add-hook 'prog-mode-hook #'my/prog-mode-variable-pitch-comments)

;;; EDNC NOTIFICATIONS (DBUS)

(use-package ednc
  :ensure t
  :hook (after-init . ednc-mode)
  :config
  (require 'consult)

  (defgroup ednc-enhanced nil
    "Enhanced EDNC notifications."
    :group 'ednc)

  (defcustom ednc-history-limit 200
    "Maximum number of notifications to keep in history."
    :type 'integer
    :group 'ednc-enhanced)

  (defvar ednc--notification-history nil
    "List of past notifications for consult browsing.")
  (defvar ednc--notification-ring (make-ring ednc-history-limit)
    "Ring buffer for notification history.")

  (defface ednc-notification-time-face
    '((t :inherit font-lock-comment-face))
    "Face for timestamps in notification history.")

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

  (setq ednc-notification-presentation-functions nil)
  (add-hook 'ednc-notification-presentation-functions #'ednc--store-in-history))

(use-package ednc-popup
  :vc (:url "https://codeberg.org/akib/emacs-ednc-popup.git" :branch "main")
  :if (and (not (bound-and-true-p byte-compile-current-file))
           (not noninteractive))
  :config
  (setq ednc-popup-timeout 5
        ednc-popup-width 50
        ednc-popup-max-height 10
        ednc-popup-max-count 4)
  (add-hook 'ednc-notification-presentation-functions #'ednc-popup-presentation-function))


;;; EXWM - Dynamic multi-monitor configuration for Emacs 30.1
(when (eq window-system 'x)
  (use-package exwm
    :ensure t
    :init
    (require 'exwm-randr)
    (require 'exwm-systemtray)
    :config
    (setq exwm-workspace-number 10)
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)
    (setq exwm-input-prefix-keys
          '(?\C-x
            ?\C-u
            ?\C-h
            ?\M-x
            ?\M-:
            ?\C-\M-j
            ?\C-\ ;why do i need a comment here wtf
            XF86AudioLowerVolume
            XF86AudioRaiseVolume
            XF86AudioMute
            XF86MonBrightnessUp
            XF86MonBrightnessDown))

    (setq exwm-input-global-keys
          `(
            ([?\s-l] . desktop-environment-lock-screen)
            ([?\s-\ ] . my/app-launcher)
            ([?\s-r] . exwm-reset)
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-m] . exwm-workspace-move-window)
            ([?\s-f] . exwm-floating-toggle-floating)
            ([?\s-F] . exwm-layout-toggle-fullscreen)
            ([s-left] . windmove-left)
            ([s-right] . windmove-right)
            ([s-up] . windmove-up)
            ([s-down] . windmove-down)
            ([?\s-0] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
            ([?\s-1] . (lambda () (interactive) (exwm-workspace-switch-create 1)))
            ([?\s-2] . (lambda () (interactive) (exwm-workspace-switch-create 2)))
            ([?\s-3] . (lambda () (interactive) (exwm-workspace-switch-create 3)))
            ([?\s-4] . (lambda () (interactive) (exwm-workspace-switch-create 4)))
            ([?\s-5] . (lambda () (interactive) (exwm-workspace-switch-create 5)))
            ([?\s-6] . (lambda () (interactive) (exwm-workspace-switch-create 6)))
            ([?\s-7] . (lambda () (interactive) (exwm-workspace-switch-create 7)))
            ([?\s-8] . (lambda () (interactive) (exwm-workspace-switch-create 8)))
            ([?\s-9] . (lambda () (interactive) (exwm-workspace-switch-create 9)))))

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

    (defun my/exwm-update-class ()
      "Update EXWM buffer name to window class name."
      (exwm-workspace-rename-buffer exwm-class-name))

    (defcustom my/exwm-title-max-length 80
      "Maximum length for EXWM window titles."
      :type 'integer
      :group 'exwm)

    (defun my/exwm-update-title ()
      "Update EXWM buffer name to window title."
      (let ((title (truncate-string-to-width exwm-title
                                             my/exwm-title-max-length
                                             nil nil "…")))
        (pcase exwm-class-name
          ("Firefox" (exwm-workspace-rename-buffer
                      (format "Firefox: %s" title)))
          (_ (exwm-workspace-rename-buffer
              (format "%s: %s" exwm-class-name title))))))

    (add-hook 'exwm-update-class-hook #'my/exwm-update-class)
    (add-hook 'exwm-update-title-hook #'my/exwm-update-title)

    (setq exwm-systemtray-height 24)
    (setq exwm-systemtray-icon-gap 5)
    (setq exwm-systemtray-background-color "#1a1a1a")
    (setq exwm-systemtray-workspace nil)

    (defun my/exwm-randr-setup ()
      "Set up monitor configuration before EXWM starts."
      (let* ((xrandr-output (shell-command-to-string "xrandr"))
             (connected-monitors
              (seq-filter (lambda (line)
                            (string-match-p " connected" line))
                          (split-string xrandr-output "\n")))
             (monitor-names
              (mapcar (lambda (line)
                        (car (split-string line)))
                      connected-monitors))
             (has-laptop (member "eDP-1" monitor-names))
             (external-monitors (seq-filter (lambda (m) (not (string= m "eDP-1"))) monitor-names)))

        (when (or has-laptop external-monitors)
          (let ((workspace-plist '())
                (workspace-num 0)
                (num-external (length external-monitors)))
            ;; Workspace 0 always on eDP-1 if available
            (if has-laptop
                (setq workspace-plist (list workspace-num "eDP-1"))
              ;; If no laptop, workspace 0 goes to first external
              (when external-monitors
                (setq workspace-plist (list workspace-num (car external-monitors)))))
            ;; Distribute remaining workspaces across external monitors
            (when external-monitors
              (if (= num-external 1)
                  ;; Single external monitor gets all remaining workspaces
                  (dotimes (i 9)
                    (setq workspace-num (1+ workspace-num))
                    (setq workspace-plist (append workspace-plist
                                                  (list workspace-num (car external-monitors)))))
                ;; Multiple external monitors - distribute evenly
                (let ((monitor-index 0))
                  (dotimes (i 9)
                    (setq workspace-num (1+ workspace-num))
                    (setq workspace-plist (append workspace-plist
                                                  (list workspace-num
                                                        (nth monitor-index external-monitors))))
                    (setq monitor-index (mod (1+ monitor-index) num-external))))))
            ;; Set the configuration
            (setq exwm-randr-workspace-monitor-plist workspace-plist)))))

    (defun my/exwm-configure-monitors ()
      "Configure xrandr settings and refresh EXWM."
      (let* ((xrandr-output (shell-command-to-string "xrandr"))
             (monitor-info
              (mapcar (lambda (line)
                        (when (string-match "\\([^ ]+\\) connected\\(?: primary\\)? \\([0-9]+\\)x\\([0-9]+\\)" line)
                          (list (match-string 1 line)
                                (string-to-number (match-string 2 line))
                                (string-to-number (match-string 3 line)))))
                      (seq-filter (lambda (line)
                                    (string-match-p " connected" line))
                                  (split-string xrandr-output "\n"))))
             (monitor-info (seq-filter #'identity monitor-info))
             (external-monitors (seq-filter (lambda (m) (not (string= (car m) "eDP-1"))) monitor-info))
             (has-laptop (seq-find (lambda (m) (string= (car m) "eDP-1")) monitor-info))
             (laptop-width (if has-laptop (* 2880 0.67) 0))
             (current-x 0))
        ;; Intel Xe driver workaround: kill and restart picom if needed
        (when (and (executable-find "picom")
                   (string-match-p "xe" (shell-command-to-string "lsmod | grep -E 'xe|i915'")))
          (shell-command "pkill picom")
          (sit-for 0.1))

        ;; Build xrandr command
        (cond
         ;; Three monitors with laptop on the right
         ((and has-laptop (>= (length external-monitors) 2))
          (let* ((left-monitor (car external-monitors))
                 (center-monitor (cadr external-monitors))
                 (current-x 0))
            ;; Intel Xe workaround: turn off all monitors first
            (shell-command (format "xrandr --output %s --off --output %s --off --output eDP-1 --off"
                                   (car left-monitor) (car center-monitor)))
            (sit-for 0.3)
            ;; Turn on monitors one by one
            (shell-command (format "xrandr --output %s --auto --pos 0x0 --primary"
                                   (car left-monitor)))
            (setq current-x (cadr left-monitor))
            (shell-command (format "xrandr --output %s --auto --pos %dx0"
                                   (car center-monitor) current-x))
            (setq current-x (+ current-x (cadr center-monitor)))
            (shell-command (format "xrandr --output eDP-1 --auto --pos %dx0"
                                   current-x))
            ;; Intel Xe workaround: use transform trick to ensure display updates
            (sit-for 0.1)
            (shell-command "xrandr --output eDP-1 --transform 1.001,0,0,0,1.001,0,0,0,1")
            (sit-for 0.1)
            (shell-command "xrandr --output eDP-1 --transform none")
            ;; Restart picom with GLX backend
            (when (executable-find "picom")
              (start-process "picom" nil "picom" "-b"))
            (message "EXWM: Intel Xe monitor configuration applied")))
         ;; Two monitors with laptop
         ((and has-laptop (= (length external-monitors) 1))
          (let* ((external-monitor (car external-monitors))
                 (current-x 0))
            ;; Intel Xe workaround: turn off all monitors first
            (shell-command (format "xrandr --output %s --off --output eDP-1 --off"
                                   (car external-monitor)))
            (sit-for 1)
            ;; External monitor at origin
            (shell-command (format "xrandr --output %s --auto --primary --pos 0x0"
                                   (car external-monitor)))
            (setq current-x (cadr external-monitor))
            ;; Laptop to the right without scaling
            (shell-command (format "xrandr --output eDP-1 --auto --pos %dx0"
                                   current-x))
            ;; Intel Xe workaround: use transform trick
            (sit-for 0.1)
            (shell-command "xrandr --output eDP-1 --transform 1.001,0,0,0,1.001,0,0,0,1")
            (sit-for 0.1)
            (shell-command "xrandr --output eDP-1 --transform none")
            ;; Restart picom with GLX backend
            (when (executable-find "picom")
              (start-process "picom" nil "picom" "-b"))
            (message "EXWM: Intel Xe monitor configuration applied")))
         ;; Only laptop
         ((and has-laptop (= (length external-monitors) 0))
          (message "EXWM: Running xrandr command: xrandr --output eDP-1 --scale 0.67x0.67 --primary --pos 0x0")
          (shell-command "xrandr --output eDP-1 --scale 0.67x0.67 --primary --pos 0x0"))
         ;; No laptop, just external monitors
         (t
          (let ((xrandr-cmd "xrandr"))
            ;; Configure external monitors in sequence
            (when external-monitors
              (dolist (monitor external-monitors)
                (setq xrandr-cmd (format "%s --output %s --auto --pos %dx0"
                                         xrandr-cmd (car monitor) current-x))
                (when (= current-x 0)
                  (setq xrandr-cmd (concat xrandr-cmd " --primary")))
                (setq current-x (+ current-x (cadr monitor)))))
            (message "EXWM: Running xrandr command: %s" xrandr-cmd)
            (shell-command xrandr-cmd))))))



    (defun my/exwm-start-tray-apps ()
      "Start system tray applications with delays to ensure proper icon display."
      (interactive)
      (run-with-timer 1 nil
                      (lambda ()
                        (when (executable-find "nm-applet")
                          (message "Starting nm-applet...")
                          (start-process "nm-applet" nil "nm-applet"))
                        (run-with-timer 0.5 nil
                                        (lambda ()
                                          (when (executable-find "udiskie")
                                            (message "Starting udiskie...")
                                            (start-process "udiskie" nil "udiskie" "-at"))
                                          (run-with-timer 0.5 nil
                                                          (lambda ()
                                                            (when (executable-find "blueman-applet")
                                                              (message "Starting blueman-applet...")
                                                              (start-process "blueman-applet" nil "blueman-applet")))))))))
    ;; Set up randr configuration before enabling randr mode
    (my/exwm-randr-setup)
    (setq exwm-randr-screen-change-hook
          (lambda ()
            (my/exwm-randr-setup)
            (my/exwm-configure-monitors)
            (exwm-randr-refresh)))

    (add-hook 'exwm-init-hook #'my/exwm-start-tray-apps)
    (exwm-systemtray-mode 1)
    (exwm-randr-mode 1)
    (exwm-wm-mode 1)))

  (defun my/app-launcher ()
    "Launch application using \='completing-read'."
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

  (use-package desktop-environment
    :ensure t
    :config
    (setq desktop-environment-screenlock-command "slock")
    (desktop-environment-mode 1))

  ;;; exwm-edit
  (use-package exwm-edit
    :ensure t
    :after exwm
    :config
    (setq exwm-edit-split nil)
    (define-key exwm-mode-map (kbd "C-c C-e") 'exwm-edit--compose)
    (add-hook 'exwm-edit-compose-hook
              (lambda ()
                (message "EXWM Edit buffer created. Use C-c C-c to finish, C-c C-k to cancel.")))
    ;; Override the compose function to use M-w instead of C-c for copying
    (defun exwm-edit--compose (&optional no-copy)
      "Edit text in an EXWM app. Use M-w to copy instead of C-c."
      (interactive)
      (let* ((title (exwm-edit--buffer-title (buffer-name)))
             (existing (get-buffer title))
             (inhibit-read-only t)
             (save-interprogram-paste-before-kill t)
             (selection-coding-system 'utf-8))
        (when (derived-mode-p 'exwm-mode)
          (setq exwm-edit--last-window-configuration (current-window-configuration))
          (if existing
              (switch-to-buffer-other-window existing)
            (exwm-input--fake-key ?\C-a)
            (unless (or no-copy (not exwm-edit-copy-over-contents))
              (when (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
                (setq exwm-edit-last-kill (substring-no-properties (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
              (exwm-input--fake-key ?\M-w))  ; Use M-w instead of C-c
            (with-current-buffer (get-buffer-create title)
              (run-hooks 'exwm-edit-compose-hook)
              (exwm-edit-mode 1)
              (pop-to-buffer (current-buffer) exwm-edit-display-buffer-action)
              (setq-local header-line-format
                          (substitute-command-keys
                           "Edit, then exit with `\\[exwm-edit--finish]' or cancel with `\\[exwm-edit--cancel]'"))
              (unless (or no-copy (not exwm-edit-copy-over-contents))
                (exwm-edit--yank)))))))
    ;; Override the paste function to use C-y instead of C-v
    (defun exwm-edit--send-to-exwm-buffer (text)
      "Send TEXT to the exwm window using C-y."
      (kill-new text)
      (set-window-configuration exwm-edit--last-window-configuration)
      (setq exwm-edit--last-window-configuration nil)
      (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
      (if (string= text "")
          (run-with-timer exwm-edit-paste-delay nil (lambda () (exwm-input--fake-key 'delete)))
        (run-with-timer exwm-edit-paste-delay nil (lambda ()
                                                    (exwm-input--fake-key ?\C-y)
                                                    (run-with-timer 0.1 nil (lambda ()
                                                                              (when kill-ring
                                                                                (kill-new (car kill-ring))))))))))

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

;; Disable auto-revert to prevent EXWM interruptions
(global-auto-revert-mode -1)
(setq auto-revert-interval 999999)  ; Just in case

(defvar my-tmp-dir (expand-file-name "~/.tmp/")
  "Centralized directory for temporary files, backups, and history files.
This keeps the main .emacs.d directory clean and organizes cache files logically.")

(unless (file-exists-p my-tmp-dir)
  (make-directory my-tmp-dir t))

(dolist (dir '("backups"
               "auto-saves"
               "auto-save-list"
               "recentf"
               "eshell"
               "tramp-auto-save"
               "saveplace"
               "undos"
               "gnus-drafts"))
  (let ((subdir (expand-file-name dir my-tmp-dir)))
    (unless (file-exists-p subdir)
      (make-directory subdir t))))

(use-package emacs
  :ensure nil
  :init
  (setq auth-sources '("~/.authinfo.gpg")
        auth-source-save-behavior nil
        auth-source-do-cache nil
        epa-file-cache-passphrase-for-symmetric-encryption nil
        epg-pinentry-mode 'loopback
        epg-gpg-program "gpg2")
  :custom
  (tab-always-indent 'complete)
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  (completions-sort 'historical)
  (completions-header-format nil)
  (completion-styles '(basic partial-completion emacs22 initials flex))
  (completion-category-overrides
   '((file (styles . (basic partial-completion)))
     (buffer (styles . (basic flex)))
     (info-menu (styles . (basic)))))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-max-height 15)
  :config

  ;;; Personal Information
  (setq user-full-name "TJ"
        user-mail-address "tj@emacs.su"
        calendar-location-name "New York, NY"
        calendar-latitude 40.7
        calendar-longitude -74.0)

  (let ((templates-dir "~/.config/emacs/latex/templates/"))
    (when (file-exists-p templates-dir)
      (dolist (file
               (directory-files-recursively templates-dir "\\.el$"))
        (load-file file))))

  (setenv "TZ" "America/New_York")

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")

  (setq create-lockfiles nil
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        backup-by-copying t
        backup-directory-alist `((".*" . ,(expand-file-name "backups" my-tmp-dir)))
        auto-save-default nil  ; Disable auto-save to prevent EXWM interruptions
        auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" my-tmp-dir) t))
        auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my-tmp-dir)
        save-place-file (expand-file-name "saveplace/saveplace" my-tmp-dir))

  (setq history-length 10000
        history-delete-duplicates t
        savehist-file (expand-file-name "savehist" my-tmp-dir)
        savehist-save-minibuffer-history t
        password-cache-expiry nil
        auth-source-cache-expiry nil
        plstore-cache-directory my-tmp-dir)

  (save-place-mode 1)

  (setq truncate-string-ellipsis "…"
        x-stretch-cursor t
        help-window-select t
        echo-keystrokes-help nil
        display-time-load-average t)

  (setq scroll-margin 0
        scroll-step 1
        scroll-conservatively 10000
        scroll-preserve-screen-position t
        scroll-error-top-bottom t
        auto-window-vscroll nil
        mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse t
        fast-but-imprecise-scrolling t)

  (pixel-scroll-precision-mode 1)

  (when (fboundp 'pixel-scroll-precision-mode)
    (setq pixel-scroll-precision-interpolate-page t
          pixel-scroll-precision-large-scroll-height 40.0
          pixel-scroll-precision-interpolation-factor 30))


  ;; Performance optimizations for smooth scrolling
  (setq redisplay-skip-fontification-on-input t
        jit-lock-defer-time 0
        jit-lock-context-time 0
        jit-lock-chunk-size 1000
        jit-lock-stealth-time 0.2
        jit-lock-stealth-nice 0.5
        jit-lock-stealth-verbose nil)

  ;; Cache line moves for better performance
  (setq line-move-visual t
        line-move-ignore-invisible t
        next-screen-context-lines 5)

  ;; Optimize display engine
  (setq-default bidi-display-reordering nil
                bidi-paragraph-direction 'left-to-right)

  ;; Increase cache sizes for better performance
  (setq read-process-output-max
        (cond (my/ultra-high-spec-system-p (* 32 1024 1024))
              (my/high-spec-system-p (* 8 1024 1024))
              (t (* 1024 1024)))
        gc-cons-percentage
        (cond (my/ultra-high-spec-system-p 0.15)
              (my/high-spec-system-p 0.2)
              (t 0.1)))

  ;; Disable line numbers in large buffers for performance
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (> (buffer-size) 100000)
                (display-line-numbers-mode -1))))

  ;; Enable so-long-mode for very long lines
  (global-so-long-mode 1)

  (setq mode-line-compact nil)

  (setq mode-line-position-column-line-format '(" %l:%c"))
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)

  (setq display-time-format "%Y-%m-%d %H:%M"
        display-time-default-load-average nil)
  (display-time-mode 1)

  (require 'battery)
  (when (and battery-status-function
             (not (string-match-p "N/A"
                                  (battery-format "%B"
                                                  (funcall battery-status-function)))))
    (setq battery-mode-line-format "%b%p%%  ")
    (setq battery-mode-line-limit 85)
    (display-battery-mode 1))

  (which-function-mode 1)
  (setq which-func-modes '(prog-mode)
        which-func-unknown "")

  (setq mode-line-right-align-edge 'right-fringe)

  (setq-default mode-line-format
                '("%e"
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
                  mode-line-format-right-align
                  mode-line-misc-info
                  mode-line-end-spaces))

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

  (fset 'yes-or-no-p 'y-or-n-p)

  ;;; Authentication and Encryption
  (require 'auth-source)
  (require 'epa-file)
  (unless (memq 'epa-file-handler file-name-handler-alist)
    (epa-file-enable))

  ;;; Network Security
  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 2048)
  (setq network-security-level 'high)
  (setq nsm-settings-file (expand-file-name "network-security.data" my-tmp-dir))

  ;;; Load Custom File
  (when (and custom-file (file-exists-p custom-file))
    (load custom-file))

  :hook
  ((text-mode . visual-wrap-prefix-mode)
   (before-save . (lambda ()
                    (whitespace-cleanup)))
   (emacs-startup . (lambda ()
                      (global-visual-line-mode 1)
                      (global-auto-revert-mode 1)
                      (line-number-mode 1)
                      (column-number-mode 1)
                      (size-indication-mode 1)
                      (display-time-mode 1)
                      (setq-default display-line-numbers-type t)
                      (setq-default display-line-numbers-width-start t)

                      ;; Enable line numbers only in programming modes
                      (add-hook 'prog-mode-hook #'display-line-numbers-mode)
                      (add-hook 'conf-mode-hook #'display-line-numbers-mode)

                      ;; Explicitly disable line numbers in certain modes
                      (dolist (mode '(lisp-interaction-mode-hook
                                      org-mode-hook
                                      term-mode-hook
                                      eshell-mode-hook
                                      pdf-view-mode-hook
                                      dired-mode-hook
                                      eww-mode-hook
                                      erc-mode-hook
                                      eat-mode-hook))
                        (add-hook mode (lambda () (display-line-numbers-mode -1)))))))

  :bind
  (("C-x k" . kill-current-buffer)
   ("C-x K" . kill-buffer))
  :custom-face
  (mode-line ((t (:box (:line-width -1 :style released-button)))))
  (mode-line-inactive ((t (:box (:line-width -1 :style released-button)))))
  (mode-line-buffer-id ((t (:weight bold :inherit font-lock-keyword-face))))
  (mode-line-emphasis ((t (:weight bold :inherit warning)))))

;;; Minions for minor mode management
(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  :custom
  (minions-prominent-modes '(flymake-mode
                             flycheck-mode
                             lsp-mode
                             eglot--managed-mode))
  (minions-mode-line-lighter " ◎"))

;;; Visual bell in mode-line
(use-package mode-line-bell
  :ensure t
  :config
  (mode-line-bell-mode 1))

;;; Theme Configuration (separate use-package)

;; Ensure modus-themes is available (built-in since Emacs 28)
(use-package modus-themes
  :ensure nil
  :defer t)

(use-package ef-themes
  :ensure t
  :demand t
  :init
  (mapc #'disable-theme custom-enabled-themes)
  :config

  (custom-set-faces
   '(cursor ((t (:background "#FFC107")))))

  (setq ef-themes-to-toggle '(ef-winter ef-summer))
  (keymap-global-set "M-s-<backspace>" 'ef-themes-toggle)
  (setq ef-themes-headings
        '((0 variable-pitch light 1.9)
          (1 variable-pitch light 1.8)
          (2 variable-pitch regular 1.7)
          (3 variable-pitch regular 1.6)
          (4 variable-pitch regular 1.5)
          (5 variable-pitch 1.4)
          (6 variable-pitch 1.3)
          (7 variable-pitch 1.2)
          (t variable-pitch 1.1)))

  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)

  ;; Load theme
  (load-theme 'ef-winter t)

  (defun my-ef-themes-mode-line ()
    "Tweak the style of the mode lines."
    (ef-themes-with-colors
      (custom-set-faces
       `(mode-line ((,c :background ,bg-mode-line :foreground ,fg-mode-line :box (:line-width 1 :color ,fg-dim))))
       `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

  (defun my-ef-themes-custom-faces ()
    "My customizations on top of the Ef themes.
This function is added to the \=`ef-themes-post-load-hook'."
    (ef-themes-with-colors
      (custom-set-faces
       `(font-lock-comment-face ((,c :inherit italic :foreground ,comment)))
       `(font-lock-variable-name-face ((,c :foreground ,variable))))))
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-custom-faces)
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line))

(use-package windower
  :ensure t
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
   ("s-S-<right>" . windower-swap-right)))

;;; shell environment (path, etc)

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (setq exec-path-from-shell-variables '("PATH"))
  (let ((local-bin (expand-file-name "~/.local/bin")))
    (unless (member local-bin exec-path)
      (add-to-list 'exec-path local-bin t)
      (setenv "PATH" (concat local-bin ":" (getenv "PATH")))))
  (exec-path-from-shell-initialize))

;;; ediff settings

(use-package ediff
  :ensure nil
  :defer t

  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-right)

  (ediff-keep-variants nil)

  (ediff-control-frame-parameters
   (cons '(unsplittable . t) ediff-control-frame-parameters))

  :custom-face
  (ediff-current-diff-A ((t (:inherit diff-removed :extend t))))
  (ediff-fine-diff-A ((t (:inherit diff-removed :weight bold))))
  (ediff-current-diff-B ((t (:inherit diff-added :extend t))))
  (ediff-fine-diff-B ((t (:inherit diff-added :weight bold))))

  :config
  (defun my/ediff-quit ()
    "Quit ediff and ensure clean window restoration."
    (interactive)
    (when (bound-and-true-p ediff-control-buffer)
      (ediff-quit t)))

  (defun my/ediff-buffer-with-file ()
    "Compare current buffer with its file on disk.
If buffer is modified, offer to save first."
    (interactive)
    (unless buffer-file-name
      (user-error "Current buffer is not visiting a file"))
    (when (and (buffer-modified-p)
               (y-or-n-p "Buffer is modified.  Save it first? "))
      (save-buffer))
    (ediff-current-file))

  (defun my/ediff-directories ()
    "Compare directories with better default regex."
    (interactive)
    (let ((ediff-default-filtering-regexp ""))
      (call-interactively #'ediff-directories)))

  :hook
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

(use-package diff-mode
  :ensure nil
  :defer t
  :custom-face
  (diff-added ((t (:foreground "green4" :extend t))))
  (diff-removed ((t (:foreground "red3" :extend t))))
  (diff-hunk-header ((t (:inherit font-lock-comment-face :weight bold))))
  (diff-file-header ((t (:inherit font-lock-keyword-face :weight bold)))))

;;; tramp settings

(use-package tramp
  :ensure nil
  :defer t

  :custom
  (tramp-default-method "ssh")
  (tramp-use-scp-direct-remote-copying t)
  (tramp-copy-size-limit (* 4 1024 1024))

  (tramp-verbose 1)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save-visited t)

  (tramp-use-connection-share t)
  (tramp-connection-timeout 10)
  (remote-file-name-inhibit-cache nil)

  (tramp-auto-save-directory (expand-file-name "tramp-auto-save" my-tmp-dir))
  (tramp-persistency-file-name (expand-file-name "tramp-persistence" my-tmp-dir))

  (auto-revert-remote-files t)
  (auto-revert-use-notify nil)

  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)
     (tramp-pipe-stty-settings . "")
     (shell-file-name . "/bin/sh")
     (shell-command-switch . "-c")))

  (connection-local-set-profiles
   '(:application tramp)
   'remote-direct-async-process)

  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options))

  (setq read-process-output-max
        (cond (my/ultra-high-spec-system-p (* 32 1024 1024))
              (my/high-spec-system-p (* 8 1024 1024))
              (t (* 1024 1024))))

  (defun my/tramp-cleanup-all ()
    "Clean all TRAMP connections and buffers."
    (interactive)
    (tramp-cleanup-all-buffers)
    (tramp-cleanup-all-connections)
    (message "TRAMP connections and buffers cleaned"))

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
(define-derived-mode log-mode fundamental-mode "Log"
  "Major mode for viewing log files with syntax highlighting."
  (setq-local font-lock-defaults
              '((("\\<DEBUG\\>" . font-lock-comment-face)
                 ("\\<INFO\\>" . font-lock-string-face)
                 ("\\<WARN\\>" . font-lock-warning-face)
                 ("\\<ERROR\\>" . font-lock-function-name-face)
                 ("\\b[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[T ][0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\b"
                  . font-lock-constant-face)
                 ("\\b[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\b"
                  . font-lock-variable-name-face))))
  (setq buffer-read-only t)
  (auto-revert-tail-mode 1))

(add-to-list 'auto-mode-alist '("\\.log\\'" . log-mode))

;;; Undo Tree Visualization
(use-package vundo
  :ensure t
  :defer t
  :bind ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-files-directory (expand-file-name "vundo" my-tmp-dir))
  (vundo-compact-display t))

;;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :diminish
  :hook ((prog-mode . rainbow-delimiters-mode)
         (conf-mode . rainbow-delimiters-mode)
         (yaml-mode . rainbow-delimiters-mode))
  :custom
  (rainbow-delimiters-max-face-count 9)
  :config
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
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.2)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-display-on-blank-lines t)
  (indent-bars-prefer-character nil)

  (indent-bars-highlight-current-depth '(:blend 0.3))

  (indent-bars-no-descend-strings t)
  (indent-bars-no-descend-lists t)
  (indent-bars-depth-update-delay 0.1)

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
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  (make-backup-files t)
  (vc-make-backup-files t))

;;; Auto-revert Configuration
(use-package autorevert
  :ensure nil
  :diminish (auto-revert-mode . "")
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t))

;;; Modeline Management - Using :diminish instead of delight

;;; Structural Editing
(use-package smartparens
  :ensure t
  :defer t
  :diminish
  :hook ((prog-mode . smartparens-mode)
         (text-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  (add-hook 'minibuffer-setup-hook #'turn-off-smartparens-mode)
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-u" . sp-backward-up-sexp)
        ("C-M-d" . sp-down-sexp)
        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)))

;;; vertico
(use-package vertico
  :ensure t
  :demand t
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
(use-package orderless
  :ensure t
  :demand t
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
(use-package savehist
  :ensure nil
  :init (savehist-mode 1)
  :custom
  (savehist-file (expand-file-name "savehist" my-tmp-dir))
  (savehist-autosave-interval nil)  ; Disable auto-save, only save on exit
  (savehist-additional-variables
   '(kill-ring
     mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history
     vertico-repeat-history)))

;;; marginalia
(use-package marginalia
  :ensure t
  :demand t
  :init (marginalia-mode 1)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

;;; consult
(use-package consult
  :ensure t
  :defer 1
  :custom
  (consult-preview-key '(:debounce 0.3 any))
  (consult-narrow-key "<")

  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)

  (consult-line-numbers-widen t)
  (consult-line-start-from-top t)

  :config
  (consult-customize
   consult-theme :preview-key nil
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))

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
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x C-b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
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
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))

  :init
  (global-set-key [remap Info-search] #'consult-info)
  (global-set-key [remap isearch-forward] #'consult-line)
  (global-set-key [remap recentf-open-files] #'consult-recent-file))

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet)
  :bind ("C-c Y" . consult-yasnippet))

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
  (completion-preview-minimum-symbol-length 3)
  (completion-preview-idle-delay 0)
  (completion-preview-exact-match-only nil)
  (completion-preview-insert-on-completion t)
  :custom-face
  (completion-preview ((t (:inherit shadow :foreground "#FFC107"))))
  (completion-preview-exact ((t (:inherit completion-preview :weight bold))))
  :bind
  (:map completion-preview-active-mode-map
        ("TAB" . completion-preview-insert)
        ([tab] . completion-preview-insert)
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate)))

;;; nerd-icons

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-color-icons t))

(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; expand-region for some reason
(use-package expand-region :ensure t :bind ("C-=" . er/expand-region))

;;; Project.el - Built-in project management
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project" ".projectile" "Makefile" "package.json"
                                   "Cargo.toml" "go.mod" "requirements.txt"))

  :config
  (defun my/project-find-test-or-impl ()
    "Switch between test and implementation files."
    (interactive)
    (let* ((current-file (buffer-file-name))
           (test-patterns '("_test" ".test" "-test" "Test" ".spec"))
           (impl-patterns '("_test" ".test" "-test" "Test" ".spec"))
           (is-test-file (cl-some (lambda (pattern) (string-match-p pattern current-file)) test-patterns)))
      (if is-test-file
          (let ((impl-file (replace-regexp-in-string "_test\\|.test\\|-test\\|Test\\|.spec" "" current-file)))
            (if (file-exists-p impl-file)
                (find-file impl-file)
              (project-find-file)))
        (project-find-file))))
  :bind
  (:map project-prefix-map
        ("t" . my/project-find-test-or-impl)
        ("m" . magit-status)))

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
  :ensure nil
  :demand t
  :diminish
  :custom
  (which-key-idle-delay 0)
  (which-key-idle-secondary-delay 0)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.30)
  (which-key-show-early-on-C-h t)

  (which-key-separator " → ")
  (which-key-prefix-prefix "+")
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-sort-order 'which-key-prefix-then-key-order)

  (which-key-dont-use-unicode nil)
  (which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))

  :config
  (which-key-mode 1)
  (which-key-add-key-based-replacements
    "C-c n" "notes"
    "C-c L" "lsp"
    "C-c 0" "0x0-upload"
    "C-c d" "diff"
    "C-c n q" "query")

  (push '((nil . "\\`\\([[:alnum:]-]+\\)\\+'") . (nil . "\\1+"))
        which-key-replacement-alist))

;;; flyspell

(use-package flyspell
  :ensure nil
  :defer t
  :diminish (flyspell-mode . " ✍")
  :hook
  ((text-mode . flyspell-mode)
   (org-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-dictionary "en_US")
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)

  (setq ispell-alternate-dictionary nil)

  (add-hook 'text-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (remove 'ispell-completion-at-point
                                  completion-at-point-functions))))

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
  :ensure nil
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil)
  (eglot-extend-to-xref t)

  :config
  (setq-default eglot-workspace-configuration
                '((pylsp (plugins (flake8 (enabled . t))
                                  (black (enabled . t))
                                  (pycodestyle (enabled . :json-false))))))

  :hook
  ((python-mode python-ts-mode
                js-mode js-ts-mode
                typescript-mode typescript-ts-mode) . eglot-ensure)

  :bind
  (:map eglot-mode-map
        ("C-c l r" . xref-find-references)
        ("C-c l d" . xref-find-definitions)
        ("C-c l i" . eglot-find-implementation)
        ("C-c l t" . eglot-find-typeDefinition)
        ("C-c l a" . eglot-code-actions)
        ("C-c l R" . eglot-rename)
        ("C-c l f" . eglot-format)
        ("C-c l F" . eglot-format-buffer)))

;;; Consult integration for Eglot (NOT consult-lsp!)
(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :bind
  (:map eglot-mode-map
        ("C-c l s" . consult-eglot-symbols)))


;;; Cape for better completion (works with Eglot)
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :bind
  ("C-c p f" . cape-file)
  ("C-c p d" . cape-dabbrev)
  ("C-c p l" . cape-line))

;;; Org Mode
(use-package org
  :ensure nil
  :defer t
  :commands (org-mode org-agenda org-capture org-store-link)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :custom
  (org-directory "~/Documents/notes/")
  (org-startup-indented t)
  (org-startup-folded 'show)
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-hide-emphasis-markers t)

  (org-agenda-files (list org-directory))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c@)")))

  (org-capture-templates
   '(("t" "Todo" entry (file "todo.org")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
     ("n" "Note" entry (file "notes.org")
      "* %? :NOTE:\n%U\n")))

  (org-export-with-broken-links t)
  (org-html-validation-link nil)

  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"))

  :hook
  (org-mode . visual-line-mode))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("citywide"
                 "\\documentclass{citywide}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (when (executable-find "latexmk")
    (setq org-latex-pdf-process
          '("latexmk -pdf -f -interaction=nonstopmode -output-directory=%o %f"))))

(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-label-border 1)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2))

(use-package ox-gfm
  :ensure t
  :after org)

;;; markdown

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
              ("C-c C-c p" . markdown-preview)
              ("C-c C-c e" . markdown-export)
              ("C-c C-c o" . markdown-open)
              ("C-c C-t" . markdown-toggle-gfm-checkbox))
  :config
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

;;; Magit - Git interface
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :init
  (global-set-key (kbd "C-c g") 'magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-refresh-status-buffer nil)
  (magit-tramp-pipe-stty-settings 'pty)

  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(non-empty-second-line))

  (magit-repository-directories '(("~/Code" . 1))))

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-connector 'sqlite-builtin))

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
  :init
  (setq recentf-auto-cleanup nil  ; Disable automatic cleanup/save timer
        recentf-save-file (expand-file-name "recentf/recentf" my-tmp-dir)
        recentf-max-saved-items 200
        recentf-max-menu-items 25)
  :config
  ;; Cancel any timers that recentf might have created
  (dolist (timer timer-list)
    (when (and (timerp timer)
               (eq (timer--function timer) 'recentf-save-list))
      (cancel-timer timer)))

  ;; Only save when Emacs exits
  (add-hook 'kill-emacs-hook 'recentf-save-list)

  :bind (("C-c r" . consult-recent-file)))

;;; Dired - Directory Editor
(use-package dired
  :ensure nil
  :bind
  ("C-x C-j" . dired-jump)
  :hook
  (dired-mode . hl-line-mode)
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t)

  :config
  (require 'dired-x)
  (setq dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'")

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

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :ensure t
  :config (diredfl-global-mode 1))

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
  (eww-search-prefix "https://duckduckgo.com/html?q=")
  (shr-use-colors t)
  (shr-use-fonts t)
  (shr-max-image-proportion 0.7)
  (shr-width 80)

  :bind
  (:map eww-mode-map
        ("+" . text-scale-increase)
        ("-" . text-scale-decrease)
        ("0" . text-scale-set))

  :hook
  (eww-mode . visual-line-mode))

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
  (unless (featurep 'pdf-tools)
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
  (
   (text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  (:map
   global-map
   ("C-c n n" . denote)
   ("C-c n j" . denote-journal-new-or-existing-entry)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep)

   ("C-c n l" . denote-link)
   ("C-c n L" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n q c" . denote-query-contents-link)
   ("C-c n q f" . denote-query-filenames-link)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)

   :map
   dired-mode-map
   ("C-c C-d C-i" . denote-dired-link-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-R"
    .
    denote-dired-rename-marked-files-using-front-matter))

  :config
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

  (setq denote-date-prompt-use-org-read-date t)

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
  :commands
  (denote-journal-new-entry
   denote-journal-new-or-existing-entry
   denote-journal-link-or-create-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year))

(use-package denote-org :ensure t :after denote :defer t)

;;; treesit

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (python-mode . python-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode))))

;;; Tree-sitter auto - Only for languages that benefit from it
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(python typescript tsx json bash))
  :config
  (global-treesit-auto-mode))

;;; so-long

(use-package so-long :ensure nil :config (global-so-long-mode 1))

;;; Flymake - Built-in syntax checking (works with Eglot!)
(use-package flymake
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-start-on-flymake-mode t)

  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-show-diagnostics-at-end-of-line t)

  :bind
  (:map flymake-mode-map
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! L" . flymake-show-project-diagnostics)
        ("C-c ! e" . display-local-help))

  :config
  (setq flymake-diagnostic-functions
        (append flymake-diagnostic-functions
                '(flymake-proc-legacy-flymake))))


(use-package package-lint-flymake
  :ensure t
  :hook
  (emacs-lisp-mode . package-lint-flymake-setup)
  :config
  (defun my/maybe-enable-package-lint ()
    (when (and (buffer-file-name)
               (string-match-p "\\(?:packages\\|lisp\\)/" (buffer-file-name))
               (not (string-match-p "init\\.el\\|config\\.el" (buffer-file-name))))
      (package-lint-flymake-setup)))

  (remove-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
  (add-hook 'emacs-lisp-mode-hook #'my/maybe-enable-package-lint))


;;; YAsnippet

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :custom
  (yas-snippet-dirs
   (list
    (expand-file-name "snippets/" user-emacs-directory)
    (when (locate-library "yasnippet-snippets")
      (expand-file-name "snippets" (file-name-directory (locate-library "yasnippet-snippets"))))))
  (yas-prompt-functions '(yas-completing-prompt))
  (yas-choose-keys-first t)
  (yas-choose-tables-first t)
  :hook
  (after-init . yas-reload-all)
  :bind
  (:map yas-minor-mode-map
        ("C-c y" . yas-insert-snippet)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (let ((snippets-dir (when (locate-library "yasnippet-snippets")
                        (expand-file-name "snippets" (file-name-directory (locate-library "yasnippet-snippets"))))))
    (when snippets-dir
      (unless (file-directory-p snippets-dir)
        (warn "yasnippet-snippets directory %s not found; consider reinstalling the package" snippets-dir)))))


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
  :config
  (setq 0x0-kill-ring-results t)
  (defvar my-0x0-prefix-map (make-sparse-keymap)
    "Prefix keymap for 0x0 commands.")
  (define-prefix-command 'my-0x0-prefix-map)
  (global-set-key (kbd "C-c 0") 'my-0x0-prefix-map)
  (define-key my-0x0-prefix-map (kbd "f") '0x0-upload-file)
  (define-key my-0x0-prefix-map (kbd "s") '0x0-shorten-uri)
  (define-key my-0x0-prefix-map (kbd "t") '0x0-upload-text)
  (define-key my-0x0-prefix-map (kbd "d") '0x0-dwim)
  (define-key my-0x0-prefix-map (kbd "p") '0x0-popup)
  )

;;; Eshell - Emacs Shell
(use-package eshell
  :ensure nil
  :defer t
  :custom
  (eshell-directory-name (expand-file-name "eshell" my-tmp-dir))
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-prompt-regexp "^[^#$\n]*[#$] ")

  :config
  (setq eshell-prompt-function
        (lambda ()
          (concat (propertize (abbreviate-file-name default-directory)
                              'face 'font-lock-comment-face)
                  (if (= (user-uid) 0) " # " " $ "))))

  (defalias 'eshell/ll 'eshell/ls)
  (defalias 'eshell/la '(lambda () (eshell/ls "-a")))
  (defalias 'eshell/clear 'eshell/clear-scrollback)

  :hook
  (eshell-mode . (lambda () (display-line-numbers-mode -1)))

  :bind
  ("C-c e" . eshell))

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
  "Ensure UI disabled for new frames, ie: daemon clients.

This function explicitly disables \='menu-bar-mode', \='tool-bar-mode',
and \='scroll-bar-mode' for the specified FRAME (or current frame if nil).
This complements the frame parameters set in early-init.el to ensure
robust UI element disabling."
  (with-selected-frame (or frame (selected-frame))
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

(unless (daemonp)
  (my-after-make-frame-setup))

(add-hook 'after-make-frame-functions #'my-after-make-frame-setup)

;;; hl-line

(use-package hl-line
  :ensure nil
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
  (setenv "TERM" "xterm-256color")
  (eat-kill-buffer-on-exit t)
  (eat-query-before-killing-running-terminal nil)

  (eat-enable-blinking-text t)
  (eat-term-scrollback-size 10000)

  (eat-shell-command (list (getenv "SHELL") "-l"))

  (eat-eshell-visual-command-mode-map
   '(("git" . ("log" "diff" "show"))))

  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode)

  :bind
  ("C-c T" . eat))


;;; Claude Code

(use-package claude-code
  :ensure t
  :defer t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c v" . claude-code-command-map))

;;; EditorConfig support optimized for Emacs 30.1

(use-package editorconfig
  :ensure nil
  :demand t
  :config
  (editorconfig-mode 1)

  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode
        editorconfig-get-properties-function
        #'editorconfig-get-properties-from-exec)

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

  (setq editorconfig-exclude-modes
        '(help-mode
          magit-mode
          magit-diff-mode
          dired-mode
          ibuffer-mode
          minibuffer-mode))

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
  (setq proced-auto-update-flag 'visible)
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
  (uniquify-buffer-name-style 'post-forward)
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
  (setq minibuffer-completion-auto-choose nil)
  (setq completions-max-height 20)
  (minibuffer-depth-indicate-mode 1))

;;; Shell (M-x shell)\

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
  (setq shell-command-prompt-show-cwd t)
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t)
  (setq shell-has-auto-cd nil)
  (setq shell-get-old-input-include-continuation-lines t)
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
  :defer t
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

;;; stupid fucking emojis

(use-package emoji
  :ensure nil
  :bind ("C-c e" . emoji-search))

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

;;; ERC

(use-package erc
  :ensure nil
  :defer t
  :config
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

  (setq erc-modules '(autojoin button completion fill irccontrols
                               list log match menu move-to-prompt netsplit
                               networks noncommands readonly ring stamp track
                               sasl))
  (erc-update-modules)

  (setq erc-track-enable-keybindings t
        erc-track-visibility t
        erc-track-position-in-mode-line t
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
        erc-track-exclude-server-buffer t
        erc-track-shorten-start 8
        erc-track-shorten-function nil
        erc-track-switch-direction 'newest
        erc-track-showcount t
        erc-track-showcount-string ":")

  (setq erc-autojoin-channels-alist
        '(("Libera.Chat" "#emacs" "#gnu" "#fsf" "#lisp" "#commonlisp"))
        erc-autojoin-timing 'ident
        erc-autojoin-delay 5)

  (setq erc-join-buffer 'bury
        erc-auto-query 'bury
        erc-query-display 'bury)

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

  (defun my-erc-completion-setup ()
    "Enable completion-preview for ERC."
    (setq-local pcomplete-cycle-completions nil)
    (completion-preview-mode 1))

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
        ("C-c C-b" . my-erc-switch-to-buffer)
        ("C-c C-SPC" . erc-track-switch-buffer)
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

;;; DYNAMIC PERFORMANCE OPTIMIZATIONS

(defun my/apply-performance-optimizations ()
  "Apply performance optimizations based on system specifications."
  (interactive)

  ;; Optimize based on available memory
  (when my/high-spec-system-p
    ;; Increase various limits for high-memory systems
    (setq kill-ring-max (if my/ultra-high-spec-system-p 500 200)
          mark-ring-max 50
          global-mark-ring-max 50
          message-log-max (if my/ultra-high-spec-system-p 20000 10000)
          history-length (if my/ultra-high-spec-system-p 2000 1000)
          savehist-save-minibuffer-history t)

    ;; Optimize file operations
    (setq auto-save-interval 1000
          auto-save-timeout 60
          create-lockfiles nil
          backup-by-copying t
          vc-handled-backends '(Git)
          find-file-visit-truename nil
          vc-follow-symlinks t)

    ;; Optimize font rendering for powerful systems
    (setq font-lock-maximum-decoration t
          jit-lock-stealth-time 0.2
          jit-lock-chunk-size (if my/ultra-high-spec-system-p 8192 4096)
          jit-lock-defer-time 0.05)

    ;; Display optimizations
    (setq fast-but-imprecise-scrolling t
          redisplay-skip-fontification-on-input t
          inhibit-compacting-font-caches t
          highlight-nonselected-windows nil)

    ;; Increase limits for better performance
    (setq recentf-max-saved-items (if my/ultra-high-spec-system-p 2000 1000)
          desktop-restore-eager (if my/ultra-high-spec-system-p 20 10)
          desktop-lazy-verbose nil
          desktop-lazy-idle-delay 5))

  ;; CPU-based optimizations
  (when (>= my/cpu-count 8)
    ;; Enable parallel operations
    (setq native-comp-async-jobs-number
          (cond ((>= my/cpu-count 16) (- my/cpu-count 4))
                ((>= my/cpu-count 12) (- my/cpu-count 2))
                (t (max 4 (/ my/cpu-count 2)))))

    ;; Optimize minibuffer and completion
    (setq enable-recursive-minibuffers t
          minibuffer-depth-indicate-mode t
          max-mini-window-height 0.5
          completion-cycle-threshold 3
          tab-always-indent 'complete))

  ;; LSP optimizations for multicore systems
  (with-eval-after-load 'lsp-mode
    (when (>= my/cpu-count 8)
      (setq lsp-idle-delay 0.1
            lsp-log-io nil
            lsp-completion-provider :none
            lsp-prefer-flymake nil
            lsp-enable-file-watchers t
            lsp-file-watch-threshold
            (if my/ultra-high-spec-system-p 20000 10000))))

  ;; Magit optimizations for large repositories
  (with-eval-after-load 'magit
    (when my/high-spec-system-p
      (setq magit-refresh-status-buffer nil
            magit-diff-highlight-indentation nil
            magit-diff-highlight-trailing nil
            magit-diff-paint-whitespace nil
            magit-diff-highlight-hunk-body nil
            magit-diff-refine-hunk nil)))

  (message "Performance optimizations applied! Memory: %.1fGB, CPUs: %d"
           my/system-memory my/cpu-count))

;; Apply optimizations after init
(add-hook 'emacs-startup-hook #'my/apply-performance-optimizations)

(provide 'init)
;;; init.el ends here
