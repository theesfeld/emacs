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
;;; Code:

(defun my/get-system-memory ()
  "Get system memory in GB."
  (if (fboundp 'memory-info)
      (/ (car (memory-info)) 1024.0)
    32))

(defconst my/system-memory (my/get-system-memory)
  "System memory in GB.")

(defconst my/cpu-count (or (num-processors) 4)
  "Number of CPU cores.")

(defconst my/high-spec-system-p
  (and (>= my/system-memory 32) (>= my/cpu-count 8))
  "Non-nil if system has high specifications.")

(defconst my/ultra-high-spec-system-p
  (and (>= my/system-memory 64) (>= my/cpu-count 12))
  "Non-nil if system has ultra-high specifications.")

(defconst my/tmp-dir (expand-file-name "~/.tmp/")
  "Directory for temporary files.")

(unless (file-directory-p my/tmp-dir)
  (make-directory my/tmp-dir t))

(setq package-vc-register-as-project nil)

(use-package gcmh
  :ensure t
  :demand t
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 256 1024 1024))
  :config
  (gcmh-mode 1))

(setenv "GPG_AGENT_INFO" nil)

(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)

(when (fboundp 'pinentry-start)
  (pinentry-start))

(defun my-common-auth-get-field (host prop)
  "Find PROP in `auth-sources' for HOST entry."
  (when-let* ((source (car (auth-source-search :host host))))
    (if (eq prop :secret)
        (funcall (plist-get source prop))
      (plist-get source prop))))

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

(defun my/program-launcher ()
  "Launch program using completion with command history."
  (interactive)
  (let* ((history-commands
          (when (boundp 'shell-command-history)
            (delete-dups (copy-sequence shell-command-history))))
         (common-commands
          '("firefox" "mpv" "emacs" "pavucontrol" "thunar" "gimp"
            "inkscape"))
         (all-commands
          (delete-dups
           (append history-commands common-commands)))
         (command
          (completing-read "Launch program: " all-commands nil nil nil
                           'shell-command-history)))
    (when (and command (not (string-empty-p command)))
      (start-process-shell-command command nil command)
      (push command shell-command-history))))

(defun increase-text-and-pane ()
  "Increase text size."
  (interactive)
  (text-scale-increase 1))

(defun decrease-text-and-pane ()
  "Decrease text size."
  (interactive)
  (text-scale-decrease 1))

(defun my/sanitize-filename (name)
  "Convert NAME to safe filename by replacing invalid characters."
  (replace-regexp-in-string
   "[/\\:*?\"<>|]" "_"
   (replace-regexp-in-string "^[*]\\|[*]$" "" name)))

(defun my/get-buffer-screenshot-name ()
  "Get meaningful name for screenshot from current buffer."
  (let* ((orig-buffer (current-buffer))
         (name (with-current-buffer orig-buffer
                 (cond
                  ((and (boundp 'exwm-class-name) exwm-class-name)
                   exwm-class-name)
                  ((string-match "\\*.*\\*" (buffer-name))
                   (format "emacs-%s" (substring (buffer-name) 1 -1)))
                  (t (buffer-name))))))
    (my/sanitize-filename name)))

(defun my/screenshot-to-kill-ring ()
  "Take a screenshot, save to ~/Pictures/Screenshots and add to kill-ring."
  (interactive)
  (let* ((buffer-prefix (my/get-buffer-screenshot-name))
         (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S"))
         (filename (expand-file-name
                   (format "%s_%s.png" buffer-prefix timestamp)
                   "~/Pictures/Screenshots/"))
         (dir (file-name-directory filename)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (call-process "scrot" nil 0 nil filename)
    (sleep-for 0.1)
    (when (file-exists-p filename)
      (let ((image-data (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally filename)
                         (buffer-string))))
        (kill-new image-data))
      (call-process "xclip" nil 0 nil
                    "-selection" "clipboard"
                    "-t" "image/png"
                    "-i" filename)
      (message "Screenshot saved to %s and added to kill-ring" filename))))

(defun my/screenshot-selection-to-kill-ring ()
  "Take a partial screenshot, save to ~/Pictures/Screenshots and add to kill-ring."
  (interactive)
  (let* ((buffer-prefix (my/get-buffer-screenshot-name))
         (timestamp (format-time-string "%Y-%m-%d_%H-%M-%S"))
         (filename (expand-file-name
                   (format "%s_%s_selection.png" buffer-prefix timestamp)
                   "~/Pictures/Screenshots/"))
         (dir (file-name-directory filename)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (call-process "scrot" nil 0 nil "-s" filename)
    (sleep-for 0.1)
    (when (file-exists-p filename)
      (let ((image-data (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally filename)
                         (buffer-string))))
        (kill-new image-data))
      (call-process "xclip" nil 0 nil
                    "-selection" "clipboard"
                    "-t" "image/png"
                    "-i" filename)
      (message "Screenshot saved to %s and added to kill-ring" filename))))

(declare-function corfu-next "corfu")
(declare-function corfu-previous "corfu")
(declare-function corfu-complete "corfu")
(declare-function corfu-quit "corfu")

(font-spec :name "AporeticSansMono Nerd Font")
(set-face-attribute 'default nil
                    :height 190)

(font-spec :name "AporeticSerif Nerd Font")
(set-face-attribute 'variable-pitch nil
                    :height 150)

(defun my/prog-mode-variable-pitch-comments ()
  "Use variable-pitch font for comments in programming modes."
  (face-remap-add-relative 'font-lock-comment-face
                           '(:inherit
                             (font-lock-comment-face variable-pitch)))
  (face-remap-add-relative 'font-lock-comment-delimiter-face
                           '(:inherit
                             (font-lock-comment-delimiter-face
                              variable-pitch)))
  (face-remap-add-relative 'font-lock-doc-face
                           '(:inherit
                             (font-lock-doc-face variable-pitch))))

(add-hook 'prog-mode-hook #'my/prog-mode-variable-pitch-comments)

(use-package ef-themes
  :ensure t
  :defer 0.1
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
       `(font-lock-variable-name-face ((,c :foreground ,variable)))
       `(vc-state-base ((,c :inherit italic :height 0.85)))
       `(vc-up-to-date-state ((,c :inherit (vc-state-base font-lock-comment-face))))
       `(vc-edited-state ((,c :inherit (vc-state-base font-lock-warning-face))))
       `(vc-locally-added-state ((,c :inherit (vc-state-base font-lock-string-face))))
       `(vc-locked-state ((,c :inherit (vc-state-base font-lock-function-name-face))))
       `(vc-missing-state ((,c :inherit (vc-state-base font-lock-keyword-face))))
       `(vc-needs-update-state ((,c :inherit (vc-state-base font-lock-variable-name-face))))
       `(vc-removed-state ((,c :inherit (vc-state-base font-lock-keyword-face))))
       `(vc-conflict-state ((,c :inherit (vc-state-base error))))
       `(vc-unregistered-state ((,c :inherit (vc-state-base shadow)))))))
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-custom-faces)
  (add-hook 'ef-themes-post-load-hook #'my-ef-themes-mode-line))

(when (eq window-system 'x)
  (use-package exwm
    :ensure nil
    :init
    (require 'exwm-randr)
    (require 'exwm-systemtray)
    :config
    (setq exwm-workspace-number 5)
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)
    (setq exwm-input-prefix-keys
          '(?\C-x
            ?\C-u
            ?\C-h
            ?\M-x
            ?\M-:
            ?\C-\M-j
            ?\C-\\
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
                         (start-process-shell-command command nil
                                                      command)))
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-m] . exwm-workspace-move-window)
            ([?\s-f] . exwm-floating-toggle-floating)
            ([?\s-F] . exwm-layout-toggle-fullscreen)
            ([s-left] . windmove-left)
            ([s-right] . windmove-right)
            ([s-up] . windmove-up)
            ([s-down] . windmove-down)
            ([?\s-0]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 0)))
            ([?\s-1]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 1)))
            ([?\s-2]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 2)))
            ([?\s-3]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 3)))
            ([?\s-4]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 4)))
            ([?\s-5]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 5)))
            ([?\s-6]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 6)))
            ([?\s-7]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 7)))
            ([?\s-8]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 8)))
            ([?\s-9]
             . (lambda () (interactive)
                 (exwm-workspace-switch-create 9)))
            ([XF86AudioLowerVolume]
             . desktop-environment-volume-decrement)
            ([XF86AudioRaiseVolume]
             . desktop-environment-volume-increment)
            ([XF86AudioMute] . desktop-environment-toggle-mute)
            ([XF86AudioMicMute]
             . desktop-environment-toggle-microphone-mute)
            ([XF86MonBrightnessUp]
             . desktop-environment-brightness-increment)
            ([XF86MonBrightnessDown]
             . desktop-environment-brightness-decrement)
            ([print] . my/screenshot-to-kill-ring)
            ([S-print] . my/screenshot-selection-to-kill-ring)))
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
     (setq exwm-systemtray-workspace nil)

    (defun my/exwm-randr-setup ()
      "Set up monitor configuration with eDP-1 as workspace 0, externals left to right."
      (let* ((xrandr-output (shell-command-to-string "xrandr"))
             (monitor-lines (seq-filter (lambda (line)
                                        (string-match-p " connected" line))
                                      (split-string xrandr-output "\n")))
             (monitor-info (mapcar (lambda (line)
                                   (when (string-match "\\([^ ]+\\) connected.*\\+\\([0-9]+\\)\\+" line)
                                     (cons (match-string 1 line)
                                           (string-to-number (match-string 2 line)))))
                                 monitor-lines))
             (monitor-info (seq-filter #'identity monitor-info))
             (external-monitors (seq-sort (lambda (a b)
                                          (< (cdr a) (cdr b)))
                                        (seq-filter (lambda (m)
                                                    (not (string= (car m) "eDP-1")))
                                                  monitor-info)))
             (external-monitor-names (mapcar #'car external-monitors))
             (has-laptop (assoc "eDP-1" monitor-info))
             workspace-plist)
        (when has-laptop
          (setq workspace-plist (list 0 "eDP-1")))
        (let ((workspace-num (if has-laptop 1 0))
              (monitor-index 0))
          (while (and (< workspace-num 10) (< monitor-index (length external-monitor-names)))
            (setq workspace-plist (append workspace-plist
                                        (list workspace-num (nth monitor-index external-monitor-names))))
            (setq workspace-num (1+ workspace-num))
            (setq monitor-index (1+ monitor-index))))
        (let ((all-monitor-names (if has-laptop
                                   (cons "eDP-1" external-monitor-names)
                                 external-monitor-names))
              (num-monitors (+ (if has-laptop 1 0) (length external-monitor-names))))
          (when (and (> num-monitors 0) (< (/ (length workspace-plist) 2) 10))
            (while (< (/ (length workspace-plist) 2) 10)
              (let* ((workspace-num (/ (length workspace-plist) 2))
                     (monitor-index (mod workspace-num num-monitors))
                     (monitor-name (nth monitor-index all-monitor-names)))
                (setq workspace-plist (append workspace-plist
                                            (list workspace-num monitor-name)))))))
        (setq exwm-randr-workspace-monitor-plist workspace-plist)))

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
             (xrandr-cmd "xrandr")
             (current-x 0))
        (cond
         ((and has-laptop (null external-monitors))
          (shell-command "xrandr --output eDP-1 --scale 0.67x0.67 --primary --pos 0x0"))
         (external-monitors
          (dolist (monitor external-monitors)
            (setq xrandr-cmd (format "%s --output %s --auto --pos %dx0"
                                   xrandr-cmd (car monitor) current-x))
            (when (= current-x 0)
              (setq xrandr-cmd (concat xrandr-cmd " --primary")))
            (setq current-x (+ current-x (cadr monitor))))
          (when has-laptop
            (setq xrandr-cmd (format "%s --output eDP-1 --auto --scale 1x1 --pos %dx0"
                                   xrandr-cmd current-x)))
          (shell-command xrandr-cmd))
         (t
          (message "No monitors detected")))))

    (defun my/exwm-start-tray-apps ()
      "Start system tray applications with delays to ensure proper icon display."
      (interactive)
      (run-with-timer 1 nil
                      (lambda ()
                        (when (executable-find "nm-applet")
                          (start-process "nm-applet" nil "nm-applet"))
                        (run-with-timer 0.5 nil
                                        (lambda ()
                                          (when
                                              (executable-find
                                               "udiskie")
                                            (start-process "udiskie"
                                                           nil
                                                           "udiskie"
                                                           "-at"))
                                          (run-with-timer 0.5 nil
                                                          (lambda ()
                                                            (when
                                                                (executable-find
                                                                 "blueman-applet")
                                                              (start-process
                                                               "blueman-applet"
                                                               nil
                                                               "blueman-applet")))))))))
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

(defalias 'my/app-launcher 'my/program-launcher)

(defalias 'my/exwm-run-program 'my/program-launcher)

(use-package desktop-environment
  :ensure t
  :defer 1
  :config
  (setq desktop-environment-screenlock-command "slock")
  (setq desktop-environment-screenshot-directory
        "~/Pictures/Screenshots/")
  (setq desktop-environment-screenshot-command
        (concat "scrot '%Y-%m-%d_%H-%M-%S_$wx$h.png' -e "
                "'mv $f ~/Pictures/Screenshots/ && xclip -selection clipboard -t image/png -i ~/Pictures/Screenshots/$f'"))
  (setq desktop-environment-screenshot-partial-command
        (concat "scrot -s '%Y-%m-%d_%H-%M-%S_$wx$h_selection.png' -e "
                "'mv $f ~/Pictures/Screenshots/ && xclip -selection clipboard -t image/png -i ~/Pictures/Screenshots/$f'"))
  (desktop-environment-mode 1))

(use-package desktop
  :ensure nil
  :defer t
  :custom
  (desktop-save-mode nil)
  (desktop-restore-eager (if my/ultra-high-spec-system-p 20 10))
  (desktop-lazy-verbose nil)
  (desktop-lazy-idle-delay 5))

(use-package exwm-edit
  :ensure t
  :after exwm
  :config
  (setq exwm-edit-split nil)
  (define-key exwm-mode-map (kbd "C-c C-e") 'exwm-edit--compose)
  (add-hook 'exwm-edit-compose-hook
            (lambda ()
              (message
               "EXWM Edit buffer created. Use C-c C-c to finish, C-c C-k to cancel.")))

  (defun exwm-edit--compose (&optional no-copy)
    "Edit text in an EXWM app. Use M-w to copy instead of C-c."
    (interactive)
    (let* ((title (exwm-edit--buffer-title (buffer-name)))
           (existing (get-buffer title))
           (inhibit-read-only t)
           (save-interprogram-paste-before-kill t)
           (selection-coding-system 'utf-8))
      (when (derived-mode-p 'exwm-mode)
        (setq exwm-edit--last-window-configuration
              (current-window-configuration))
        (if existing
            (switch-to-buffer-other-window existing)
          (exwm-input--fake-key ?\C-a)
          (unless (or no-copy (not exwm-edit-copy-over-contents))
            (when (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
              (setq exwm-edit-last-kill
                    (substring-no-properties
                     (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
            (exwm-input--fake-key ?\M-w))
          (with-current-buffer (get-buffer-create title)
            (run-hooks 'exwm-edit-compose-hook)
            (exwm-edit-mode 1)
            (pop-to-buffer (current-buffer)
                           exwm-edit-display-buffer-action)
            (setq-local header-line-format
                        (substitute-command-keys
                         "Edit, then exit with `\\[exwm-edit--finish]' or cancel with `\\[exwm-edit--cancel]'"))
            (unless (or no-copy (not exwm-edit-copy-over-contents))
              (exwm-edit--yank)))))))
  (defun exwm-edit--send-to-exwm-buffer (text)
    "Send TEXT to the exwm window using C-y."
    (kill-new text)
    (set-window-configuration exwm-edit--last-window-configuration)
    (setq exwm-edit--last-window-configuration nil)
    (exwm-input--set-focus
     (exwm--buffer->id (window-buffer (selected-window))))
    (if (string= text "")
        (run-with-timer exwm-edit-paste-delay nil
                        (lambda () (exwm-input--fake-key 'delete)))
      (run-with-timer exwm-edit-paste-delay nil (lambda ()
                                                  (exwm-input--fake-key
                                                   ?\C-y)
                                                  (run-with-timer 0.1
                                                                  nil
                                                                  (lambda
                                                                    ()
                                                                    (when
                                                                        kill-ring
                                                                      (kill-new
                                                                       (car
                                                                        kill-ring))))))))))

(use-package vc
  :ensure nil
  :defer 1
  :config
    (when my/high-spec-system-p
    (setq vc-handled-backends '(Git)))
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


(dolist (dir '("backups"
               "auto-saves"
               "auto-save-list"
               "recentf"
               "eshell"
               "tramp-auto-save"
               "saveplace"
               "undos"
               "gnus-drafts"))
  (let ((subdir (expand-file-name dir my/tmp-dir)))
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
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles . (basic partial-completion)))
     (buffer (styles . (basic flex)))
     (info-menu (styles . (basic)))))
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-max-height 15)
  (use-short-answers t)
  (completion-eager-display t)
  (completion-show-help t)
  (completion-auto-wrap t)
  :config
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
        backup-directory-alist
        `((".*" . ,(expand-file-name "backups" my/tmp-dir)))
        auto-save-default t
        auto-save-interval 300
        auto-save-timeout 30
        auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-saves/" my/tmp-dir) t))
        auto-save-list-file-prefix
        (expand-file-name "auto-save-list/.saves-" my/tmp-dir)
        auto-save-visited-interval 120
        save-place-file
        (expand-file-name "saveplace/saveplace" my/tmp-dir))
  (auto-save-visited-mode 1)
  (defun my/cleanup-old-temp-files ()
    "Clean up old files in temp directories."
    (interactive)
    (let ((cutoff-time (- (float-time) (* 7 24 60 60))))
      (dolist (dir '("auto-saves" "backups" "auto-save-list"))
        (let ((full-dir (expand-file-name dir my/tmp-dir)))
          (when (file-directory-p full-dir)
            (dolist (file (directory-files full-dir t "^[^.]"))
              (when (and (file-regular-p file)
                         (<
                          (float-time (nth 5 (file-attributes file)))
                          cutoff-time))
                (delete-file file))))))))
  (setq history-length 10000
        history-delete-duplicates t
        password-cache-expiry nil
        auth-source-cache-expiry nil
        plstore-cache-directory my/tmp-dir)
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
  (setq redisplay-skip-fontification-on-input t
        jit-lock-defer-time 0
        jit-lock-context-time 0
        jit-lock-chunk-size 1000
        jit-lock-stealth-time 0.2
        jit-lock-stealth-nice 0.5
        jit-lock-stealth-verbose nil)
  (setq line-move-visual t
        line-move-ignore-invisible t
        next-screen-context-lines 5)
  (setq-default bidi-display-reordering nil
                bidi-paragraph-direction 'left-to-right)
  (setq read-process-output-max
        (cond (my/ultra-high-spec-system-p (* 32 1024 1024))
              (my/high-spec-system-p (* 8 1024 1024))
              (t (* 1024 1024))))
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (> (buffer-size) 100000)
                (display-line-numbers-mode -1))))
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
                                                  (funcall
                                                   battery-status-function)))))
    (setq battery-mode-line-format "%b%p%%  ")
    (setq battery-mode-line-limit 85)
    (display-battery-mode 1))
  (which-function-mode 1)
  (setq which-func-modes '(prog-mode)
        which-func-unknown "")
  (setq mode-line-right-align-edge 'right-fringe)
  (setq vc-follow-symlinks t)
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-modified
                  mode-line-buffer-identification
                  " "
                  mode-line-position
                  mode-line-modes
                  mode-line-format-right-align
                  vc-mode
                  " "
                  mode-line-misc-info))
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
  (setq isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        lazy-count-suffix-format nil)
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t
        remote-file-name-inhibit-auto-save t)
  (setq fast-read-process-output t
        garbage-collection-messages nil)
  (setq confirm-kill-processes nil
        confirm-kill-emacs nil
        shell-kill-buffer-on-exit t
        window-combination-resize t
        eval-expression-print-length nil
        next-error-recenter '(4)
        find-library-include-other-files nil)
  (fset 'yes-or-no-p 'y-or-n-p)
  (require 'auth-source)
  (require 'epa-file)
  (unless (memq 'epa-file-handler file-name-handler-alist)
    (epa-file-enable))
  (setq gnutls-verify-error t)
  (setq gnutls-min-prime-bits 2048)
  (setq network-security-level 'high)
  (setq nsm-settings-file
        (expand-file-name "network-security.data" my/tmp-dir))
  (when (and custom-file (file-exists-p custom-file))
    (load custom-file))
  (when my/high-spec-system-p
    (setq kill-ring-max (if my/ultra-high-spec-system-p 500 200)
          mark-ring-max 50
          global-mark-ring-max 50
          message-log-max (if my/ultra-high-spec-system-p 20000 10000)))
  (when my/high-spec-system-p
    (setq auto-save-interval 1000
          auto-save-timeout 60
          find-file-visit-truename nil))
  (when my/high-spec-system-p
    (setq font-lock-maximum-decoration t
          jit-lock-chunk-size (if my/ultra-high-spec-system-p 8192 4096)
          jit-lock-defer-time 0.05
          inhibit-compacting-font-caches t
          highlight-nonselected-windows nil))
  (when (>= my/cpu-count 8)
    (setq enable-recursive-minibuffers t
          minibuffer-depth-indicate-mode t
          max-mini-window-height 0.5
          completion-cycle-threshold 3))
  :hook
  ((text-mode . visual-wrap-prefix-mode)
   (before-save . (lambda ()
                    (whitespace-cleanup)))
   (emacs-startup . (lambda ()
                      (global-visual-line-mode 1)
                      (line-number-mode 1)
                      (column-number-mode 1)
                      (size-indication-mode 1)
                      (display-time-mode 1)
                      (setq-default display-line-numbers-type t)
                      (setq-default display-line-numbers-width-start t)
                      (add-hook 'prog-mode-hook
                                #'display-line-numbers-mode)
                      (add-hook 'conf-mode-hook
                                #'display-line-numbers-mode)
                      (dolist (mode '(lisp-interaction-mode-hook
                                      org-mode-hook
                                      term-mode-hook
                                      eshell-mode-hook
                                      pdf-view-mode-hook
                                      dired-mode-hook
                                      eww-mode-hook
                                      erc-mode-hook
                                      eat-mode-hook))
                        (add-hook mode
                                  (lambda ()
                                    (display-line-numbers-mode -1)))))))
  :bind
  (("C-x k" . kill-current-buffer)
   ("C-x K" . kill-buffer)
   ("M-y" . consult-yank-pop)
   ("C-i" . consult-imenu)
   ("s-=" . increase-text-and-pane)
   ("s--" . decrease-text-and-pane)
   ("<print>" . my/screenshot-to-kill-ring)
   ("S-<print>" . my/screenshot-selection-to-kill-ring)
   ("s-SPC" . my/program-launcher)
   ("C-c 0" . my-0x0-prefix-map))
  :custom-face
  (mode-line ((t (:box (:line-width -1 :style released-button)))))
  (mode-line-inactive
   ((t (:box (:line-width -1 :style released-button)))))
  (mode-line-buffer-id
   ((t (:weight bold :inherit font-lock-keyword-face))))
  (mode-line-emphasis ((t (:weight bold :inherit warning)))))

(use-package diminish
  :ensure t
  :demand t
  :config
  (diminish 'eldoc-mode)
  (diminish 'abbrev-mode)
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'auto-fill-function))

(defun my/flash-mode-line ()
  "Flash the mode line as a visual bell."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function 'my/flash-mode-line)

(use-package winner
  :ensure nil
  :defer 0.1
  :config
  (winner-mode 1)
  :bind
  (("C-c <left>" . winner-undo)
   ("C-c <right>" . winner-redo)))

(use-package exec-path-from-shell
  :ensure t
  :defer 0.2
  :config
  (setq exec-path-from-shell-shell-name "/bin/bash")
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (setq exec-path-from-shell-variables '("PATH"))
  (let ((local-bin (expand-file-name "~/.local/bin")))
    (unless (member local-bin exec-path)
      (add-to-list 'exec-path local-bin t)
      (setenv "PATH" (concat local-bin ":" (getenv "PATH")))))
  (exec-path-from-shell-initialize))

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
                          (define-key ediff-mode-map (kbd "Q")
                                      #'my/ediff-quit)
                          (define-key ediff-mode-map (kbd "q")
                                      #'my/ediff-quit)))
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
  (diff-hunk-header
   ((t (:inherit font-lock-comment-face :weight bold))))
  (diff-file-header
   ((t (:inherit font-lock-keyword-face :weight bold)))))

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
  (tramp-auto-save-directory
   (expand-file-name "tramp-auto-save" my/tmp-dir))
  (tramp-persistency-file-name
   (expand-file-name "tramp-persistence" my/tmp-dir))
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

(define-derived-mode log-mode fundamental-mode
  "Log"
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

(use-package vundo
  :ensure t
  :defer t
  :bind ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-files-directory (expand-file-name "vundo" my/tmp-dir))
  (vundo-compact-display t))

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
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :weight
                      'bold))

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
                                 :underline
                                 (:color "#5e81ac" :style line))))))

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
          while_statement switch_statement class_specifier
          namespace_definition)
     (rust function_item impl_item match_expression if_expression)
     (javascript function_declaration function_expression
                 arrow_function
                 class_declaration if_statement for_statement)
     (typescript function_declaration function_expression
                 arrow_function
                 class_declaration if_statement for_statement)
     (go function_declaration method_declaration if_statement
         for_statement switch_statement))))

(use-package files
  :ensure nil
  :defer t
  :custom
  (make-backup-files t)
  (vc-make-backup-files t))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 5)  ; Check every 5 seconds (default)
  (auto-revert-verbose nil)  ; Don't show messages for every revert
  (global-auto-revert-non-file-buffers t)  ; Also revert buffers like Dired
  (auto-revert-check-vc-info t)  ; Update version control info
  (auto-revert-avoid-polling t)  ; Use file notification if available
  (auto-revert-use-notify t))  ; Use file system notifications

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)
         (text-mode . electric-pair-mode))
  :config
  (setq electric-pair-preserve-balance t
        electric-pair-delete-adjacent-pairs t
        electric-pair-skip-self 'electric-pair-conservative-skip
        electric-pair-skip-whitespace nil
        electric-pair-skip-whitespace-chars '(?\t ?\n ?\r))
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (when (and (boundp 'electric-pair-mode) electric-pair-mode)
                (electric-pair-local-mode -1)))))

(use-package vertico
  :ensure t
  :defer 0.5
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-preselect 'first)
  (vertico-sort-function #'vertico-sort-history-alpha)
  :init
  (vertico-mode 1)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)
        ("C-j" . vertico-exit-input)))

(use-package orderless
  :ensure t
  :defer 0.5
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion orderless))
     (buffer (styles basic orderless))
     (eglot (styles orderless))
     (eglot-capf (styles orderless))))
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp))
  (orderless-smart-case t)
  (orderless-style-dispatchers nil)
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file (expand-file-name "savehist" my/tmp-dir))
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history
                                   shell-command-history
                                   file-name-history)))

(use-package marginalia
  :ensure t
  :defer 0.5
  :after vertico
  :init (marginalia-mode 1)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

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
  (consult-imenu-config
   '((emacs-lisp-mode :toplevel "Packages"
                      :types ((?f "Functions" font-lock-function-name-face)
                              (?m "Macros" font-lock-function-name-face)
                              (?p "Packages" font-lock-constant-face)
                              (?t "Types" font-lock-type-face)
                              (?v "Variables" font-lock-variable-name-face)))))
  :config
  (consult-customize
   consult-theme :preview-key nil
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any))
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
   ([remap Info-search] . consult-info)
   ([remap isearch-forward] . consult-line)
   ([remap recentf-open-files] . consult-recent-file)
   ("M-y" . consult-yank-pop)
   ("C-i" . consult-imenu)
   ("C-c r" . consult-recent-file)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ("M-s l" . consult-line)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 200)
  (setq recentf-max-saved-items 200))

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet)
  :bind ("C-c Y" . consult-yasnippet))

(use-package corfu
  :ensure t
  :defer t
  :diminish
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 3)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-scroll-margin 5)
  (corfu-popupinfo-delay 0.5)
  (corfu-echo-documentation nil)
  (corfu-separator ?\s)
  :bind
  (:map corfu-map
        ("TAB" . corfu-complete)
        ([tab] . corfu-complete)
        ("RET" . nil)
        ([return] . nil)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<down>" . corfu-next)
        ("<up>" . corfu-previous)
        ("C-g" . corfu-quit)
        ("ESC" . corfu-quit)
        ("SPC" . nil)))

(use-package completion-preview
  :ensure nil
  :hook ((prog-mode text-mode) . completion-preview-mode)
  :custom
  (completion-preview-idle-delay 0.2)
  (completion-preview-minimum-prefix-length 2)
  (completion-preview-insert-on-completion t)
  (completion-preview-exact-match-only nil)
  :config
  (setq completion-preview-sort-function
        (lambda (completions)
          (seq-sort-by #'length #'< completions)))
  :bind
  (:map completion-preview-mode-map
        ("TAB" . completion-preview-insert)
        ("M-TAB" . completion-preview-complete)
        ("C-n" . completion-preview-next)
        ("C-p" . completion-preview-prev)))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "AporeticSansMono Nerd Font")
  (nerd-icons-color-icons t))

(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook
            #'nerd-icons-completion-marginalia-setup))

(use-package expand-region
  :ensure t
  :defer t
  :bind ("C-=" . er/expand-region))

(use-package project
  :ensure nil
  :defer t
  :custom
  (project-vc-extra-root-markers '(".project" ".projectile" "Makefile"
                                   "package.json"
                                   "Cargo.toml" "go.mod"
                                   "requirements.txt"))
  :config
  (defun my/project-find-test-or-impl ()
    "Switch between test and implementation files."
    (interactive)
    (let* ((current-file (buffer-file-name))
           (test-patterns '("_test" ".test" "-test" "Test" ".spec"))
           (impl-patterns '("_test" ".test" "-test" "Test" ".spec"))
           (is-test-file
            (cl-some
             (lambda (pattern) (string-match-p pattern current-file))
             test-patterns)))
      (if is-test-file
          (let
              ((impl-file
                (replace-regexp-in-string
                 "_test\\|.test\\|-test\\|Test\\|.spec" ""
                 current-file)))
            (if (file-exists-p impl-file)
                (find-file impl-file)
              (project-find-file)))
        (project-find-file))))
  :bind
  (:map project-prefix-map
        ("t" . my/project-find-test-or-impl)
        ("m" . magit-status)))

(use-package kmacro
  :ensure nil
  :bind
  (("C-x (" . kmacro-start-macro)
   ("C-x )" . kmacro-end-macro)
   ("C-x e" . kmacro-end-and-call-macro))
  :config
  (setq kmacro-ring-max 20)
  (defun my/apply-macro-to-region-lines ()
    "Apply the last keyboard macro to each line in the region."
    (interactive)
    (if (use-region-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (goto-char start)
          (while (< (point) end)
            (beginning-of-line)
            (kmacro-call-macro nil)
            (forward-line 1)))
      (message "No region selected")))
  :bind (:map kmacro-keymap
              ("r" . my/apply-macro-to-region-lines)))

(use-package diff-hl
  :ensure t
  :defer t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode +1))

(use-package which-key
  :ensure nil
  :defer 1
  :diminish
  :custom
  (which-key-idle-delay 0.8)
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

(use-package flyspell
  :ensure nil
  :defer t
  :diminish
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
  (when (>= my/cpu-count 8)
    (setq eglot-events-buffer-size 0
          eglot-connect-timeout 30))
  :hook
  ((python-mode python-ts-mode
                js-mode js-ts-mode
                typescript-mode typescript-ts-mode)
   . eglot-ensure)
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

(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :bind
  (:map eglot-mode-map
        ("C-c l s" . consult-eglot-symbols)))

(use-package org
  :ensure nil
  :defer t
  :commands (org-mode org-agenda org-capture org-store-link org-refile)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c r" . org-refile)
   ("C-c x" . org-archive-subtree-default)
   ("C-c o d" . my/org-daily-review))
  :custom
  (org-directory "~/org/")
  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                          (expand-file-name "tasks.org" org-directory)
                          (expand-file-name "projects.org" org-directory)
                          (expand-file-name "someday.org" org-directory)))
  (org-archive-location (concat (expand-file-name "archive.org" org-directory) "::* From %s"))
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-return-follows-link t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (org-todo-keyword-faces
   '(("TODO" . org-warning)
     ("NEXT" . (:foreground "blue" :weight bold))
     ("WAITING" . (:foreground "orange" :weight bold))
     ("DONE" . org-done)
     ("CANCELLED" . (:foreground "gray" :weight bold))))
  (org-capture-templates
   '(("t" "Task" entry (file "inbox.org")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a")
     ("n" "Note" entry (file "inbox.org")
      "* %? :NOTE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i")
     ("m" "Meeting" entry (file "inbox.org")
      "* MEETING with %? :MEETING:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Date: %^{Date}U\n** Participants:\n   - %^{Participants}\n** Agenda:\n   - %^{Agenda items}\n** Notes:\n   - \n** Action Items:\n   - [ ] \n** Next Steps:\n   - ")
     ("p" "Phone Call" entry (file "inbox.org")
      "* PHONE %? :PHONE:\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Who: %^{Who}\n** Notes:\n   - \n** Follow-up:\n   - [ ] ")
     ("i" "Idea" entry (file "inbox.org")
      "* %? :IDEA:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i")))
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-agenda-span 'day)
  (org-agenda-start-with-log-mode nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-custom-commands
   '(("d" "Daily Review"
      ((agenda "" ((org-agenda-span 'day)))
       (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
       (todo "TODO" ((org-agenda-overriding-header "Inbox")
                     (org-agenda-files '("~/org/inbox.org"))))))
     ("w" "Weekly Review"
      ((agenda "" ((org-agenda-span 'week)))
       (todo "TODO|NEXT|WAITING" ((org-agenda-overriding-header "All Open Items")))
       (todo "DONE" ((org-agenda-overriding-header "Completed This Week")
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "CLOSED: \\[.*\\]"))))))
     ("n" "Next Actions" todo "NEXT")
     ("i" "Inbox" todo "TODO" ((org-agenda-files '("~/org/inbox.org"))))))
  (org-export-with-broken-links t)
  (org-html-validation-link nil)
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"
     "pdflatex -interaction nonstopmode -output-directory %o %f"))
  :hook
  (org-mode . visual-line-mode)
  :config
  (defun my/org-daily-review ()
    "Open inbox and agenda for daily review."
    (interactive)
    (delete-other-windows)
    (find-file "~/org/inbox.org")
    (split-window-horizontally)
    (other-window 1)
    (org-agenda nil "d")))

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

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :bind
  ("C-c g" . magit-status)
  :custom
  (magit-diff-refine-hunk (not my/high-spec-system-p))
  (magit-refresh-status-buffer nil)
  (magit-tramp-pipe-stty-settings 'pty)
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(non-empty-second-line))
  (magit-repository-directories '(("~/Code" . 1)))
  (magit-diff-highlight-indentation (not my/high-spec-system-p))
  (magit-diff-highlight-trailing (not my/high-spec-system-p))
  (magit-diff-paint-whitespace (not my/high-spec-system-p))
  (magit-diff-highlight-hunk-body (not my/high-spec-system-p)))

(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-connector 'sqlite-builtin))

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

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-auto-cleanup nil
        recentf-save-file
        (expand-file-name "recentf/recentf" my/tmp-dir)
        recentf-max-saved-items 200
        recentf-max-menu-items 25)
  :config
  (dolist (timer timer-list)
    (when (and (timerp timer)
               (eq (timer--function timer) 'recentf-save-list))
      (cancel-timer timer)))
  (add-hook 'kill-emacs-hook 'recentf-save-list)
  :bind (("C-c r" . consult-recent-file)))

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

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  (treesit-auto-install-grammar t)
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (conf-toml-mode . toml-ts-mode)
          (css-mode . css-ts-mode)
          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (java-mode . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (lua-mode . lua-ts-mode)
          (markdown-mode . markdown-ts-mode)
          (python-mode . python-ts-mode)
          (rust-mode . rust-ts-mode)
          (sh-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))

  (defun my/install-missing-grammars ()
    "Install missing tree-sitter grammars interactively."
    (interactive)
    (dolist (lang-source treesit-language-source-alist)
      (let ((lang (car lang-source)))
        (unless (treesit-language-available-p lang)
          (when (y-or-n-p (format "Install tree-sitter grammar for %s? " lang))
            (treesit-install-language-grammar lang))))))

  (setq treesit-simple-indent-presets
        (append treesit-simple-indent-presets
                '((jsx-indent . (lambda (node parent bol)
                                  (if (string-match-p "jsx" (treesit-node-type node))
                                      js-indent-level
                                    0))))))

  (defun my/treesit-setup ()
    "Setup tree-sitter for current buffer."
    (when (and (treesit-available-p)
               (treesit-language-at-point-function))
      (treesit-font-lock-recompute-features)))

  (add-hook 'prog-mode-hook #'my/treesit-setup)

  (setq treesit-primary-parser 'auto)

  (setq treesit-aggregated-simple-imenu-settings
        '((nil "Function" "\\`function_definition\\'" nil nil)
          (nil "Class" "\\`class_definition\\'" nil nil)
          (nil "Method" "\\`method_definition\\'" nil nil)))

  (setq treesit-aggregated-outline-predicate
        (lambda (node)
          (string-match-p
           "\\`\\(function\\|class\\|method\\|module\\)_definition\\'"
           (treesit-node-type node))))

  (defun my/treesit-configure-outline ()
    "Configure outline-minor-mode for tree-sitter."
    (when (and (treesit-available-p)
               (treesit-parser-list))
      (setq-local outline-regexp "")
      (setq-local outline-level
                  (lambda ()
                    (let ((node (treesit-node-at (point))))
                      (if node
                          (length (treesit-node-parent-chain node))
                        1))))))

  (add-hook 'tree-sitter-after-first-parse-hook #'my/treesit-configure-outline)

  (with-eval-after-load 'which-function
    (setq which-func-functions
          (cons (lambda ()
                  (when (and (treesit-available-p)
                             (treesit-parser-list))
                    (let ((node (treesit-node-at (point))))
                      (when node
                        (treesit-node-text
                         (or (treesit-parent-until
                              node
                              (lambda (n)
                                (string-match-p
                                 "\\`\\(function\\|method\\)_definition\\'"
                                 (treesit-node-type n))))
                             node)
                         t)))))
                which-func-functions))))

(use-package c-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.[ch]\\'" . c-ts-mode)
  :config
  (setq c-ts-mode-indent-style 'gnu)
  (setq c-ts-mode-indent-offset 2))

(use-package c++-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.\\(cpp\\|cxx\\|cc\\|C\\|hpp\\|hxx\\|hh\\|H\\)\\'" . c++-ts-mode))

(use-package python-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.py[iw]?\\'" . python-ts-mode)
  :config
  (setq python-ts-mode-indent-offset 4))

(use-package js-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.m?js\\'" . js-ts-mode)
  :config
  (setq js-ts-mode-indent-offset 2))

(use-package typescript-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :config
  (setq typescript-ts-mode-indent-offset 2))

(use-package tsx-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.tsx\\'" . tsx-ts-mode))

(use-package json-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.json\\'" . json-ts-mode))

(use-package yaml-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

(use-package toml-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.toml\\'" . toml-ts-mode))

(use-package markdown-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.md\\'" . markdown-ts-mode))

(use-package rust-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.rs\\'" . rust-ts-mode)
  :config
  (setq rust-ts-mode-indent-offset 4))

(use-package go-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.go\\'" . go-ts-mode))

(use-package bash-ts-mode
  :ensure nil
  :after treesit
  :mode ("\\.\\(sh\\|bash\\|zsh\\)\\'" . bash-ts-mode))

(use-package simple
  :ensure nil
  :config
  (kill-ring-deindent-mode 1)
  (setq next-error-message-highlight t)
  (setq read-minibuffer-restore-windows t)
  (setq kill-do-not-save-duplicates t))

(use-package so-long :ensure nil :config (global-so-long-mode 1))

(use-package flymake
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-start-on-flymake-mode t)
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-show-diagnostics-at-end-of-line nil)
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
               (string-match-p "\\(?:packages\\|lisp\\)/"
                               (buffer-file-name))
               (not
                (string-match-p "init\\.el\\|config\\.el"
                                (buffer-file-name))))
      (package-lint-flymake-setup)))
  (remove-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
  (add-hook 'emacs-lisp-mode-hook #'my/maybe-enable-package-lint))

(use-package checkdoc
  :ensure nil
  :custom
  (checkdoc-force-docstrings-flag nil)
  (checkdoc-arguments-in-order-flag nil)
  (checkdoc-verb-check-experimental-flag nil)
  (checkdoc-permit-comma-termination-flag t)
  (checkdoc-spellcheck-documentation-flag nil))

(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . checkdoc-minor-mode)
         (emacs-lisp-mode . (lambda ()
                              (setq-local flymake-diagnostic-functions
                                          (append
                                           flymake-diagnostic-functions
                                           '(elisp-flymake-checkdoc))))))
  :bind (:map emacs-lisp-mode-map
              ("C-c C-d" . checkdoc)
              ("C-c C-b" . checkdoc-current-buffer)
              ("C-c C-c" . checkdoc-comments))
  :config
  (defun my/checkdoc-maybe-suppress-in-init ()
    "Suppress some checkdoc warnings in init files."
    (when (and (buffer-file-name)
               (string-match-p "init\\.el\\|early-init\\.el"
                               (buffer-file-name)))
      (setq-local checkdoc-force-docstrings-flag nil)
      (setq-local checkdoc-symbol-words '("init.el" "early-init.el"))))
  (add-hook 'emacs-lisp-mode-hook #'my/checkdoc-maybe-suppress-in-init))

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
      (expand-file-name "snippets"
                        (file-name-directory
                         (locate-library "yasnippet-snippets"))))))
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
                        (expand-file-name "snippets"
                                          (file-name-directory
                                           (locate-library
                                            "yasnippet-snippets"))))))
    (when snippets-dir
      (unless (file-directory-p snippets-dir)
        (warn
         "yasnippet-snippets directory %s not found; consider reinstalling the package"
         snippets-dir)))))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-key] . helpful-key))

(use-package elisp-demos
  :ensure t
  :config
  (advice-add
   'helpful-update
   :after #'elisp-demos-advice-helpful-update))

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

(use-package eshell
  :ensure nil
  :defer t
  :custom
  (eshell-directory-name (expand-file-name "eshell" my/tmp-dir))
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-prompt-regexp "^[^#$\n]*[#$] ")
  (eshell-buffer-maximum-lines 50000)
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-visual-commands '("vi" "vim" "nvim" "screen" "tmux" "top"
                            "htop" "less" "more"
                            "lynx" "links" "ncftp" "mutt" "pine" "tin"
                            "trn" "elm"
                            "irssi" "mc" "nano" "emacs" "ssh" "telnet"
                            "ftp" "su"
                            "watch" "tail" "head" "pager" "most"
                            "alsamixer" "nethack"
                            "cmus" "mocp" "tig" "lftp" "aptitude"
                            "dpkg-reconfigure"
                            "nmtui" "raspi-config" "bluetoothctl"
                            "virsh" "docker"
                            "podman" "k9s" "lazygit" "lazydocker"
                            "gdu" "ncdu" "btop"
                            "gotop" "ytop" "bpytop" "glances" "nmon"
                            "iotop" "iftop"
                            "nethogs" "bmon" "newsboat" "newsbeuter"
                            "rtorrent"
                            "transmission-cli" "weechat" "finch"
                            "centerim" "epic"
                            "epic5" "scrollz" "sic" "ii" "ratpoison"
                            "dvtm" "byobu"
                            "abduco" "dtach"))
  (eshell-visual-subcommands
   '(("git" "log" "diff" "show" "rebase" "add" "commit")
     ("systemctl" "status" "edit")
     ("journalctl" "")
     ("cargo" "install" "build" "test" "run")
     ("npm" "install" "update")
     ("yarn" "install" "upgrade")
     ("apt" "install" "update" "upgrade"
      "search")
     ("apt-get" "install" "update" "upgrade")
     ("brew" "install" "update" "upgrade")))
  (eshell-visual-options '(("git" "--help" "--paginate")
                           ("man" "")
                           ("sudo" "-e")))
  (eshell-modules-list '(eshell-alias
                         eshell-banner
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
                         eshell-unix
                         eshell-tramp
                         eshell-xtra
                         eshell-elecslash
                         eshell-rebind))
  :config
  (setq eshell-prompt-function
        (lambda ()
          (concat (propertize (abbreviate-file-name default-directory)
                              'face 'font-lock-comment-face)
                  (if (= (user-uid) 0) " # " " $ "))))
  (defalias 'eshell/ll 'eshell/ls)
  (defalias 'eshell/la '(lambda () (eshell/ls "-a")))
  (defalias 'eshell/clear 'eshell/clear-scrollback)
  (with-eval-after-load 'pcomplete
    (setq pcomplete-termination-string ""
          pcomplete-ignore-case t
          pcomplete-autolist nil
          pcomplete-cycle-completions nil
          pcomplete-cycle-cutoff-length 0))
  :hook
  (eshell-mode . (lambda ()
                   (display-line-numbers-mode -1)
                   (setq-local pcomplete-cycle-completions nil)
                   (setq-local completion-at-point-functions
                               '(pcomplete-completions-at-point))
))
  :bind
  ("C-c e" . eshell))

(use-package eshell-syntax-highlighting
  :ensure t
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode 1))

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
        (expand-file-name "gnus-drafts" my/tmp-dir)))

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

(use-package hl-line
  :ensure nil
  :commands (hl-line-mode)
  :config
  (setq hl-line-sticky-flag nil)
  (setq hl-line-overlay-priority -50)
  :hook
  (prog-mode . hl-line-mode)
  (occur-mode . hl-line-mode))

(use-package sly
  :ensure t
  :defer t
  :diminish
  :mode ("\\.lisp\\'" . lisp-mode)
  :custom
  (inferior-lisp-program "sbcl")
  (sly-lisp-implementations
   '((sbcl ("sbcl" "--dynamic-space-size" "2048"))))
  (sly-net-coding-system 'utf-8-unix)
  (sly-complete-symbol-function 'sly-simple-completions)
  :config
  (setq sly-symbol-completion-mode nil)
  (setq sly-autodoc-use-multiline-p nil)
  (setq sly-kill-without-query-p t)
  (setq sly-description-autofocus t)
  (setq sly-inhibit-pipelining nil)
  (setq sly-load-failed-fasl 'never)
  :bind (:map sly-mode-map
              ("C-c C-d C-d" . sly-describe-symbol)
              ("C-c C-d C-f" . sly-describe-function)
              ("M-." . sly-edit-definition)
              ("M-," . sly-pop-find-definition-stack)
              ("C-c C-c" . sly-compile-defun)
              ("C-c C-k" . sly-compile-file)
              ("C-c C-z" . sly-mrepl)))

(use-package auth-source-xoauth2-plugin
  :ensure t
  :defer t
  :custom (auth-source-xoauth2-plugin-mode t))

(require 'mailcap)

(mailcap-parse-mailcaps)

(use-package eat
  :ensure t
  :defer t
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-query-before-killing-running-terminal nil)
  (eat-enable-blinking-text t)
  (eat-term-scrollback-size 50000)
  (eat-shell-command (list (getenv "SHELL") "-l"))
  (eat-default-terminfo-name "xterm-256color")
  (eat-eshell-visual-command-mode-map
   '(("git" . ("log" "diff" "show" "rebase" "add" "commit"))
     ("systemctl" . ("status" "edit"))
     ("journalctl" . (""))
     ("cargo" . ("install" "build" "test" "run"))
     ("npm" . ("install" "update"))
     ("yarn" . ("install" "upgrade"))
     ("apt" . ("install" "update" "upgrade" "search"))
     ("apt-get" . ("install" "update" "upgrade"))
     ("brew" . ("install" "update" "upgrade"))))
  (eat-enable-directory-tracking t)
  (eat-enable-shell-command-history t)
  (eat-shell-integration-directory-tracking t)
  :config
  (setenv "TERM" "xterm-256color")
  (defun my/eat-eshell-force-kill-on-exit ()
    "Force eat-kill-buffer-on-exit for eshell visual commands."
    (when (and (derived-mode-p 'eat-mode)
               (or (local-variable-p 'eshell-parent-buffer)
                   (and (boundp 'eat--eshell-process)
                        eat--eshell-process)))
      (setq-local eat-kill-buffer-on-exit t)
      (add-hook 'eat-exit-hook #'eat--kill-buffer 90 t)))
  (add-hook 'eat-mode-hook #'my/eat-eshell-force-kill-on-exit)
  (defun my/eat-visual-buffer-name-advice (orig-func &rest args)
    "Customize buffer names for eshell visual commands."
    (if (and args (stringp (car args)))
        (let* ((program (car args))
               (cmd-name (file-name-nondirectory program))
               (eat-buffer-name (format "*%s*" cmd-name)))
          (apply orig-func args))
      (apply orig-func args)))
  (advice-add 'eat--eshell-exec-visual :around
              #'my/eat-visual-buffer-name-advice)
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c v" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat
        claude-code-ide-use-side-window nil
        claude-code-ide-buffer-name-function
      (lambda (directory)
        (if directory
            (format "*Claude:%s*" (file-name-nondirectory (directory-file-name directory)))
          "*Claude:Global*"))))

(use-package electric
  :ensure nil
  :defer t
  :hook ((prog-mode . electric-indent-mode)
         (org-mode . electric-indent-mode))
  :config
  (setq electric-indent-inhibit nil))

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

(use-package man
  :ensure nil
  :defer t
  :commands (man)
  :config
  (setq Man-notify-method 'pushy))

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

(use-package server
  :ensure nil
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

(use-package password-store
  :ensure t
  )

(use-package password-store-otp
  :ensure t
  )

(use-package pass
  :ensure t
  :after (password-store password-store-otp)
  :bind (("C-c P" . pass))
  :commands (pass))

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

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

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

(use-package dictionary
  :ensure nil
  :defer t
  :custom
  (dictionary-server "dict.org")
  :bind
  ("<f6>" . dictionary-lookup-definition))

(use-package logview
  :ensure t
  :mode (("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode)
         ("\\<\\(syslog\\|messages\\|error\\|debug\\|server\\|access\\|log\\)\\'"
          . logview-mode))
  :config
  (add-to-list 'magic-mode-alist
               '("^\\(?:[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-+\\|[A-Za-z]\\{3\\}\\s-+[0-9]\\{1,2\\}\\s-+[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"
                 . logview-mode)))

(use-package journalctl-mode
  :ensure t
  :defer t
  :bind (("C-c j" . journalctl)
         :map journalctl-mode-map
         ("f" . journalctl-mode-toggle-follow)
         ("n" . journalctl-next-chunk)
         ("p" . journalctl-previous-chunk)
         ("g" . journalctl-refresh)
         ("q" . kill-current-buffer))
  :config
  (setq journalctl-chunk-size 500)
  (setq journalctl-follow-freq 2)
  (setq journalctl-units nil)
  (defun my/journalctl-boot ()
    "Show logs from current boot."
    (interactive)
    (journalctl "-b"))
  (defun my/journalctl-errors ()
    "Show only error-level logs."
    (interactive)
    (journalctl "-p err"))
  (defun my/journalctl-today ()
    "Show logs from today."
    (interactive)
    (journalctl "-S today")))

(use-package pulse
  :ensure nil
  :config
  (setq pulse-iterations 10)
  (setq pulse-delay 0.055)
  (set-face-attribute 'pulse-highlight-start-face nil
                      :background "#51afef")
  (set-face-attribute 'pulse-highlight-face nil
                      :background "#51afef")

  (defun my/pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun my/pulse-region (start end &rest _)
    "Pulse region between START and END."
    (pulse-momentary-highlight-region start end))

  (dolist (cmd '(recenter-top-bottom
                 reposition-window
                 other-window
                 delete-window
                 delete-other-windows
                 forward-page
                 backward-page
                 scroll-up-command
                 scroll-down-command
                 windmove-left
                 windmove-right
                 windmove-up
                 windmove-down
                 winner-undo
                 winner-redo))
    (advice-add cmd :after #'my/pulse-line))

  (with-eval-after-load 'bookmark
    (advice-add 'bookmark-jump :after #'my/pulse-line))

  (with-eval-after-load 'consult
    (add-hook 'consult-after-jump-hook #'my/pulse-line))

  (with-eval-after-load 'imenu
    (advice-add 'imenu :after #'my/pulse-line))

  (add-hook 'minibuffer-setup-hook #'my/pulse-line))

(use-package volatile-highlights
  :ensure t
  :defer t
  :init (volatile-highlights-mode 1))

(use-package smtpmail
  :ensure nil
  :config
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.mailbox.org"
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587
        smtpmail-debug-info t
        smtpmail-debug-verb t))

(use-package emoji
  :ensure nil
  :bind ("C-c E" . emoji-search))

(use-package simple
  :ensure nil
  :bind
  (("C-x k" . kill-current-buffer)
   ("C-x K" . kill-buffer)
   ([remap keyboard-quit] . my/keyboard-quit-dwim)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "Emacs started in %.2f seconds with %d garbage collections"
             (float-time
              (time-subtract after-init-time before-init-time))
             gcs-done)))

(use-package erc
  :ensure nil
  :defer t
  :config
  (setq
   erc-server-coding-system '(utf-8 . utf-8)
   erc-kill-buffer-on-part t
   erc-kill-queries-on-quit t
   erc-kill-server-buffer-on-quit t
   erc-prompt
   (lambda () (concat "[" (or (erc-default-target) "ERC") "] "))
   erc-timestamp-format "[%H:%M] "
   erc-fill-function 'erc-fill-static
   erc-fill-static-center 20
   erc-fill-prefix "      "
   erc-log-channels-directory (expand-file-name "irc-logs" my/tmp-dir)
   erc-save-buffer-on-part t
   erc-save-queries-on-quit t
   erc-log-write-after-send t
   erc-log-write-after-insert t)
  (setq erc-modules '(autojoin button completion fill irccontrols
                               list log match menu move-to-prompt
                               netsplit
                               networks noncommands readonly ring
                               stamp track
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
                                          :port
                                          (number-to-string port)
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
    "Enable completion for ERC."
    (setq-local pcomplete-cycle-completions nil))
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
   (erc-mode
    . (lambda () (display-line-numbers-mode -1) (hl-line-mode 1)))
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

(provide 'init)
;;; init.el ends here
