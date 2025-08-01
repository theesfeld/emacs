;;; minimal-exwm.el --- Minimal EXWM configuration for testing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 William Theesfeld <william@theesfeld.net>

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

;; Minimal EXWM configuration to test basic functionality

;;; Code:

;; Bootstrap straight.el package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package
(straight-use-package 'use-package)
(require 'use-package)

;; Basic EXWM configuration
(when (eq window-system 'x)
  (use-package exwm
    :straight t
    :config
    ;; Set the initial number of workspaces
    (setq exwm-workspace-number 4)
    
    ;; Basic global key bindings
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset)
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))
    
    ;; Basic randr configuration
    (require 'exwm-randr)
    
    ;; Simple monitor detection
    (defun my/simple-exwm-randr-setup ()
      "Simple EXWM randr setup."
      (message "EXWM: Setting up monitors...")
      ;; Get monitor names
      (let* ((xrandr-output (shell-command-to-string "xrandr --listmonitors"))
             (monitor-lines (split-string xrandr-output "\n"))
             (monitors (seq-filter 
                        (lambda (line) 
                          (string-match-p "^ *[0-9]+:" line))
                        monitor-lines)))
        (message "EXWM: Found %d monitors" (length monitors))
        ;; Simple workspace assignment
        (when (> (length monitors) 1)
          (setq exwm-randr-workspace-monitor-plist
                '(0 "eDP-1" 1 "eDP-1" 2 "eDP-1" 3 "eDP-1")))
        (message "EXWM: Monitor setup complete")))
    
    ;; Set up randr
    (my/simple-exwm-randr-setup)
    (exwm-randr-mode 1)
    
    ;; Enable EXWM
    (exwm-enable)
    (message "EXWM: Enabled successfully")))

(provide 'minimal-exwm)
;;; minimal-exwm.el ends here