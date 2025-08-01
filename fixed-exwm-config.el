;;; fixed-exwm-config.el --- Fixed EXWM monitor configuration -*- lexical-binding: t; -*-

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

;; Fixed EXWM monitor configuration with proper error handling

;;; Code:

(defun my/exwm-get-connected-monitors ()
  "Get list of connected monitors with their properties."
  (let* ((xrandr-output (shell-command-to-string "xrandr"))
         (lines (split-string xrandr-output "\n"))
         (monitors '()))
    (dolist (line lines)
      (when (string-match "\\([^ ]+\\) connected\\(?: primary\\)? \\([0-9]+\\)x\\([0-9]+\\)" line)
        (push (list (match-string 1 line)
                    (string-to-number (match-string 2 line))
                    (string-to-number (match-string 3 line)))
              monitors)))
    (nreverse monitors)))

(defun my/exwm-randr-workspace-assignment (monitors)
  "Create workspace assignment plist based on MONITORS."
  (let* ((has-laptop (seq-find (lambda (m) (string= (car m) "eDP-1")) monitors))
         (external-monitors (seq-filter (lambda (m) (not (string= (car m) "eDP-1"))) monitors))
         (num-external (length external-monitors))
         (plist '())
         (workspace 0))
    (cond
     ;; Three monitors: distribute workspaces across all
     ((and has-laptop (>= num-external 2))
      ;; Workspaces 0-2 on eDP-1
      (dotimes (i 3)
        (setq plist (append plist (list workspace "eDP-1")))
        (setq workspace (1+ workspace)))
      ;; Workspaces 3-6 on first external
      (when (> num-external 0)
        (dotimes (i 4)
          (setq plist (append plist (list workspace (car (car external-monitors)))))
          (setq workspace (1+ workspace))))
      ;; Workspaces 7-9 on second external
      (when (> num-external 1)
        (dotimes (i 3)
          (setq plist (append plist (list workspace (car (cadr external-monitors)))))
          (setq workspace (1+ workspace)))))
     
     ;; Two monitors: split workspaces
     ((and has-laptop (= num-external 1))
      ;; Workspaces 0-4 on eDP-1
      (dotimes (i 5)
        (setq plist (append plist (list workspace "eDP-1")))
        (setq workspace (1+ workspace)))
      ;; Workspaces 5-9 on external
      (dotimes (i 5)
        (setq plist (append plist (list workspace (car (car external-monitors)))))
        (setq workspace (1+ workspace))))
     
     ;; Laptop only: all workspaces on eDP-1
     (has-laptop
      (dotimes (i 10)
        (setq plist (append plist (list i "eDP-1")))))
     
     ;; External monitors only: distribute evenly
     ((> num-external 0)
      (let ((per-monitor (/ 10 num-external))
            (extra (% 10 num-external)))
        (dolist (monitor external-monitors)
          (let ((count (if (> extra 0)
                           (prog1 (1+ per-monitor)
                             (setq extra (1- extra)))
                         per-monitor)))
            (dotimes (i count)
              (setq plist (append plist (list workspace (car monitor))))
              (setq workspace (1+ workspace))))))))
    plist))

(defun my/exwm-configure-monitors-safe ()
  "Configure monitors with proper error handling."
  (condition-case err
      (let* ((monitors (my/exwm-get-connected-monitors))
             (has-laptop (seq-find (lambda (m) (string= (car m) "eDP-1")) monitors))
             (external-monitors (seq-filter (lambda (m) (not (string= (car m) "eDP-1"))) monitors)))
        
        (message "EXWM: Detected monitors: %s" 
                 (mapconcat (lambda (m) (format "%s (%dx%d)" (car m) (cadr m) (caddr m))) 
                            monitors ", "))
        
        ;; Build xrandr command
        (let ((xrandr-cmd "xrandr")
              (x-pos 0))
          (cond
           ;; Three monitors
           ((and has-laptop (>= (length external-monitors) 2))
            (let ((left (car external-monitors))
                  (center (cadr external-monitors)))
              (setq xrandr-cmd (format "%s --output %s --auto --pos 0x0 --primary" 
                                       xrandr-cmd (car left)))
              (setq x-pos (cadr left))
              (setq xrandr-cmd (format "%s --output %s --auto --pos %dx0" 
                                       xrandr-cmd (car center) x-pos))
              (setq x-pos (+ x-pos (cadr center)))
              (setq xrandr-cmd (format "%s --output eDP-1 --auto --pos %dx0" 
                                       xrandr-cmd x-pos))))
           
           ;; Two monitors
           ((and has-laptop (= (length external-monitors) 1))
            (let ((external (car external-monitors)))
              (setq xrandr-cmd (format "%s --output %s --auto --pos 0x0 --primary" 
                                       xrandr-cmd (car external)))
              (setq xrandr-cmd (format "%s --output eDP-1 --auto --pos %dx0" 
                                       xrandr-cmd (cadr external)))))
           
           ;; Laptop only
           (has-laptop
            (setq xrandr-cmd "xrandr --output eDP-1 --auto --pos 0x0 --primary"))
           
           ;; External only
           (t
            (dolist (monitor external-monitors)
              (setq xrandr-cmd (format "%s --output %s --auto --pos %dx0" 
                                       xrandr-cmd (car monitor) x-pos))
              (when (= x-pos 0)
                (setq xrandr-cmd (concat xrandr-cmd " --primary")))
              (setq x-pos (+ x-pos (cadr monitor))))))
          
          ;; Execute xrandr command
          (message "EXWM: Executing: %s" xrandr-cmd)
          (shell-command xrandr-cmd)
          
          ;; Set workspace assignment
          (let ((plist (my/exwm-randr-workspace-assignment monitors)))
            (setq exwm-randr-workspace-monitor-plist plist)
            (message "EXWM: Workspace assignment: %S" plist))
          
          ;; Refresh EXWM
          (when (fboundp 'exwm-randr-refresh)
            (exwm-randr-refresh))
          
          (message "EXWM: Monitor configuration complete")))
    (error 
     (message "EXWM: Error configuring monitors: %s" (error-message-string err)))))

;; Use this configuration in your init.el:
;; Replace the my/exwm-randr-setup and my/exwm-configure-monitors functions
;; with my/exwm-configure-monitors-safe

;; In the EXWM use-package block:
;; (my/exwm-configure-monitors-safe)
;; (setq exwm-randr-screen-change-hook
;;       (lambda ()
;;         (my/exwm-configure-monitors-safe)))

(provide 'fixed-exwm-config)
;;; fixed-exwm-config.el ends here