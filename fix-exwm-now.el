;;; fix-exwm-now.el --- Emergency fix for EXWM interruptions -*- lexical-binding: t; -*-

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

;; Nuclear option to stop EXWM interruptions.

;;; Code:

(defun fix-exwm-interruptions-now ()
  "Stop ALL potential sources of EXWM interruptions."
  (interactive)
  
  ;; 1. Cancel ALL frequent timers
  (let ((cancelled 0))
    (dolist (timer (append timer-list timer-idle-list))
      (when (and (timerp timer)
                 (timer--repeat-delay timer)
                 (< (timer--repeat-delay timer) 30))
        (cancel-timer timer)
        (message "Cancelled: %S (every %s sec)" 
                 (timer--function timer) (timer--repeat-delay timer))
        (setq cancelled (1+ cancelled))))
    
    ;; 2. Disable all auto-save features
    (setq auto-save-default nil
          auto-save-interval 0
          auto-save-timeout 0)
    
    ;; 3. Disable savehist autosave
    (setq savehist-autosave-interval nil)
    (cancel-function-timers 'savehist-autosave)
    
    ;; 4. Disable recentf autosave
    (cancel-function-timers 'recentf-save-list)
    
    ;; 5. Disable eldoc if running
    (global-eldoc-mode -1)
    
    ;; 6. Check for display-time-mode
    (when (bound-and-true-p display-time-mode)
      (display-time-mode -1)
      (message "Disabled display-time-mode"))
    
    ;; 7. Disable which-key idle timer if present
    (when (bound-and-true-p which-key-mode)
      (which-key-mode -1)
      (message "Disabled which-key-mode"))
    
    ;; 8. Look for package-specific timers
    (dolist (timer timer-list)
      (when (timerp timer)
        (let ((func-name (symbol-name (timer--function timer))))
          (when (string-match-p "lsp\\|eglot\\|flycheck\\|flymake\\|company\\|corfu" func-name)
            (cancel-timer timer)
            (message "Cancelled package timer: %s" func-name)))))
    
    (message "Fixed! Cancelled %d timers. Try typing in EXWM now." cancelled)))

(defun show-all-active-timers ()
  "Show all active timers in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*All Active Timers*")
    (erase-buffer)
    (insert "ALL ACTIVE TIMERS:\n\n")
    (let ((all-timers '()))
      ;; Collect all timers with details
      (dolist (timer timer-list)
        (when (timerp timer)
          (push (format "Regular: %S | Repeat: %S sec | Next: %s"
                        (timer--function timer)
                        (timer--repeat-delay timer)
                        (format-time-string "%H:%M:%S" (timer--time timer)))
                all-timers)))
      (dolist (timer timer-idle-list)
        (when (timerp timer)
          (push (format "Idle: %S | Repeat: %S sec"
                        (timer--function timer)
                        (timer--repeat-delay timer))
                all-timers)))
      ;; Display sorted by type
      (dolist (timer-str (sort all-timers 'string<))
        (insert timer-str "\n")))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; Run it immediately
(fix-exwm-interruptions-now)

(provide 'fix-exwm-now)
;;; fix-exwm-now.el ends here