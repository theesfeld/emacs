;;; debug-exwm-timers.el --- Debug EXWM interruptions -*- lexical-binding: t; -*-

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

;; Debug tool to find what's interrupting EXWM input.

;;; Code:

(defun debug-list-all-timers ()
  "List all active timers with their functions."
  (interactive)
  (with-current-buffer (get-buffer-create "*Timer Debug*")
    (erase-buffer)
    (insert "=== ACTIVE TIMERS ===\n\n")
    (dolist (timer timer-list)
      (when (timerp timer)
        (insert (format "Function: %S\n" (timer--function timer)))
        (insert (format "Repeat: %S\n" (timer--repeat-delay timer)))
        (insert (format "Next time: %S\n" (timer--time timer)))
        (insert "---\n")))
    (insert "\n=== IDLE TIMERS ===\n\n")
    (dolist (timer timer-idle-list)
      (when (timerp timer)
        (insert (format "Function: %S\n" (timer--function timer)))
        (insert (format "Repeat: %S\n" (timer--repeat-delay timer)))
        (insert (format "Idle time: %S\n" (timer--time timer)))
        (insert "---\n")))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun debug-trace-buffer-changes ()
  "Trace when EXWM buffers become read-only."
  (interactive)
  (add-hook 'after-change-functions
            (lambda (beg end len)
              (when (and (derived-mode-p 'exwm-mode)
                         buffer-read-only)
                (message "EXWM buffer became read-only! Function: %S" this-command)))
            nil t))

(defun debug-monitor-exwm-hooks ()
  "Monitor hooks that might affect EXWM."
  (interactive)
  (with-current-buffer (get-buffer-create "*EXWM Hook Debug*")
    (erase-buffer)
    (insert "=== POTENTIALLY PROBLEMATIC HOOKS ===\n\n")
    (dolist (hook '(post-command-hook
                    pre-command-hook
                    window-configuration-change-hook
                    buffer-list-update-hook
                    focus-in-hook
                    focus-out-hook))
      (insert (format "%s:\n" hook))
      (dolist (func (symbol-value hook))
        (insert (format "  - %S\n" func)))
      (insert "\n"))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun debug-find-suspicious-timers ()
  "Find timers that might interfere with EXWM."
  (interactive)
  (let ((suspicious '()))
    (dolist (timer (append timer-list timer-idle-list))
      (when (timerp timer)
        (let ((func (timer--function timer)))
          (when (or (string-match-p "save\\|write\\|auto" (format "%S" func))
                    (< (or (timer--repeat-delay timer) 60) 30))
            (push (list func (timer--repeat-delay timer)) suspicious)))))
    (if suspicious
        (message "Suspicious timers: %S" suspicious)
      (message "No obviously suspicious timers found"))))

(provide 'debug-exwm-timers)
;;; debug-exwm-timers.el ends here