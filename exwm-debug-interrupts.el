;;; exwm-debug-interrupts.el --- Find what's interrupting EXWM every 5 seconds -*- lexical-binding: t; -*-

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

;; Emergency debug tool for EXWM interruptions.

;;; Code:

(defun exwm-list-all-timers ()
  "List ALL timers sorted by frequency."
  (interactive)
  (let ((all-timers '()))
    ;; Collect all timers
    (dolist (timer timer-list)
      (when (timerp timer)
        (push (list 'regular timer (timer--function timer) (timer--repeat-delay timer)) all-timers)))
    (dolist (timer timer-idle-list)
      (when (timerp timer)
        (push (list 'idle timer (timer--function timer) (timer--repeat-delay timer)) all-timers)))
    
    ;; Sort by repeat delay (smallest first)
    (setq all-timers (sort all-timers (lambda (a b)
                                        (let ((a-delay (nth 3 a))
                                              (b-delay (nth 3 b)))
                                          (if (and a-delay b-delay)
                                              (< a-delay b-delay)
                                            (and a-delay (not b-delay)))))))
    
    ;; Display results
    (with-current-buffer (get-buffer-create "*EXWM Timer Analysis*")
      (erase-buffer)
      (insert "=== TIMERS THAT COULD INTERRUPT EXWM (sorted by frequency) ===\n\n")
      (dolist (timer-info all-timers)
        (let ((type (nth 0 timer-info))
              (func (nth 2 timer-info))
              (repeat (nth 3 timer-info)))
          (when (or (not repeat) (< repeat 30))  ; Show timers that repeat < 30 seconds
            (insert (format "Type: %s | Function: %S | Repeat: %s seconds\n"
                           type func (or repeat "one-shot")))
            (when (and repeat (< repeat 10))
              (insert "  ^^^ THIS COULD BE YOUR PROBLEM! ^^^\n"))
            (insert "\n"))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun exwm-cancel-frequent-timers ()
  "Cancel all timers that run more frequently than every 10 seconds."
  (interactive)
  (let ((cancelled 0))
    (dolist (timer (append timer-list timer-idle-list))
      (when (and (timerp timer)
                 (timer--repeat-delay timer)
                 (< (timer--repeat-delay timer) 10))
        (cancel-timer timer)
        (message "Cancelled timer: %S (was running every %s seconds)"
                 (timer--function timer) (timer--repeat-delay timer))
        (setq cancelled (1+ cancelled))))
    (message "Cancelled %d frequent timers" cancelled)))

(defun exwm-trace-commands ()
  "Trace all commands executed, especially during EXWM interruptions."
  (interactive)
  (add-hook 'pre-command-hook
            (lambda ()
              (when (derived-mode-p 'exwm-mode)
                (message "[%s] Command: %S | Read-only: %s"
                         (format-time-string "%H:%M:%S.%3N")
                         this-command
                         buffer-read-only)))))

(defun exwm-monitor-buffer-changes ()
  "Monitor when EXWM buffers change read-only status."
  (interactive)
  (add-variable-watcher
   'buffer-read-only
   (lambda (symbol newval operation where)
     (when (and (eq operation 'set)
                (derived-mode-p 'exwm-mode))
       (message "[%s] EXWM buffer-read-only changed to: %s in %s"
                (format-time-string "%H:%M:%S.%3N")
                newval
                (buffer-name))))))

(defun exwm-stop-all-monitoring ()
  "Stop all EXWM monitoring."
  (interactive)
  (remove-hook 'pre-command-hook 
               (lambda ()
                 (when (derived-mode-p 'exwm-mode)
                   (message "[%s] Command: %S | Read-only: %s"
                            (format-time-string "%H:%M:%S.%3N")
                            this-command
                            buffer-read-only))))
  (remove-variable-watcher 'buffer-read-only)
  (message "Stopped EXWM monitoring"))

(provide 'exwm-debug-interrupts)
;;; exwm-debug-interrupts.el ends here