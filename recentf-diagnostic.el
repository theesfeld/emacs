;;; recentf-diagnostic.el --- Diagnose recentf timer issues  -*- lexical-binding: t; -*-

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

;; Diagnostic tool for recentf timer issues with EXWM

;;; Code:

(defun diagnose-recentf-timers ()
  "Check for active recentf timers."
  (interactive)
  (let ((recentf-timers
         (cl-remove-if-not
          (lambda (timer)
            (and (timerp timer)
                 (or (eq (timer--function timer) 'recentf-save-list)
                     (and (symbolp (timer--function timer))
                          (string-match-p "recentf" (symbol-name (timer--function timer)))))))
          timer-list)))
    (if recentf-timers
        (progn
          (message "Found %d recentf-related timer(s):" (length recentf-timers))
          (dolist (timer recentf-timers)
            (message "  Timer: %s, Repeat: %s, Next time: %s"
                     (timer--function timer)
                     (timer--repeat-delay timer)
                     (format-time-string "%Y-%m-%d %H:%M:%S" (timer--time timer)))))
      (message "No recentf timers found."))))

(defun cancel-all-recentf-timers ()
  "Cancel all recentf-related timers."
  (interactive)
  (let ((count 0))
    (dolist (timer timer-list)
      (when (and (timerp timer)
                 (or (eq (timer--function timer) 'recentf-save-list)
                     (and (symbolp (timer--function timer))
                          (string-match-p "recentf" (symbol-name (timer--function timer))))))
        (cancel-timer timer)
        (setq count (1+ count))))
    (message "Cancelled %d recentf timer(s)." count)))

(defun check-recentf-hooks ()
  "Check which hooks might trigger recentf saves."
  (interactive)
  (let ((hooks-found '()))
    (dolist (hook '(after-save-hook
                    find-file-hook
                    kill-buffer-hook
                    kill-emacs-hook
                    auto-save-hook
                    before-save-hook))
      (when (and (boundp hook)
                 (cl-some (lambda (fn)
                            (and (symbolp fn)
                                 (string-match-p "recentf" (symbol-name fn))))
                          (symbol-value hook)))
        (push (cons hook (symbol-value hook)) hooks-found)))
    (if hooks-found
        (progn
          (message "Found recentf in these hooks:")
          (dolist (hook-info hooks-found)
            (message "  %s: %s" (car hook-info) (cdr hook-info))))
      (message "No recentf functions found in common hooks."))))

(provide 'recentf-diagnostic)
;;; recentf-diagnostic.el ends here