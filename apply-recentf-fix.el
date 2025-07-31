;;; apply-recentf-fix.el --- Immediate fix for recentf/EXWM issue -*- lexical-binding: t; -*-

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

;; Immediate fix for recentf interfering with EXWM input.
;; Load this file to apply the fix without restarting Emacs.

;;; Code:

(defun cancel-all-recentf-timers ()
  "Cancel all active recentf timers."
  (let ((count 0))
    (dolist (timer timer-list)
      (when (and (timerp timer)
                 (eq (timer--function timer) 'recentf-save-list))
        (cancel-timer timer)
        (setq count (1+ count))))
    count))

(let ((cancelled (cancel-all-recentf-timers)))
  (setq recentf-auto-cleanup nil)
  (message "Recentf fix applied: cancelled %d timer(s). Auto-cleanup disabled." cancelled))

(provide 'apply-recentf-fix)
;;; apply-recentf-fix.el ends here