;;; recentf-exwm-fix.el --- Fix recentf interrupting EXWM input  -*- lexical-binding: t; -*-

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

;; Fix for recentf causing "Buffer is read-only" errors in EXWM windows

;;; Code:

(defun cancel-recentf-default-timer ()
  "Cancel any default recentf periodic timer."
  (dolist (timer timer-list)
    (when (and (timerp timer)
               (eq (timer--function timer) 'recentf-save-list)
               (timer--repeat-delay timer))
      (cancel-timer timer))))

(defun setup-exwm-safe-recentf ()
  "Configure recentf to not interrupt EXWM input."
  (when (boundp 'recentf-mode)
    ;; First, cancel any existing timers
    (cancel-recentf-default-timer)
    
    ;; Disable the default periodic cleanup/save
    (setq recentf-auto-cleanup nil)
    
    ;; Remove any existing save hooks that might cause issues
    (remove-hook 'kill-buffer-hook 'recentf-track-closed-file)
    (remove-hook 'write-file-functions 'recentf-track-opened-file)
    
    ;; Add back the tracking hooks without automatic saving
    (add-hook 'find-file-hook 'recentf-track-opened-file)
    (add-hook 'kill-buffer-hook 'recentf-track-closed-file)
    
    ;; Only save on Emacs exit
    (add-hook 'kill-emacs-hook 'recentf-save-list)
    
    (message "Recentf configured for EXWM compatibility")))

;; Apply the fix immediately
(setup-exwm-safe-recentf)

(provide 'recentf-exwm-fix)
;;; recentf-exwm-fix.el ends here