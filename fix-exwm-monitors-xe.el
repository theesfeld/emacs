;;; fix-exwm-monitors-xe.el --- Fix for Intel Xe monitor detection  -*- lexical-binding: t; -*-

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

;; Temporary fix for Intel Xe driver monitor detection issues with EXWM

;;; Code:

(defun fix-exwm-monitors-xe ()
  "Fix monitor configuration for Intel Xe driver."
  (interactive)
  (shell-command "xrandr --output DP-3-1-6 --auto --primary --pos 0x0 --output DP-3-2 --auto --pos 3840x0 --output eDP-1 --scale 0.67x0.67 --pos 7680x0")
  (when (fboundp 'exwm-randr-refresh)
    (exwm-randr-refresh))
  (message "Monitor configuration fixed for Intel Xe driver"))

(fix-exwm-monitors-xe)

(provide 'fix-exwm-monitors-xe)
;;; fix-exwm-monitors-xe.el ends here