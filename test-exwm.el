;;; test-exwm.el --- Test EXWM loading -*- lexical-binding: t; -*-

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

;; Test script to verify EXWM loads correctly with monitor configuration

;;; Code:

(defun test-exwm-monitor-config ()
  "Test EXWM monitor configuration functions in isolation."
  (message "\n=== Testing EXWM Monitor Configuration ===")
  
  ;; Test 1: Check xrandr availability
  (message "\n[Test 1] Checking xrandr availability...")
  (if (executable-find "xrandr")
      (message "✓ xrandr found")
    (error "✗ xrandr not found in PATH"))
  
  ;; Test 2: Get current monitor configuration
  (message "\n[Test 2] Getting current monitor configuration...")
  (condition-case err
      (let ((xrandr-output (shell-command-to-string "xrandr")))
        (message "✓ xrandr output captured (%d chars)" (length xrandr-output))
        
        ;; Parse connected monitors
        (let* ((connected-monitors
                (seq-filter (lambda (line)
                              (string-match-p " connected" line))
                            (split-string xrandr-output "\n")))
               (monitor-names
                (mapcar (lambda (line)
                          (car (split-string line)))
                        connected-monitors)))
          (message "✓ Found %d connected monitors: %s" 
                   (length monitor-names) 
                   (mapconcat #'identity monitor-names ", "))))
    (error (message "✗ Error getting monitor info: %s" (error-message-string err))))
  
  ;; Test 3: Test the problematic string-match-p pattern
  (message "\n[Test 3] Testing eDP-1 detection pattern...")
  (condition-case err
      (let ((test-output (shell-command-to-string "xrandr")))
        (if (string-match-p "eDP-1.*[0-9]+x[0-9]+\\+" test-output)
            (message "✓ eDP-1 pattern matches current output")
          (message "✓ eDP-1 pattern does not match (monitor may be off/disconnected)")))
    (error (message "✗ Error testing pattern: %s" (error-message-string err))))
  
  ;; Test 4: Check if EXWM is available
  (message "\n[Test 4] Checking EXWM availability...")
  (if (require 'exwm nil t)
      (message "✓ EXWM package loaded successfully")
    (message "✗ EXWM package not available"))
  
  (message "\n=== Test Complete ===\n"))

;; Run the test
(test-exwm-monitor-config)

(provide 'test-exwm)
;;; test-exwm.el ends here