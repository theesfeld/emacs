(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize 'force
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t     ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(when (find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
  (set-face-attribute 'default nil
                      :font "BerkeleyMonoVariable Nerd Font Mono"
                      :height 140))

;; Set variable-pitch font (optional, for prose or Org-mode)
(when (find-font (font-spec :name "BerkeleyMonoVariable Nerd Font Mono"))
  (set-face-attribute 'variable-pitch nil
                      :font "BerkeleyMonoVariable Nerd Font Mono"
                      :height 160))

;; Customize font-lock faces
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic
                    :weight 'light)
(set-face-attribute 'font-lock-keyword-face nil
                    :weight 'black)

(dolist (variable '(initial-frame-alist default-frame-alist))
  (set variable `((width . (text-pixels . 800))
                  (height . (text-pixels . 900))
                  (horizontal-scroll-bars . nil)
                  (menu-bar-lines . 0) ; alternative to disabling `menu-bar-mode'
                  (tool-bar-lines . 0) ; alternative to disabling `tool-bar-mode'
                  ,@(if x-toolkit-scroll-bars
                        (list
                         '(vertical-scroll-bars . nil)
                         '(scroll-bar-width . 12))
                      (list
                       '(vertical-scroll-bars . right)
                       '(scroll-bar-width . 6))))))

(defun grim-emacs-no-minibuffer-scroll-bar (frame)
  "Remove the minibuffer scroll bars from FRAME."
  (when scroll-bar-mode
    (set-window-scroll-bars (minibuffer-window frame) nil nil nil nil :persistent)))
(add-hook 'after-make-frame-functions #'grim-emacs-no-minibuffer-scroll-bar)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(defvar grim-emacs--file-name-handler-alist file-name-handler-alist)
(defvar grim-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 100 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist grim-emacs--file-name-handler-alist
                  vc-handled-backends grim-emacs--vc-handled-backends)))

(setq package-enable-at-startup t)

;;;; General theme code

(defun grim-emacs-re-enable-frame-theme (_frame)
  "Re-enable active theme, if any, upon FRAME creation.
Add this to `after-make-frame-functions' so that new frames do
not retain the generic background set by the function
`grim-emacs-avoid-initial-flash-of-light'."
  (when-let* ((theme (car custom-enabled-themes)))
    (enable-theme theme)))

(defun grim-emacs-avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, if needed."
  (setq mode-line-format nil)
  (set-face-attribute 'default nil :background "#000000" :foreground "#ffffff")
  (set-face-attribute 'mode-line nil :background "#000000" :foreground "#ffffff" :box 'unspecified)
  (add-hook 'after-make-frame-functions #'grim-emacs-re-enable-frame-theme))

(grim-emacs-avoid-initial-flash-of-light)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
