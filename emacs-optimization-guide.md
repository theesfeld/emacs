### 1. Replace Third-Party Packages with Built-in Alternatives

#### 1.2 Use Built-in `display-line-numbers-mode` Features
```elisp
;; Instead of manually controlling in each mode, use:
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)

;; Then selectively disable:
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))
```

---

### 2. Startup Performance Optimizations

#### 2.2 Lazy Load Heavy Packages
```elisp
;; Defer org-mode completely until needed:
(use-package org
  :ensure nil
  :defer t
  :commands (org-mode org-agenda org-capture)
  ;; ... rest of config
  )

;; Defer magit more aggressively:
(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :init
  ;; Only set up the binding, don't load anything
  (global-set-key (kbd "C-c g") 'magit-status))
```

#### 2.3 Optimize Native Compilation Settings
```elisp
;; In early-init.el, refine native-comp settings:
(when (native-comp-available-p)
  ;; Reduce parallel jobs to avoid system overload
  (setq native-comp-async-jobs-number (min 4 (num-processors)))

  ;; More selective JIT compilation
  (setq native-comp-jit-compilation-deny-list
        '("/\\.dir-locals\\.el\\'"
          "/\\.\\(git\\|svn\\|hg\\)/.*\\'"
          "/temp\\(?:orary\\)?/.*\\'"
          "/tmp/.*\\'")))
```

---

### 3. Built-in Completion Enhancements

#### 3.1 Enhance Built-in Completion
```elisp
;; Leverage more built-in completion features:
(setq completion-styles '(basic partial-completion emacs22 initials flex))
(setq completion-category-overrides
      '((file (styles . (basic partial-completion)))
        (buffer (styles . (basic flex)))
        (info-menu (styles . (basic)))))

;; Use built-in completion-preview-mode more effectively:
(setq completion-preview-minimum-symbol-length 3)
(setq completion-preview-insert-on-completion t)
(setq completion-auto-help 'lazy)
(setq completion-auto-select 'second-tab)
```

---

### 4. Memory and GC Optimizations

---

### 5. EXWM Optimizations

#### 5.1 Simplify Monitor Detection
```elisp
(defun my/exwm-configure-monitors ()
  "Simplified monitor configuration using xrandr directly."
  (start-process "xrandr-auto" nil "xrandr" "--auto")
  (run-with-timer 0.1 nil #'exwm-randr-refresh))

;; Replace the complex monitor detection with simpler approach
```

#### 5.2 Reduce Subprocess Spawning
```elisp
;; Cache workspace configuration instead of dynamic generation:
(defvar my/exwm-workspace-monitor-cache nil)

(defun my/exwm-cache-monitor-config ()
  "Cache monitor configuration to avoid repeated xrandr calls."
  (setq my/exwm-workspace-monitor-cache
        (exwm-randr--get-monitor-alias)))
```

---
### 7. Leverage More Emacs 30.1 Features

#### 7.2 Use Emacs 30's Improved Completions Buffer
```elisp
;; New in Emacs 30:
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-header-format nil)
(setq completions-max-height 15)
```

---

### 8. Security and Best Practices

#### 8.1 Improve Auth-source Usage
```elisp
;; More secure auth configuration:
(setq auth-sources '("~/.authinfo.gpg"))
(setq auth-source-save-behavior nil)  ; Never save passwords
(setq auth-source-do-cache nil)       ; Don't cache in memory
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)
```

#### 8.2 Network Security
```elisp
;; Enhance TLS security:
(setq gnutls-verify-error t)
(setq gnutls-min-prime-bits 2048)
(setq network-security-level 'high)
(setq nsm-settings-file (expand-file-name "network-security.data" my-tmp-dir))
```

---

### 9. Remove Redundancies

#### 9.1 Consolidate Similar Packages
- Remove `delight` - use `:diminish` in use-package

---

## Validation Commands

After implementing changes:

```elisp
;; Check startup time:
M-x emacs-init-time

;; Profile startup:
emacs --debug-init --timed-requires

;; Check package load order:
M-x describe-variable RET load-history

;; Memory usage:
M-x memory-report
```

---
