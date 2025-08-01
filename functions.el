(defun comment-delete (arg)
  "Delete the first comment on this line, if any. Don't touch the kill-ring.
With prefix ARG, delete comments on that many lines starting with this one."
  (interactive "P")
  (comment-normalize-vars)
  (dotimes (_i (prefix-numeric-value arg))
    (save-excursion
      (beginning-of-line)
      (let ((cs (comment-search-forward (line-end-position) t)))
        (when cs
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (delete-region cs (if (bolp) (1- (point)) (point)))
          (indent-according-to-mode))))
    (if arg (forward-line 1))))

(defun comment-delete-dwim (beg end arg)
  "Delete comments without touching the kill-ring.
- With active region, delete comments in region.
- With prefix ARG, delete comments in whole buffer.
- With neither, delete comments on current line."
  (interactive "r\nP")
  (let ((lines (cond (arg (count-lines (point-min) (point-max)))
                     ((region-active-p) (count-lines beg end)))))
    (save-excursion
      (when lines
        (goto-char (if arg (point-min) beg)))
      (comment-delete (or lines 1)))))

(defun delete-blank-lines-dwim (beg end arg)
  "Delete blank lines (empty or whitespace-only).
- With active region, delete in region.
- With prefix ARG, delete in whole buffer.
- Otherwise, delete around point (like `delete-blank-lines')."
  (interactive "r\nP")
  (save-excursion
    (if arg
        (progn
          (goto-char (point-min))
          (flush-lines "^\\s-*$"))
      (if (region-active-p)
          (flush-lines "^\\s-*$" beg end)
        (delete-blank-lines)))))
