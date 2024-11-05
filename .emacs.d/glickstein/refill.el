;; need to set path first?
(require 'limited)

(defvar refill-mode nil
  "Mode variable for refill minor mode.")
(make-variable-buffer-local 'refill-mode')

(defvar refill-mode nil
  "Mode variable for refill minor mode.")

(make-variable-buffer-local 'refill-mode)

(defun refill-mode (&optional arg)
  "Refill minor mode."
  (interactive "P")
  (setq refil-mode
	(if (null arg)
	    (not refill-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if refill-mode
      (add-hook 'after-change-functions 'refill nil t)
    (remove-hook 'after-change-functions 'refill t)))

(defun refill (start end len)		; start and end of new text, len of old text
  "After a text change, refill the current paragraph."
  (let ((left (if (or (zerop len) (not (before-2nd-word-p start)))
		  start
		(max (save-excursion
		       (goto-char start)
		       (beginning-of-line 0)
		       (point))
		     (save-excursion
		       (goto-char start)
		       (backward-paragraph 1)
		       (point))))))
    (if (or (and (zerop len)
		 (same-line-p start end)
		 (short-line-p end))
	    (and (eq (char-syntax (preceding-char)) ?\) )
		 (looking-at "\\s *$")))
	nil
      (save-excursion
	(fill-region left end nil nil t))))) ; end variable is unnecessary here

(defun before-2nd-word-p (pos)
  "Does pos lie before the second word on the line?"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-syntax-forward (concat "^ " (char-to-string char-syntax ?\n)))
    (skip-syntax-forward " ")
    (< pos (point))))

(defun same-line-p (start end)
  "Are start and end on the same line?"
  (save-excursion
    (goto-char start)
    (end-of-line)
    (<= end (point))))

(defun short-line-p (pos)
  "Does line containing pos stay within `fill-column`?"
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (<= (current-column) fill-column)))

(provide 'refill-mode)
