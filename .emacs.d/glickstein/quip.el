;; From "Writing GNU Emacs Extensions" by Bob Glickstein

;; What else should Quip mode be able to do?
;;     It should allow the user to move forward and backward a quip at a time.
;;     It should allow the user to restrict editing operations to a single quip.
;;     It should be able to report the number of quips in the file, and the number of the quip that point is on.
;;     Apart from that, it should work by and large the same way Text mode works. After all, the contents are mostly plain text.

(require 'derived)

(defvar quip-mode-hook nil
  "*List of functions to call when entering Quip mode.")

;; (defalias 'backward-quip 'backward-page)
;; (defalias 'forward-quip 'forward-page)
;; (defalias 'narrow-to-quip 'narrow-to-page)
;; (defalias 'what-quip 'what-page)

(defvar quip-mode-map nil
  "Keymap for quip major mode.")

;; (if quip-mode-map
;;     nil					; do nothing if quip-mode-map exists
;;   ;; (setq quip-mode-map (make-sparse-keymap))
;;   (setq quip-mode-map (copy-keymap text-mode-map))
;;   (define-key quip-mode-map "\C-x[" 'backward-quip)
;;   (define-key quip-mode-map "\C-x]" 'forward-quip)
;;   (define-key quip-mode-map "\C-xnq" 'narrow-to-quip)
;;   (define-key quip-mode-map "\C-cw" 'what-quip))

;; (defun quip-mode ()
;;   "Major mode for editing Quip files.
;; Special commands:
;; \\{quip-mode-map}"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (text-mode)
;;   (setq major-mode 'quip-mode)
;;   (setq mode-name "Quip")
;;   (make-local-variable 'paragraph-separate)
;;   (make-local-variable 'paragraph-start)
;;   (make-local-variable 'page-delimiter)
;;   (setq paragraph-start "%%\\|[ \t\n\^L]")
;;   (setq paragraph-separate "%%$\\|[ \t\^L]*$")
;;   (setq page-delimiter "^%%$")
;;   (use-local-map quip-mode-map)      ; this installs the keymap
;;   (run-hooks 'quip-mode-hook))

(define-derived-mode quip-mode text-mode "Quip"
  "Major mode for editing Quip files.
Special commands:
\\{quip-mode-map}"
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (make-local-variable 'page-delimiter)
  (setq paragraph-start "%%\\|[ \t\n\^L]")
  (setq paragraph-separate "%%$\\|[ \t\^L]*$")
  (setq page-delimiter "^%%$"))
(define-key quip-mode-map "\C-x[" 'backward-quip)
(define-key quip-mode-map "\C-x]" 'forward-quip)
(define-key quip-mode-map "\C-xnq" 'narrow-to-quip)
(define-key quip-mode-map "\C-cw" 'what-quip)

(defun count-quips ()
  "Count the quips in the buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (count-matches "^%%$"))))

(provide 'quip)
