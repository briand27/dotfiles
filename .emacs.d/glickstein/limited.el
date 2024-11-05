;; See "Chapter 8: Evaluation and Error Recovery" from "Writing GNU Emacs Extensions"
(defmacro limited-save-excursion (&rest subexprs)
  "Like save-excursion, but only restores point."
  (let ((orig-point-symbol (make-symbol "orig-point")))
    `(let ((,orig-point-symbol (point-marker)))
       (unwind-protect
           (progn ,@subexprs)
         (goto-char ,orig-point-symbol)))))

(provide 'limited)
