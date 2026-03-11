(require 'thingatpt)

(defun bounds-of-number-at-point ()
  "Return the bounds of the decimal integer at point."
  (save-excursion
    (let* ((orig-point (point))
           (sign "[+-]?")
           (digits "[0-9]+")
           (valid-number (concat sign digits)))
      (skip-chars-backward "-+0-9")
      (unless (looking-at valid-number)
        (goto-char orig-point)
        (error "No number at point"))
      (let ((beg (point)))
        (re-search-forward valid-number)
        (cons beg (match-end 0))))))

(put 'number 'bounds-of-thing-at-point 'bounds-of-number-at-point)

(defun number-at-point ()
  "Return the number at point, as a number."
  (let ((bounds (bounds-of-thing-at-point 'number)))
    (string-to-number (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun increment-number-at-point (&optional arg)
  "Increment the number forward from point by ARG (default 1)."
  (interactive "p")
  (let* ((inc (or arg 1))
         (n (number-at-point))
         (bounds (bounds-of-thing-at-point 'number)))
    (delete-region (car bounds) (cdr bounds))
    (insert (number-to-string (+ n inc)))))

(defun decrement-number-at-point (&optional arg)
  "Decrement the number forward from point by ARG (default 1)."
  (interactive "p")
  (increment-number-at-point (- (or arg 1))))

(provide 'incdec)

