(defun countries_capitals (lst name)
    (cond ((assoc name lst) (cdr (assoc name lst)))
		((rassoc name lst) (car (rassoc name lst)))
		(T Nil)))

