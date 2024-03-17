(defun poly(list)
    (
        equal list (reverse list)
    ))

(print (poly '(1 2 2 1)))
(print (poly '(1 2 2 3)))