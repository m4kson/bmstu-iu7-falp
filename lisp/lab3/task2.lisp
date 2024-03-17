(defun abs-add-one (x)
    (
        if (< x 0)
        (- x 1)
        (+ x 1)
    ))

(print (abs-add-one -5))
(print (abs-add-one 5))