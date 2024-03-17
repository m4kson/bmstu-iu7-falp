(defun mylist (x y)
    (
        if (< x y)
        (cons x (cons y nil))
        (cons y (cons x nil))
    ))

(print (mylist 3 1))
(print (mylist 1 3))