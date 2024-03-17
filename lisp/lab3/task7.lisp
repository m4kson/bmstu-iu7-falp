(defun pred1 (x)
    (
        and (numberp x) (plusp x)
    ))

(defun pred2 (x)
    (
        and (plusp x) (numberp x)
    ))

(print (pred1 2))
(print (pred2 2))