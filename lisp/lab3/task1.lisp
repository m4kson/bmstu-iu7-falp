(defun make-even (x) 
    (
        if (evenp x)
        x
        (+ x 1)
    ))

(print (make-even 5))