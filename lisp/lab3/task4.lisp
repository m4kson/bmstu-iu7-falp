(defun task4 (x y z) 
    (
        if (or (and (< x y) (> x z)) (and (< x z) (> x y)))
        t
        nil
    ))

(print (task4 1 2 3))
(print (task4 3 2 1))
(print (task4 2 1 3))
(print (task4 2 3 1))