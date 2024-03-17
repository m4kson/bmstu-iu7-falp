(defun task4_1 (x y z) 
    (
        if (< x y)
        (if (> x z) t nil)
        (if (< x z) (if (> x y) t nil) nil)
    ))

(defun task4_2 (x y z) 
    (
        cond ((> x y) (cond ((< x z) t) (t nil))) ((< x y) (cond ((> x z) t) (t nil)))
    ))

(defun task4_3 (x y z)
    (
        or (and (< x y) (> x z)) (and (< x z) (> x y))
    ))


(print (task4_3 1 2 3))
(print (task4_3 3 2 1))
(print (task4_3 2 1 3))
(print (task4_3 2 3 1))