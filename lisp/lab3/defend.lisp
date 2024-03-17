(defvar x)
(defvar y)

(defun find_coords (a b c d)
    (
        cond ((> (+ a b) (+ c d)) 
                (cond ((> a b) 
                (
                    (set x (+ c (sqrt (+ a b))))
                    (set y (+ a (sqrt (+ c d))))
                    )))
                ((= a b) ((set x a) (set y b)))
                ((< a b) ((set x a) (set y (sqrt (+ c d))))))
            ((<= (+ a b) (+ c d)) 
                (cond ((> a b) ((set x (+ a (sqrt (- c d)))) (set y (sqrt (- c d)))))
                    ((<= a b) ((set x (+ -c (sqrt d))) (set y (+ c (sqrt -d))))))
                )
    ))

(find_coords 1 2 3 4)
(print x)
(print y)
