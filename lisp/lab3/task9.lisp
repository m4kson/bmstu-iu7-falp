(defun how_alike (x y)
    (cond ((or (- x y) (equal x y)) 'the_same)
            ((and (oddp x) (oddp y)) 'both_odd)
            ((and (evenp x) (evenp y)) 'both_even)
            (t 'different)))

(defun how_alike2 (x y)
    (
        if (= x y) 
            'the_same
            (if (and (oddp x) (oddp y))
                'both_odd
                (if (and (evenp x) (evenp y))
                    'both_even
                    'different))
    ))


(print (how_alike2 2 2))
(print (how_alike2 2 4))
(print (how_alike2 1 3))
(print (how_alike2 1 2))