(defun select-between (lst lower-bound upper-bound)
  (cond ((null lst) nil)
        ((and (>= (car lst) lower-bound) (<= (car lst) upper-bound))
         (cons (car lst) (select-between (cdr lst) lower-bound upper-bound)))
        (t (select-between (cdr lst) lower-bound upper-bound))))

(print (select-between '(1 2 3 4 5 6 7 8 9 10) 3 8))