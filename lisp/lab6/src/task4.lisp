(defun multyply-all-1(lst n)
    (
        cond ((null lst) nil)
            (t (cons (* (car lst) n) (multyply-all-1 (cdr lst) n)))
    ))

(defun multyply-all-2 (lst n)
    (
        cond ((null lst) nil)
            ((numberp (car lst)) (cons (* (car lst) n) (multyply-all-2 (cdr lst) n)))
            (t (cons (car lst) (multyply-all-2 (cdr lst) n))) 
    ))

(print (multyply-all-1 '(1 2 3 4) 4))
(print (multyply-all-2 '(a 1 fdf 2 df 3 4) 4))