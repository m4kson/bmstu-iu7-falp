(defun func (lst &optional result)
  (cond ((null lst) result)
        ((and (< (first lst) 10) (> (first lst) 0)) 
         (func (cdr lst) (nconc result (list (first lst)))))
        (t (func (cdr lst) result))))

(print (func '(102 -3 1 -5 2 1010 3)))

