(defun square-list (lst &optional result)
  (cond ((null lst) (nreverse result))
      (t (square-list (cdr lst) 
                   (cons (* (car lst) (car lst)) 
                         result)))
                         ))

(print (square-list '(1 2 3 4 5)))