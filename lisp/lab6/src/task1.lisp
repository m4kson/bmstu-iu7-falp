(defun my-reverse (lst &optional result)
  (cond((null lst) result)
       (t (my-reverse (cdr lst) (cons (car lst) result)))))

(print (my-reverse '(1 2 3)))