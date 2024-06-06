(defun rec-first-odd (lst)
  (cond ((null lst) nil) 
        ((atom lst)
         (if (oddp lst) lst 
             nil))
        (t 
         (or (rec-first-odd (car lst)) 
             (rec-first-odd (cdr lst)))))) 

(print (rec-first-odd '(2 (4 (6 1 2) 3) 4 5)))