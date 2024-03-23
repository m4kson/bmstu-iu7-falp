(defun ret-sublist(lst)
    (
       cond ((null (car lst)) (ret-sublist (cdr lst)))
            ((numberp (car lst)) (ret-sublist (cdr lst)))
            (t (car lst))
            
    ))

(print (ret-sublist '(1 2 nil '(2 2) 33 4)))