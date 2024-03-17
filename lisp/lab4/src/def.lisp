(setf b '(* + cons append))
(setf a 'b)

(defun a (n lst) (nth n lst))
(defun b (n lst) (append (list (apply #'a (list n (eval a)))) lst))

(print (funcall #'b 1 '(2 3)))
(print (eval(funcall #'b 1 '(2 3))))

(defun myfunc (lst) 
    (
        cond ((symbolp (first lst)) (mapcan #'(lambda (x) (and (symbolp x) (list x))) lst))
             ((and (numberp (first lst)) (> (first lst) 0)) (mapcan #'(lambda (x) (and (numberp x) (list (eval(funcall #'b 1 (list x 2)))))) lst))
             (t (mapcan #'(lambda (x) (and (numberp x) (list (eval(funcall #'b 0 (list x x)))))) lst))
    ))

(print (myfunc '(-2 d 1 2 d 3 j f)))
