(defun mult_a (n lst)
	(cond ((and (numberp (car lst)) (numberp (cadr lst)) 
            (numberp (caddr lst)) (numberp n))
		(* (car lst) n))
		(T Nil)))

(defun mult_b (n lst)
    (cond ((numberp (car lst)) (* (car lst) n))
            ((numberp (cadr lst)) (* (cadr lst) n))
            ((numberp (caddr lst)) (* (caddr lst) n))))

(print (mult_a 3 '(3 2 1)))
(print (mult_b 3 '(a this 3)))