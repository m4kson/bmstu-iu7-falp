; (setf b '(* + cons append))
; (setf a 'b)
; ; (defun a (n lst) (nth n lst))
; ; (defun b (n lst) (apply a n (eval a)))

; (defun myfunc (lst)
;     (
;         (cond ((symbolp (car lst)) (mapcar #'(lambda (x) 
;                                                     (
;                                                         if (symbolp x)
;                                                             (x)))))
;             (t nil)
;         )
;     ))

; (print (myfunc (a 3 2 d c)))

(defun f (lst)
(cond ((symbolp (car lst)) (mapcan #'(lambda (el) (and (symbolp el) (list el))) lst))
((and (numberp (car lst)) (> (car lst) 0)) (mapcan #'(lambda (el) (and (numberp el) (list (+ el 2)))) lst))
(T (mapcan #'(lambda (el) (and (numberp el) (list (* el el)))) lst))
)
)

(print (f (a 3 2 d c)))