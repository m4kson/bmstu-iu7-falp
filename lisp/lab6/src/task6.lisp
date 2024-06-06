(defun rec-add-1 (lst)
  (if (null lst)
      0
      (if (listp (car lst))
          (+ (rec-add-1 (car lst)) (rec-add-1 (cdr lst)))
          (+ (car lst) (rec-add-1(cdr lst))))))


(defun rec-add-2 (lst)
  (cond ((null lst) 0)
        ((atom lst) lst)
        (t (+ (rec-add-2 (car lst)) (rec-add-2 (cdr lst))))))

(setq lst '(1 2 (3 4) 5 (6 (7 8)) 9))
(print (rec-add-1 lst)) ; Выведет: 45

(setq lst '(1 2 (3 (4 5) 6) (7 8 (9 (10))))) 
(print (rec-add-2 lst)) ; Выведет: 55
