(defun recnth (n lst)
  (cond ( (zerop n) (car lst) )
      (t (recnth (1- n) (cdr lst)))
      ))


(print (recnth 3 '(1 2 3 4 5)))
(print (nth 3 '(1 2 3 4 5)))