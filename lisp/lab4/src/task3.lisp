(defun return-last1(list1)
(car (reverse list1)))

(defun return-last2(list1)
(car (last list1)))

(print (return-last1 '(1 2 3 4)))
(print (return-last2 '(1 2 3 4)))