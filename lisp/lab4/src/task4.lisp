(defun without-last(list)
(reverse (cdr (reverse list))))

(defun without-last2(list)
(remove (car (last list)) list))

(print (without-last '(1 2 3 4)))
(print (without-last2 '(1 2 3 4)))