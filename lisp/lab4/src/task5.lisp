(defun swap-first-last (lst)
			(
				nconc 
				(last lst)
				(reverse 
					(cdr 
						(reverse (cdr lst)))
				)
				(list (car lst))
			)
		)

(print (swap-first-last '(1 2 3 4 5)))