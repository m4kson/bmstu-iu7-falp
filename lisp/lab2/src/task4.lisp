(defun longer_than(list1 list2)
(> (length list1) (length list2)))

(print (longer_than '(1 2 3 4 5) '(1 2 3)))
(print (longer_than '(1 2) '(1 2 3)))