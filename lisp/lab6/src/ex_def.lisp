; (defun merge_sort (list1 &optional list2)
;     (
;         cond ((and (null list1) (null list2)) nil)
;             ((null list1) (list2))
;             ((null list2) (list1))
;             ((and (atom lst1) (atom lst2)) 
;                     (cond ((<= list1 list2) (list list1 list2))
;                             (t (list list2 list1))))
;             ((atom list1) 
;                     (cond((<= list1 (car list2)) (cons list1 list2))
;                         (t (cons list2 (merge_sort list1 (cdr list2))))))
;             ((atom list2) 
;                     (cond((<= list2 (car list1)) (cons list2 list1))
;                         (t (cons list1 (merge_sort list2 (cdr list1))))))
;             ((<= (car list1) (car list2)) (cons (car list1) (car list2) (merge_sort (cdr list1) (cdr list2))))
;             (t (cons (car list2) (car list1) (merge_sort (cdr list2) (cdr list1)))))
;     )

(defun flatten-sort (lst)
  (cond
    ((null lst) nil)
    ((atom lst) (list lst))
    (t (merge-sort (flatten-sort (car lst)) (flatten-sort (cdr lst))))))

(defun merge-sort (lst1 lst2)
  (cond
    ((null lst1) lst2)
    ((null lst2) lst1)
    ((< (car lst1) (car lst2)) (cons (car lst1) (merge-sort (cdr lst1) lst2)))
    (t (cons (car lst2) (merge-sort lst1 (cdr lst2))))))


(print (flatten-sort '(3 1 2 (3 9 (10 8 1) 8) 6 2)))

