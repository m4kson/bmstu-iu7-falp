(ql:quickload "fiveam")
(in-package :fiveam)

(defun f (lst)
  (cond ((every #'(lambda (elem)
                     (cond ((> elem 0) T)
                           (T nil)))
                (first lst))
         (mapcar #'(lambda (sub_lst)
                     (mapcar #'(lambda (elem)
                                 (* elem 2))
                             sub_lst))
                 lst))
        ((every #'(lambda (elem)
                     (cond ((< elem 0) T)
                           (T nil)))
                (first lst))
         (mapcar #'(lambda (sub_lst)
                     (* (first sub_lst) (second sub_lst)))
                 lst))
        (t (apply #'+ (mapcan #'(lambda (sub_lst)
                                  (remove-if-not #'numberp sub_lst))
                              lst)))))

(print (f '((1 2) (3 4) (5 6))))
(print (f '((-1 -2) (3 4) (5 6))))
(print (f '((-1 2) (3 4) (5 6))))


(test test-positive-case ()
    (is (equalp '((2 4) (6 8) (10 12)) (f '((1 2) (3 4) (5 6))))))
  
(test test-negative-case ()
    (is (equalp '(2 12 30)  (f '((-1 -2) (3 4) (5 6))))))
  
(test test-mixed-case ()
    (is (= 19 (f '((-1 2) (3 4) (5 6))))))

(run!)
