; (defun func (lst &optional neg &optional pos)
;     (cond
;         ((null (car lst))
;             (nconc neg pos))
;         ((listp (car lst))
;             (func (nconc (car lst) (cdr lst)) neg pos))
;         ((symbolp (car lst))
;             (func (cdr lst) neg pos))
;         ((< (car lst) 0)
;             (func (cdr lst) (cons (car lst) neg) pos))
;         ((> (car lst) 0)
;             (func (cdr lst) neg (cons (car lst) pos)))))


; (print rec '(1 2 -4 (-2 s) 4 5))

(defun func (lst neg pos)
    (cond
        ((null lst)
            (nconc neg pos))
        ((listp (car lst))
            (func (nconc (car lst) (cdr lst)) neg pos))
        ((symbolp (car lst))
            (func (cdr lst) neg pos))
        ((vectorp (car lst))
            (func (cdr lst) neg pos))
        ((< (car lst) 0)
            (func (cdr lst) (cons (car lst) neg) pos))
        ((> (car lst) 0)
            (func (cdr lst) neg (cons (car lst) pos)))))

(defun rec_call (lst)
    (func lst nil nil))

(print (rec_call '(1 2 -4 (-2 s) 4 5)))


(ql:quickload "fiveam")

(fiveam:test test_1
  (fiveam:is (equal (rec '(a 1 2 -3 4 b f -6)) '(-6 -3 4 2 1))))

(fiveam:test test_2
  (fiveam:is (equal (rec '(c 1 d (d (2 (-3)) a) 0 4 b -6)) '(-6 -3 4 2 1))))

(fiveam:test test_3
  (fiveam:is (equal (rec '(d -2 1 nil 3 -2 #(9) t)) '(-2 -2 3 1))))

(fiveam:test test_4
  (fiveam:is (equal (rec '(a d b)) '())))

(fiveam:run!)