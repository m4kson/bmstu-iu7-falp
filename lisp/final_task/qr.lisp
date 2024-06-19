(defun scalar_mul_vector (vector1 vector2)
    (reduce #'+ (mapcar #'* vector1 vector2))
)

(defun mul_vector_number (vec num)
    (mapcar #'(lambda (x) (* x num)) vec)
)

(defun sub_vector (vector1 vector2)
    (mapcar #'- vector1 vector2)
)

(defun add_list_vector (lst vec)
    (append lst (list vec))
)

(defun project_vector (vector1 vector2)
    (cond ((equal (scalar_mul_vector vector2 vector2) 0) nil) ; inache delenie na 0
  	    (T (mul_vector_number vector2 (/ (scalar_mul_vector vector1 vector2) (scalar_mul_vector vector2 vector2)))))
)

(defun len_vector (vec)
    (sqrt (reduce #'(lambda (i j) (+ i (* j j ))) (cons 0 vec)))
)



(defun normalize_column (column)
    (cond ((equal (len_vector column) 0) nil)
  	    (T (mapcar #'(lambda (x) (/ x (len_vector column))) column)))
)

(defun normalize_matrix (matrix)
    (mapcar #'normalize_column matrix)
)

(defun transpose_matrix (matrix)
    (apply #'mapcar (cons #'list matrix))
)

(defun mul_matrix (matrix1 matrix2)
    (mapcar #'(lambda (row)
	    (mapcar #'(lambda (col)
			(apply #'+ (mapcar #'* row col)))
		    (apply #'mapcar (cons 'list matrix2)))) matrix1)
)

(defun is_null_vector (vec)
    (reduce #'(lambda (x y) (and x y)) (mapcar #'(lambda (num) (= num 0)) vec))
)

(defun is_null_matrix (mat)
    (reduce #'(lambda (x y) (and x y)) (mapcar #'is_null_vector mat))
)


(defun grama_shmidt_i (vec bn)
    (reduce #'sub_vector (cons vec (mapcar #'(lambda (tmp) (project_vector vec tmp)) bn)))
)

(defun grama_shmidt_rec (matrix bn)
    (cond ((null matrix) bn)
	    (T (grama_shmidt_rec (cdr matrix) (add_list_vector bn (grama_shmidt_i (car matrix) bn)))))
)

(defun grama_shmidt (matrix)
    (grama_shmidt_rec matrix nil)
)

(defun qr_q (matrix)
    (cond ((is_null_matrix matrix) nil)
  	    (T (transpose_matrix (normalize_matrix (grama_shmidt (transpose_matrix matrix))))))
)

(defun qr_r (matrix)
    (cond ((is_null_matrix matrix) nil)
        (T (mul_matrix (normalize_matrix (grama_shmidt (transpose_matrix matrix))) matrix)))
)



(defun float_cmp (num1 num2 eps)
    (< (abs (- num1 num2)) eps)
)

(defun vector_cmp (vector1 vector2 eps)
    (reduce #'(lambda (x y) (and x y)) (mapcar #'(lambda (num1 num2) (float_cmp num1 num2 eps)) vector1 vector2))
)

(defun matrix_compare (matrix1 matrix2 eps)
    (reduce #'(lambda (x y) (and x y)) (mapcar #'(lambda (vec1 vec2) (vector_cmp vec1 vec2 eps)) matrix1 matrix2))
)


(setf null_mat '((0 0 0)
		  (0 0 0)
		  (0 0 0))
)

(setf oridinary_natural '((1 5 3)
		  (6 4 2)
		  (7 5 2))
)

(setf q_mat2 '(( 0.1078327  0.9928949  0.0503154)
	       ( 0.6469966 -0.1085131  0.7547319)
	       ( 0.7548294 -0.0488309 -0.6541009))
)
(setf r_mat2 '(( 9.2736184  6.9012974  3.1271504)
	       ( 0.0000000  4.2862679  2.6639969)
	       ( 0.0000000  0.0000000  0.3522082))
)

(setf one_mat '((1 0 0)
		  (0 1 0)
		  (0 0 1))
)
(setf q_mat3 '(( 1.0000000  0.0000000  0.0000000)
	       ( 0.0000000  1.0000000  0.0000000)
	       ( 0.0000000  0.0000000  1.0000000))
)
(setf r_mat3 '(( 1.0000000  0.0000000  0.0000000)
	       ( 0.0000000  1.0000000  0.0000000)
	       ( 0.0000000  0.0000000  1.0000000))
)

(setf oridinary_natural2 '((7 5 3)
		  (8 7 3)
		  (7 1 2))
)
(setf q_mat4 '(( 0.5499719  0.1930789  0.8125585)
	       ( 0.6285393  0.5449809 -0.5549180)
	       ( 0.5499719 -0.8159143 -0.1783665))
)
(setf r_mat4 '((12.7279220  7.6996071  4.6354777)
	       ( 0.0000000  3.9643472  0.5823510)
	       ( 0.0000000  0.0000000  0.4161885))
)

(setf two_cols '((1 5)
		  (7 5)
		  (6 4))
)
(setf q_mat5 '(( 0.1078327  0.9928949)
	       ( 0.7548294 -0.0488309)
	       ( 0.6469966 -0.1085131)))
(setf r_mat5 '(( 9.2736184  6.9012974)
	       ( 0.0000000  4.2862679))
)

(setf one_elem '((1)))
(setf q_mat6 '((1)))
(setf r_mat6 '((1)))

(setf one_col '((1)
		  (6)
		  (2))
)
(setf q_mat7 '(( 0.1561737)
	       ( 0.9370425)
	       ( 0.3123475))
)
(setf r_mat7 '(( 6.4031242))
)

(setf twenty
  '((1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0  0.0)
   (0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  0.0  1.0))
)

(setf big '((3 9 3 9 4 8 8 5 0 4 8 1 5 9 2 5 3 2 8 6 )
(0 2 2 9 3 0 1 9 7 8 7 0 7 2 2 2 0 0 9 3)
(5 9 4 0 0 8 8 3 0 6 1 0 0 5 0 4 5 3 3 4)
(1 2 5 8 4 7 2 7 7 3 0 2 2 6 5 4 4 3 9 6)
(1 2 6 2 7 8 6 5 1 1 9 2 4 4 3 0 3 7 7 1)
(1 9 5 5 5 2 0 1 5 1 7 9 4 6 1 1 4 9 6 8)
(2 6 2 8 2 5 9 6 3 6 9 6 8 4 1 3 7 3 7 4)
(5 6 3 9 2 6 2 9 5 9 7 8 7 1 6 9 7 7 7 2)
(6 6 8 4 3 1 9 2 5 6 6 0 3 2 9 5 8 1 6 6)
(2 3 6 9 5 4 1 4 2 0 8 0 7 8 4 0 9 3 4 4)
(0 2 4 5 4 5 2 5 7 9 3 9 2 9 1 9 5 4 5 9)
(4 3 9 3 1 3 5 1 9 9 7 1 2 2 6 8 7 8 3 6)
(7 8 8 2 7 1 3 5 5 9 4 1 4 6 7 8 1 2 1 0)
(4 8 1 6 2 7 6 2 8 2 8 5 0 6 9 0 7 3 5 4)
(4 1 6 0 7 5 0 9 7 3 9 3 4 3 1 6 2 8 8 0)
(0 9 8 0 5 7 0 5 2 7 9 6 9 7 9 8 2 9 7 2)
(3 9 5 9 4 7 5 6 5 6 7 7 5 5 7 2 4 0 7 7)
(7 9 5 8 8 4 7 1 6 6 5 1 7 2 0 1 9 7 0 6)
(3 7 3 0 4 3 3 8 3 2 5 2 3 1 1 2 7 8 5 5)
(6 0 6 4 2 8 7 4 6 7 0 1 6 6 2 0 9 7 9 4)))


(print (qr_q big))
(print (qr_r big))


(load "~/quicklisp/setup.lisp")
(ql:quickload "fiveam")

; Негативы

(fiveam:test NULL_Q (fiveam:is (equal (qr_q null_mat) nil)))
(fiveam:test NULL_R (fiveam:is (equal (qr_r null_mat) nil)))

; Позитивы

(fiveam:test oridinary_natural1 (fiveam:is (matrix_compare (mul_matrix (qr_q oridinary_natural) (qr_r oridinary_natural)) oridinary_natural 1e-6)))
(fiveam:test oridinary_natural2 (fiveam:is (matrix_compare (mul_matrix (qr_q oridinary_natural2) (qr_r oridinary_natural2)) oridinary_natural2 1e-6)))
(fiveam:test one_matrix (fiveam:is (matrix_compare (mul_matrix (qr_q one_mat) (qr_r one_mat)) one_mat 1e-6)))
(fiveam:test two_cols (fiveam:is (matrix_compare (mul_matrix (qr_q two_cols) (qr_r two_cols)) two_cols 1e-6)))
(fiveam:test one_elem (fiveam:is (matrix_compare (mul_matrix (qr_q one_elem) (qr_r one_elem)) one_elem 1e-6)))
(fiveam:test one_col (fiveam:is (matrix_compare (mul_matrix (qr_q one_col) (qr_r one_col)) one_col 1e-6)))
(fiveam:test twenty_na_twenty (fiveam:is (matrix_compare (mul_matrix (qr_q twenty) (qr_r twenty)) twenty 1e-6)))

(fiveam:run!)

