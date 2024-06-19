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

(defun flipud(matrix) ;поворот по вертикали
    (reverse matrix)
)

(defun fliplr(matrix) ;поворот по горизонтали
    (mapcar #'reverse matrix)
)

(defun rq_q(matrix)
    (fliplr (flipud (transpose_matrix(qr_q (transpose_matrix (fliplr (flipud matrix)))))))
)

(defun rq_r(matrix)
    (fliplr (flipud (transpose_matrix(qr_r (transpose_matrix (fliplr (flipud matrix)))))))
)

(defun minor (matrix row-index column-index)
  "Находит минор матрицы для заданных индексов строки и столбца."
  (mapcar #'(lambda (row)
              (remove (nth column-index row) row))
          (remove (nth row-index matrix) matrix)))

(defun determinant-helper (matrix j)
  (if (>= j (length matrix))
      0
      (+ (* (expt -1 j) (nth j (nth 0 matrix))
            (determinant (minor matrix 0 j)))
         (determinant-helper matrix (1+ j)))))


(defun determinant (matrix)
  (cond ((= (length matrix) 1) (car (car matrix)))
        ((= (length matrix) 2) (- (* (nth 0 (nth 0 matrix)) (nth 1 (nth 1 matrix)))
                                   (* (nth 1 (nth 0 matrix)) (nth 0 (nth 1 matrix)))))
        ((<= (length matrix) 0) 0)
        (t (determinant-helper matrix 0))))



(defun cofactor_i(matrix i j)
    (* (expt -1 (+ i j)) (determinant (minor matrix i j)))
)

(defun cofactor (matrix &optional (i 0) (j 0))
  (if (>= i (length matrix))
      nil
      (cons (cofactor-row matrix i j) (cofactor matrix (1+ i)))))

(defun cofactor-row (matrix i &optional (j 0))
  (if (>= j (length (nth 0 matrix)))
      nil
      (cons (cofactor_i matrix i j) (cofactor-row matrix i (1+ j)))))

(defun number-sequence (from to)
  (if (> from to)
      nil
      (cons from (number-sequence (1+ from) to))))


(defun cofactor_matrix(matrix)
    (transpose_matrix (cofactor matrix))
)

(defun scalar-multiply-matrix (scalar matrix)
  (mapcar (lambda (row)
            (mapcar (lambda (element)
                      (* scalar element))
                    row))
          matrix))

(defun inv_matrix(matrix)
    (scalar-multiply-matrix (/ 1 (determinant matrix)) (cofactor_matrix matrix))
)

(defun qz_iteration_a(matrix1 matrix2)
    (mul_matrix (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix1) 
                (rq_q (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2)))
)

(defun qz_iteration_b(matrix1 matrix2)
    (mul_matrix (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2) 
                (rq_q (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2)))
)

(defun qz_iteration_q(matrix1 matrix2)
    (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))
)

(defun qz_iteration_z(matrix1 matrix2)
    (rq_q (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2))
)

(defun eye (n &optional (i 0))
  (if (= i n)
      '()
      (cons (eye-make-row i n) (eye n (+ i 1)))))

(defun eye-make-row (i n &optional (j 0))
  (if (= j n)
      '()
      (cons (if (= i j) 1 0) (eye-make-row i n (+ j 1)))))

; (defun qz_s(matrix1 matrix2 max_iterations &optional (i 0))
;     (if (= i (- max_iterations 1))
;         (qz_iteration_a matrix1 matrix2)
;         (qz_s ))
; )

(defun qz-s (A B &optional (max-iterations 100) (tol 1e-6) (i 0))
  (if (>= i max-iterations)
      A
      (qz-s (qz_iteration_a A B)
                          (qz_iteration_b A B)
                          max-iterations
                          tol
                          ( + i 1))))

(defun qz-t(A B &optional (max-iterations 100) (tol 1e-6) (i 0))
    (if (>= i max-iterations)
      B
      (qz-t (qz_iteration_a A B)
                          (qz_iteration_b A B)
                          max-iterations
                          tol
                          ( + i 1)))
)


(defun qz-q (A B &optional (max-iterations 100) (tol 1e-6) (i 0) 
               (Q-total (eye (length A))))
  (if (>= i max-iterations)
      Q-total
      (qz-q (qz_iteration_a A B)
            (qz_iteration_b A B)
            max-iterations
            tol
            (+ i 1)
            (mul_matrix Q-total (qz_iteration_q A B)))))

(defun qz-z (A B &optional (max-iterations 100) (tol 1e-6) (i 0) 
               (Z-total (eye (length A))))
  (if (>= i max-iterations)
      Z-total
      (qz-z (qz_iteration_a A B)
            (qz_iteration_b A B)
            max-iterations
            tol
            (+ i 1)
            (mul_matrix Z-total (qz_iteration_z A B)))))

(setf oridinary_natural '((2 3 1)
		  (0 5 2)
		  (3 1 0))
)

(setf A '((1 2 3)
		  (4 5 6)
		  (7 8 10))
)

(setf B '((3 2 1)
		  (1 1 1)
		  (2 1 1))
)

(setf C '((3 2 1)
		  (5 2 4)
		  (2 3 6))
)


;(print (qz_iteration_a A C))


;(print (mul_matrix (mul_matrix (transpose_matrix (qz-q A C)) A) (qz-z A C)))

(print (mul_matrix (mul_matrix (qz-q A C) (qz-s A C)) (transpose_matrix (qz-z A C))))

