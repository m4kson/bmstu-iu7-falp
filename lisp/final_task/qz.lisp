(defun scalar_mul_vector (vector1 vector2)
"Функция скалярного умножения двух векторов"
    (reduce #'+ (mapcar #'* vector1 vector2))
)

(defun mul_vector_number (vec num)
"Функция умножения вектора на число"
    (mapcar #'(lambda (x) (* x num)) vec)
)

(defun sub_vector (vector1 vector2)
"Функция разности векторов"
    (mapcar #'- vector1 vector2)
)

(defun add_list_vector (lst vec)
"Функция добавляет вектор vec в конец списка lst"
    (append lst (list vec))
)

(defun project_vector (vector1 vector2)
"Функция проекции вектора 1 на вектор 2"
    (cond ((equal (scalar_mul_vector vector2 vector2) 0) nil)
  	    (T (mul_vector_number vector2 (/ (scalar_mul_vector vector1 vector2) (scalar_mul_vector vector2 vector2)))))
)

(defun len_vector (vec)
"Вычисляет норму вектора"
    (sqrt (reduce #'(lambda (i j) (+ i (* j j ))) (cons 0 vec)))
)

(defun normalize_column (column)
"Фунцция нормализации столбца"
    (cond ((equal (len_vector column) 0) nil)
  	    (T (mapcar #'(lambda (x) (/ x (len_vector column))) column)))
)

(defun normalize_matrix (matrix)
"Функция нормализации матрицы"
    (mapcar #'normalize_column matrix)
)

(defun transpose_matrix (matrix)
"Транспозиция матрицы"
    (apply #'mapcar (cons #'list matrix))
)

(defun mul_matrix (matrix1 matrix2)
"Умножение матриц"
    (mapcar #'(lambda (row)
	    (mapcar #'(lambda (col)
			(apply #'+ (mapcar #'* row col)))
		    (apply #'mapcar (cons 'list matrix2)))) matrix1)
)

(defun is_null_vector (vec)
"Функция проверяет, является ли вектор нулевым"
    (reduce #'(lambda (x y) (and x y)) (mapcar #'(lambda (num) (= num 0)) vec))
)

(defun is_null_matrix (matrix)
"Функция проверяет, является ли матрица нулевой"
    (reduce #'(lambda (x y) (and x y)) (mapcar #'is_null_vector matrix))
)

(defun grama_shmidt_i (vec result)
    (reduce #'sub_vector (cons vec (mapcar #'(lambda (tmp) (project_vector vec tmp)) result)))
)

(defun grama_shmidt_rec (matrix result)
    (cond ((null matrix) result)
	    (T (grama_shmidt_rec (cdr matrix) (add_list_vector result (grama_shmidt_i (car matrix) result)))))
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

(defun flipud(matrix) 
"поворот по вертикали"
    (reverse matrix)
)

(defun fliplr(matrix)
"поворот по горизонтали"
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
  (mapcar (lambda (row)
            (concatenate 'list
                         (subseq row 0 column-index)
                         (subseq row (1+ column-index))))
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
    (scalar-multiply-matrix (/ 1.0 (determinant matrix)) (cofactor_matrix matrix))
)

(defun qz_iteration_a(matrix1 matrix2)
    (mul_matrix (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix1) 
                (transpose_matrix (rq_q (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2))))
)

(defun qz_iteration_b(matrix1 matrix2)
    (mul_matrix (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2) 
                (transpose_matrix (rq_q (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2))))
)

(defun qz_iteration_q(matrix1 matrix2)
    (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))
)

(defun qz_iteration_z(matrix1 matrix2)
    (transpose_matrix (rq_q (mul_matrix (transpose_matrix (qr_q (mul_matrix matrix1 (inv_matrix matrix2)))) matrix2)))
)

(defun eye (n &optional (i 0))
  (if (= i n)
      '()
      (cons (eye-make-row i n) (eye n (+ i 1)))))

(defun eye-make-row (i n &optional (j 0))
  (if (= j n)
      '()
      (cons (if (= i j) 1.0 0.0) (eye-make-row i n (+ j 1)))))

(defun qz-s (A B &optional (max-iterations 10) (i 0))
  (if (>= i max-iterations)
      A
      (qz-s (qz_iteration_a A B)
                          (qz_iteration_b A B)
                          max-iterations
                          ( + i 1))))

(defun qz-t(A B &optional (max-iterations 10) (i 0))
    (if  (>= i max-iterations)
      B
      (qz-t (qz_iteration_a A B)
                          (qz_iteration_b A B)
                          max-iterations
                          ( + i 1)))
)


(defun qz-q (A B &optional (max-iterations 10) (i 0) 
               (Q-total (eye (length A))))
  (if (>= i max-iterations)
      Q-total
      (qz-q (qz_iteration_a A B)
            (qz_iteration_b A B)
            max-iterations
            (+ i 1)
            (mul_matrix Q-total (qz_iteration_q A B)))))

(defun qz-z (A B &optional (max-iterations 10) (i 0) 
               (Z-total (eye (length A))))
  (if (>= i max-iterations)
      Z-total
      (qz-z (qz_iteration_a A B)
            (qz_iteration_b A B)
            max-iterations
            (+ i 1)
            (mul_matrix Z-total (qz_iteration_z A B)))))

(defun square(matrix)
    (if (equal (length matrix) (length (car matrix)))
        t
        nil)
)


(defun qz-decomposition-q(A B &optional (max-iterations 10))
    (if (and (square A) (square B) (equal (length A) (length B)))
        (qz-q A B max-iterations)
        nil
    )
)

(defun qz-decomposition-z(A B &optional (max-iterations 10) )
    (if (and (square A) (square B) (equal (length A) (length B)))
        (qz-z A B max-iterations)
        nil
    )
)

(defun qz-decomposition-s(A B &optional (max-iterations 10))
    (if (and (square A) (square B) (equal (length A) (length B)))
        (qz-s A B max-iterations)
        nil
    )
)

(defun qz-decomposition-t(A B &optional (max-iterations 10))
    (if (and (square A) (square B) (equal (length A) (length B)))
        (qz-t A B max-iterations)
        nil
    )
)

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

(setf A5 '((1 2 3 4 5)
          (6 7 8 1 1)
          (11 2 7 14 15)
          (1 17 18 12 20)
          (9 24 23 6 25)))

(setf B5 '((5 4 5 2 1)
          (2 2 2 4 5)
          (8 14 3 2 1)
          (1 2 9 6 15)
          (15 3 3 9 18)))


(setf A_float '((1.2 2.2 3.5)
		  (4.3 5.2 6.99)
		  (7.14 8.2 10.1))
)

(setf B_float '((3.22 2.5 1.43)
		  (1.1 1.2 1.3)
		  (2.22 1.9 1.5)))


(setf A_not_square '((1 2 3)
		             (4 5 6)
		             (7 8 10)
                     (7 6 4))
)

(setf B_not_square '((3 2 1)
		            (1 1 1)
		            (2 1 1)
                    (9 9 9))
)

(setf A_big '(( -8  -4   1  -8   2   9  -2   5  -8   0)
            ( -2   0  -6  -4  -9   5   7 -10   8  -9)
            (  1   8   9   3  -6  -1  -7  -6   3   6)
            (  2   1   4  -1  -1  -7  -7   8   0  -6)
            ( -8   9  -5   8  -3   3   6   1   0   6)
            ( -1   7  -4   4   9  -3  -1  -6   6 -10)
            ( -2  -7 -10   6  -1  -4   7   9   1   8)
            ( -1  -9   6  -7   0  -4   1   9   8  -3)
            ( -6  -4   7  -3   9   8   3   4   1   4)
            (  5   0  -4  -9   5 -10  -4   4  -3  -3)
))

(setf B_big '(( -6  -4  -3  -6   5   6  -4   2   9 -10)
            (  7  -3  -8  -9 -10 -10   1  -9  -1  -7)
            (  8   2  -8   2  -7  -1   7   5  -4  -7)
            ( -6   9   7   9  -7  -9  -9  -3   0   2)
            (  4   0   2 -10   2   6   9  -6   0  -9)
            ( -5  -3  -9   2  -7  -7  -3  -3  -1   4)
            (  1  -5  -6   9   5   2   8  -2  -3   0)
            (-10   3   0  -1  -6  -5   9   7  -3  -8)
            ( -8  -1   8   1  -1   1   7  -4  -6   7)
            (  2   1  -5  -9   9   0  -3  -4   0   0)
))

(setf A20 '((  0  -4   7  -9  -4   5   7   5   7  -3   7  -7   0   3   3   2   8 -3  8  0)
 (  0  -7   8  -9  -2  -9   7   7   8   8   5   4  -6  -4   8  -1   9   9 -8  -6)
 ( -7 -10   1  -2  -1   6  -2  -8  -7   7  -5   8  -9   7   5   4   1  -1 2   0)
 ( -1  -1  -9   6  -6  -2  -6   9  -5   9 -10 -10   3  -6 -10 -10  -6   6 -3   4)
 (  2 -10  -1  -6   8   6  -9 -10   1   0   6  -5   6  -9   5   0  -4   2 3   4)
 ( -2   3 -10 -10   6  -9   8 -10   9   7  -7   4  -4  -5  -7   4   1  -8 9   5)
 (  7   7   8   8  -2  -1  -8   6   0  -8   7   5  -2  -1  -4   4 -10 -10 4  -3)
 ( -2   8   0   5  -6   5   6 -10  -4  -7   8  -5   9  -1  -6   6  -3  -9 5   1)
 ( -5   1  -8   3  -3  -1  -4   7   3  -3  -6  -4   1  -7 -10   7   8   5 3  -2)
 (  4   3   4  -9   6  -5   8 -10  -4  -8   9  -8  -7   6   9   3   9   2 5   8)
 (  0   3   7  -5   4  -6   4   3  -1   2   2  -9  -3  -9  -8  -6   9   5 -7  -6)
 ( -2  -8  -2  -5   6  -2  -1   7 -10   6   2   6  -3  -6  -2   8   1  -2 9   3)
 (  2 -10  -7  -1   7   6  -2  -8   1  -7   8  -5 -10  -1  -3   3   9   3 -8   0)
 ( -4   7  -9  -6   3   6   5   2   4  -5   0   9  -7  -9  -4   2   7  -4 -3  -3)
 (  2   2   0   7   3   0  -7   7  -6   1   0   0  -6   3  -2  -4  -6  -4 -2  -7)
 (  1  -9   7  -1  -2   0   7  -7  -8   8  -5  -2  -3   0   9  -8  -9   0 -3   9)
 ( -2  -3  -1  -5 -10  -7   2   8   6  -2   5   5  -1  -8   2  -5   2  -3 -6   5)
 ( -3  -6  -4  -9   5   0   3   7   3  -5  -5  -6   1  -3   9  -5  -8   5 9   7)
 ( -2  -4   4  -6   7   7 -10   3  -1   1  -9   7   8   3   0 -10   4   4 9   1)
 (  2  -3  -7   7   0   8  -5  -9 -10  -9   8  -3   4   4   4   5   2   8 -2   7)))


 (setf B20 '(( -4  -6  -2  -6   1  -3   9  -5   7   1   2  -4   0  -1  -4   7  -3   1 9  -8)
 (  7  -8  -4  -6   7   9  -9  -3   1   3  -2  -7   1  -7   7   4   4  -9 -2   2)
 (  5   8  -4  -3   6   2   8 -10  -2  -2   3  -2   3  -5 -10  -6   4   0 4  -7)
 ( -7   9   9 -10   6  -5  -5  -7   0   5   0   1  -7  -9  -2  -2 -10   8 -8   1)
 ( -4  -5  -6   6  -5   8   5   7   2   3  -1   5  -2  -7  -9  -5  -8   4 8   1)
 (  7   1   0   2   3 -10   0   2 -10  -8   6   3   1  -8 -10   5   0 -10 8   1)
 (  8   1   8   3   6 -10   7   9   8  -2  -6  -7  -9   3   3   2  -1  -9 9   0)
 ( -4   7  -1   6   4   0  -9   3   6   3  -4  -3  -5  -4 -10  -3   6  -5 -3  -8)
 (  8   0  -2  -6  -1  -1  -4   1 -10  -2   7  -8  -4   8   2   4   4   6 -2  -1)
 ( -6  -6 -10  -9   5  -6   3  -7   4   0  -2  -4  -9   3  -6   8  -4  -2 1  -6)
 (  5  -8   3   2  -9  -8   8   6   5 -10   7   1   8   6   6  -1   3  -5 1   1)
 (  4  -1   5   5  -5   9   3   7   4   6   2  -2   4   3  -3  -6   0   3 9  -7)
 (  2   5  -4   8  -3  -6  -5  -1  -1  -9   0  -6  -5   3   8   5   4   2 -10  -6)
 (  5   3   4   0   7   8  -2   3   9   8   5   1  -2  -4   9  -1  -3   9 7  -4)
 ( -4   0  -7  -1  -3  -1  -1  -3   7  -4   3   4  -4   2  -9   6  -4  -7 -1  -3)
 (  5 -10   1   2   0   8   4  -4   3  -1   4   7  -6   7 -10  -8   7  -3 -3  -9)
 (  1 -10   2  -4 -10   7  -6   9  -6  -4  -7  -3   6   1  -5   1  -3   0 3  -9)
 ( -7  -2  -9  -5   2  -9  -8  -8  -7 -10 -10  -6  -7   0   6  -9   1   1 3   9)
 ( -4  -5   5   9  -3  -9  -3  -4  -5   2  -3   4   4   4   4   7  -7 -10 -2  -1)
 (  2   5  -1  -1  -9  -5   7  -1  -2   8   0   9  -9  -5   2  -4  -2   8 7   4)))


(load "/Users/m4ks0n/.sbclrc")
(ql:quickload "fiveam")


(fiveam:def-suite positives)
(fiveam:def-suite negatives)

(fiveam:in-suite positives)

(fiveam:test test_1_check_Q_ort
  (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-q A C) (transpose_matrix (qz-decomposition-q A C))) (eye (length A)) 1e-3))
)

(fiveam:test test_1_check_Z_ort
  (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-z A C) (transpose_matrix (qz-decomposition-z A C))) (eye (length A)) 1e-3))
)

(fiveam:test test_1_check_S
    (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A C) (qz-decomposition-s A C)) (transpose_matrix (qz-decomposition-z A C))) A 1e-3))
)

(fiveam:test test_1_check_T
    (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A C) (qz-decomposition-t A C)) (transpose_matrix (qz-decomposition-z A C))) C 1e-3))
)

(fiveam:test test_2_check_Q_ort
  (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-q A5 B5) (transpose_matrix (qz-decomposition-q A5 B5))) (eye (length B5)) 1e-3))
)

(fiveam:test test_2_check_Z_ort
  (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-z A5 B5) (transpose_matrix (qz-decomposition-z A5 B5))) (eye (length A5)) 1e-3))
)

(fiveam:test test_2_check_S
    (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A5 B5) (qz-decomposition-s A5 B5)) (transpose_matrix (qz-decomposition-z A5 B5))) A5 1e-3))
)

(fiveam:test test_2_check_T
    (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A5 B5) (qz-decomposition-t A5 B5)) (transpose_matrix (qz-decomposition-z A5 B5))) B5 1e-3))
)

(fiveam:test test_3_check_Q_ort
  (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-q A_float B_float) (transpose_matrix (qz-decomposition-q A_float B_float))) (eye (length A_float)) 1e-3))
)

(fiveam:test test_3_check_Z_ort
  (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-z A_float B_float) (transpose_matrix (qz-decomposition-z A_float B_float))) (eye (length A_float)) 1e-3))
)

(fiveam:test test_3_check_S
    (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A_float B_float) (qz-decomposition-s A_float B_float)) (transpose_matrix (qz-decomposition-z A_float B_float))) A_float 1e-3))
)

(fiveam:test test_3_check_T
    (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A_float B_float) (qz-decomposition-t A_float B_float)) (transpose_matrix (qz-decomposition-z A_float B_float))) B_float 1e-3))
)

; (fiveam:test test_4_check_Q_ort
;   (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-q A_big A_big) (transpose_matrix (qz-decomposition-q A_big A_big))) (eye (length A_big)) 1e-3))
; )

; (fiveam:test test_4_check_Z_ort
;   (fiveam:is (matrix_compare (mul_matrix (qz-decomposition-z A_big A_big) (transpose_matrix (qz-decomposition-z A_big A_big))) (eye (length A_big)) 1e-3))
; )

; (fiveam:test test_4_check_S
;     (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A_big A_big) (qz-decomposition-s A_big A_big)) (transpose_matrix (qz-decomposition-z A_big A_big))) A_big 1e-3))
; )

; (fiveam:test test_4_check_T
;     (fiveam:is (matrix_compare (mul_matrix (mul_matrix (qz-decomposition-q A_big A_big) (qz-decomposition-t A_big A_big)) (transpose_matrix (qz-decomposition-z A_big A_big))) A_big 1e-3))
; )

(fiveam:run!)

(fiveam:in-suite negatives)

(fiveam:test test_5_check_A_square_q
  (fiveam:is (equal (qz-decomposition-q A_not_square B) nil))
)

(fiveam:test test_5_check_B_square_q
  (fiveam:is (equal (qz-decomposition-q A B_not_square) nil))
)

(fiveam:test test_5_check_A_square_z
  (fiveam:is (equal (qz-decomposition-z A_not_square B) nil))
)

(fiveam:test test_5_check_B_square_s
  (fiveam:is (equal (qz-decomposition-s A B_not_square) nil))
)

(fiveam:test test_5_check_B_square_t
  (fiveam:is (equal (qz-decomposition-t A B_not_square) nil))
)

(fiveam:test test_6_check_dif_size_q
  (fiveam:is (equal (qz-decomposition-q A B5) nil))
)

(fiveam:test test_6_check_dif_size_z
  (fiveam:is (equal (qz-decomposition-z A5 B) nil))
)

(fiveam:test test_5_check_dif_size_s
  (fiveam:is (equal (qz-decomposition-s A5 B) nil))
)

(fiveam:test test_6_check_dif_size_t
  (fiveam:is (equal (qz-decomposition-t A5 B) nil))
)


(fiveam:run!)
