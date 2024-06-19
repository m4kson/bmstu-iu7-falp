
; (defun qz_t(A B &optional (max-iterations 100) (tol 1e-6) (i 0))
;     (if (>= i max-iterations)
;       B
;       (qz-decomposition-s (qz_iteration_a A B)
;                           (qz_iteration_b A B)
;                           max-iterations
;                           tol
;                           ( + i 1))))
; )