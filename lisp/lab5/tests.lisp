(ql:quickload "fiveam")


;;; log
(defun my_log_help (x accuracy
		    &optional (result 1) (current 0) (cur_sqrt (sqrt x)))
  (cond ((= accuracy current) result)
        (T (my_log_help x accuracy
			(* result (/ 2 (inc cur_sqrt)))
			(inc current)
			(sqrt cur_sqrt)))))

(defun my_log (x &optional (accuracy 25))
  (* (my_log_help x accuracy) (- x 1)))

(fiveam:test log_test_1
  (fiveam:is (< (abs (- (log 1) (my_log 1))) 1e-5)))

(fiveam:run!)

