(defvar first_player_list)
(defvar second_player_list)

(defun bones_throw ()
		
			(print "Enter first bone: ")
			(setq bone1 (read))
			(print "Enter second bone: ")
			(setq bone2 (read))
			(setq ret (list bone1 bone2))
			ret
		)

(defun check_absolute_win(list)
    (
        or (= (+ (first list) (last list) 7))
        (= (+ (first list) (last list) 11))
    ))

(defun check_rerun(list)
    (
        or (= (first list) (last list) 1)
        (= (first list) (last list) 6)
    ))


(defun check_not_absolute_win()
    (
        cond (
			    (> (+ (first first_player_list) (last first_player_list))
				    (+ (first second_player_list) (last second_player_list)))
			    (print "First player wins"))
			(
				(< (+ (first first_player_list) (last first_player_list))
					(+ (first second_player_list) (last second_player_list)))
				(print "Second player wins"))
			(T (print "Draw in the game"))
    ))

(defun second_palyer_turn ()
    (
        (print "second player throws bones: ")
        (setq second_palyer_list (bones_throw))
        (print first_player_list)

        (cond ((check_absolute_win first_player_list) (print "Second player wins"))
        ((check_rerun(first_player_list)) (second_player_turn))
        (t check_not_absolute_win))
    ))

(defun first_player_turn ()
    (
        (print "first player throws bones: ")
        (setq first_palyer_list (bones_throw))
        (print first_player_list)

        (cond ((check_absolute_win first_player_list) (print "First player wins"))
        ((check_rerun(first_player_list)) (first_player_turn))
        (t (second_player_turn)))
    ))

(first_player_turn)