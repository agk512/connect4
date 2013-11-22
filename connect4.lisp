(defun make-board ()
    (setf board (make-array '(6 7)))
)

(defun print-board ()
    (print board)
)

(defun player-1-move (c)
    (cond 
	((eq (aref board 5 c) NIL) (setf (aref board 5 c) "X"))
	((and (eq (stringp (aref board 5 c)) t) (eq (aref board 4 c) NIL)) (setf (aref board 4 c) "X"))
        ((and (eq (stringp (aref board 4 c)) t) (eq (aref board 3 c) NIL)) (setf (aref board 3 c) "X"))
        ((and (eq (stringp (aref board 3 c)) t) (eq (aref board 2 c) NIL)) (setf (aref board 2 c) "X"))
        ((and (eq (stringp (aref board 2 c)) t) (eq (aref board 1 c) NIL)) (setf (aref board 1 c) "X"))
        ((and (eq (stringp (aref board 1 c)) t) (eq (aref board 0 c) NIL)) (setf (aref board 0 c) "X")) 
    )
)

(defun player-2-move (c)
    (cond
        ((eq (aref board 5 c) NIL) (setf (aref board 5 c) "O"))
        ((and (eq (stringp (aref board 5 c)) t) (eq (aref board 4 c) NIL)) (setf (aref board 4 c) "O"))
        ((and (eq (stringp (aref board 4 c)) t) (eq (aref board 3 c) NIL)) (setf (aref board 3 c) "O"))
        ((and (eq (stringp (aref board 3 c)) t) (eq (aref board 2 c) NIL)) (setf (aref board 2 c) "O"))
        ((and (eq (stringp (aref board 2 c)) t) (eq (aref board 1 c) NIL)) (setf (aref board 1 c) "O"))
        ((and (eq (stringp (aref board 1 c)) t) (eq (aref board 0 c) NIL)) (setf (aref board 0 c) "O")) 
    )
)

;; Returns T if the move is a number in the interval
;; [0, 6], NIL otherwise.
(defun is-valid-move (move)
	(and (numberp move)
		(>= move 0)
		(<= move 6)))

;; Returns T if that column is not filled up or
;; NIL otherwise.
(defun can-play-move (move)
	(not (aref board 0 move)))

(defun check-for-winner ()

)

;; Sanitizes the player's input to make sure they
;; play a move that is a number in the interval [0, 6]
;; and the column they wish to play in is not full.
(defun human-select ()
	(let ((move -1) (check-move 0))
		(loop while (= move -1) do
			(setf check-move (read))
			(cond
				((not (is-valid-move check-move)) 
					(format t "Invalid move; expected a number in the interval [0, 6], got: ~a.~%Try again: " check-move))
				((not (can-play-move check-move))
					(format t "Invalid move; you played in column ~a but it is full.~%Try again: " check-move))
				(t
					(setf move check-move))))
		(return-from human-select move)))

;; The easy AI selects a move from the interval [0, 6]
;; at random and returns that move.
(defun easy-ai-select ()
	(let ((move -1) (rand-move 0))
		(loop while (= move -1) do
			(setf rand-move (random 7))
			(if (can-play-move rand-move)
				(setf move rand-move)))
		(return-from easy-ai-select move)))

(make-board)
(setf total-moves 0)
(loop while (< total-moves 42) do
	(print board)
	(format t "~%")
	(format t "Player 1 Input: ")
	(player-1-move (human-select))
	(incf total-moves)
	(print board)
	(format t "~%")
	(format t "Player 2 Input: ")
	(player-2-move (easy-ai-select))
	(incf total-moves))
	;;;(screen:clear-window (screen:make-window))