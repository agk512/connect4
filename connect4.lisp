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

(defun check-for-winner ()

)

(make-board)
(loop
(print board)
(format t "~%")
(format t "Player 1 Input: ~%")
(player-1-move (read))
(print board)
(format t "~%")
(format t "Player 2 Input: ~%")
(player-2-move (read))
;;;(screen:clear-window (screen:make-window))
)
