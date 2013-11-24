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
(defun can-play-move (board-array move)
	(not (aref board-array 0 move)))
	
;; Take an original game board array and copies
;; its contents to a new array
(defun copy-board (original-board)
	(let ((new-board (make-array '(6 7))))
		(loop as i from 0 to 5 do
			(loop as j from 0 to 6 do
				(setf (aref new-board i j) (aref original-board i j))))
		(return-from copy-board new-board)))

;; Plays a move in the specified column for
;; the specified board array.
;; Assumes that there has already been a check to
;; can-play-move and it returned T.
(defun play-move (board-array column marker)
	(let ((found nil) (row 5))
		(loop while (not found) do
			(if (not (aref board-array row column))
				; The piece can be played in the row
				(progn
					(setf found t)
					(setf (aref board-array row column) marker))
				; The piece cannot be played in the row
				(decf row)))))

;; Node class
(defclass node ()
	((first-child :initarg :first-child
				  :accessor node-first-child)
	 (next-sibling :initarg :next-sibling
				   :accessor node-next-sibling)
	 (data :initarg :data
		   :reader node-data))
	(:default-initargs
		:first-child nil
		:next-sibling nil))
		
;; Board-state class		
(defclass board-state ()
	((board :initarg :board
			:accessor board-state-array)
	 (max-in-a-rows :initarg :max-in-a-rows
					:reader board-max-in-a-rows)
	 (min-in-a-rows :initarg :min-in-a-rows
					:reader board-min-in-a-rows))
	(:default-initargs
		:max-in-a-rows '(0 0 0 0)
		:min-in-a-rows '(0 0 0 0)))

;; Creates a new board-state object from the game's board
(defmethod board-state-new ()
	(make-instance 'board-state
		:board (copy-board board)))
		
;; Creates a new board-state object from an existing board-
;; state
(defmethod board-state-copy ((previous board-state))
	(make-instance 'board-state
		:board (copy-board (board-state-array previous))))

(defun iterate-through-list (piece-list marker start params-list step)
	(let ((i start) (stop-looking nil) (num-checked 0) (in-a-row-count 1) (last-index 2))
		(loop while (and (< num-checked 4) (not stop-looking)) do
			(if (and (< i (list-length piece-list)) (>= i 0))
				(cond
					((equal (nth i piece-list) marker)
						(prog
							(incf (nth last-index params-list))
							(if (>= (nth last-index params-list) (list-length piece-list))
								(setf (nth last-index params-list) (- (list-length piece-list) 1)))
							(incf (nth num-checked params-list))
							(incf (nth in-a-row-count params-list))))
					((equal (nth i piece-list) "-")
						(incf (nth num-checked params-list)))
					(t
						(setf stop-looking t))
				(setf stop-looking t))
			(setf i (+ i step))))))

;; Starts counting a pontential in-a-row
(defun start-piece-count (piece-list in-a-rows-list marker start)
	(let ((params-list (list 1 1 (+ start 1))) (num-checked 0) (in-a-row-count 1) (last-index 2))
		(iterate-through-list piece-list marker (+ start 1) params-list 1)
		(if (< (nth num-checked params-list) 4)
			(let ((dummy-list (list (first params-list) (second params-list) 0)))
				(iterate-through-list piece-list marker (- start 1) dummy-list -1)
				(if (= (nth num-checked dummy-list) 4)
					(incf (nth in-a-rows-list (- (nth in-a-row-count dummy-list) 1)))))
			(let ((dummy-list (list (first params-list) (second params-list) 0)))
				(incf (nth in-a-rows-list (- (nth in-a-row-count dummy-list) 1)))
				(setf (nth num-checked dummy-list) 1)
				(setf (nth in-a-row-count dummy-list) 1)
				(iterate-through-list piece-list marker (- start 1) dummy-list -1)
				(if (= (nth num-checked dummy-list) 4)
					(incf (nth in-a-rows-list (- (nth in-a-row-count dummy-list) 1))))))
		(return-from start-piece-count (third params-list))))
		
;; Counts the number of in-a-rows from the piece-list and adds
;; the values to the list
(defun count-in-a-rows (piece-list in-a-rows-list marker)
	(let ((i 0))
		(loop while (< i (list-length piece-list)) do
			(if (equal (nth i piece) marker)
				(setf i (start-piece-count piece-list in-a-rows-list marker start))))))
		
;; Gets a character that represents a piece on the board.
;; Returns the following character:
;;		'-' if there is no piece at the given row and column
;;		'X' or 'O' if there exists a piece
;;		'.' if there is no piece in the row below
(defun get-piece-char (board-array r c)
	(if (or (= r 5) (aref board-array (+ r 1) c))
		(if (not (aref board-array r c))
			(return-from get-piece-char (list (aref board-array r c)))
			(return-from get-piece-char (list "-")))
		(return-from get-piece-char (list "."))))
		
(defmethod board-state-count ((b board-state))
	(let ((board-array (board-state-array b)))
		(loop as c from 0 to 6 do
			(let ((v-list nil))
				(loop as r from 5 downto 0 do
					(append v-list (get-piece-char board-array r c)
				(count-in-a-rows v-list (board-max-in-a-rows b) "X")
				(count-in-a-rows v-list (board-min-in-a-rows b) "O")))))
		(loop as r from 5 downto 0 do
			(let ((h-list nil))
				(loop as c from 0 to 6 do
					(append h-list (get-piece-char board-array r c))
				(count-in-a-rows h-list (board-max-in-a-rows b) "X")
				(count-in-a-rows h-list (board-min-in-a-rows b) "O"))))
		(let ((up-right-indices '((5 0) (5 1) (5 2) (5 3) (4 0) (3 0))))
			(loop for pair in up-right-indices do
				(let ((d-list nil))
					(loop as r from (first pair) downto 0 do
						(loop as c from (second pair) to 6 do
							(append d-list (get-piece-char board-array r c))))
					(count-in-a-rows d-list (board-max-in-a-rows b) "X")
					(count-in-a-rows d-list (board-min-in-a-rows b) "O")))
		(let ((up-left-indices '((5 6) (5 5) (5 4) (5 3) (4 6) (3 6))))
			(loop for pair in up-left-indices do
				(let ((d-list nil))
					(loop as r from (first pair) downto 0 do
						(loop as c from (second pair) downto 0 do
							(append d-list (get-piece-char board-array r c))))
					(count-in-a-rows d-list (board-max-in-a-rows b) "X")
					(count-in-a-rows d-list (board-min-in-a-rows b) "O")))))))

;; Add child method adds a new node to the given node
(defmethod node-add-child ((target node) (child node))
	(if (equal (node-first-child target) nil)
		(setf (node-first-child target) child)
		(progn
			(setf c-node target)
			(loop while (node-next-sibling c-node) do
				(setf c-node (node-next-sibling c-node)))
			(setf (node-next-sibling c-node) child))))

;; Gets the heuristic value from a node based on these
;; weights:
;;		1-in-a-row		+- 1
;;		2-in-a-row		+- 16
;;		3-in-a-row		+- 128
;;		4-in-a-row		+- infinity	(64,000)
(defmethod node-heuristic ((target node))
	(let ((heuristic 0) (max-in-a-rows (board-max-in-a-rows (node-data target))) (min-in-a-rows (board-min-in-a-rows (node-data target))))
		(if (> (fourth min-in-a-rows) 0)
			(setf heuristic -64000)
			(progn
				(setf heuristic (+ heuristic (* -128 (third min-in-a-rows))))
				(setf heuristic (+ heuristic (* -16 (second min-in-a-rows))))
				(setf heuristic (+ heuristic (* -1 (first min-in-a-rows))))))
		(if (> (fourth max-in-a-rows) 0)
			(setf heuristic 64000)
			(if (not (= heuristic -64000))
				(progn
					(setf heuristic (+ heuristic (* 128 (third min-in-a-rows))))
					(setf heuristic (+ heuristic (* 16 (second min-in-a-rows))))
					(setf heuristic (+ heuristic (* 1 (first min-in-a-rows)))))))
		(return-from node-heuristic heuristic)))
	
;; Generates a game tree for the specified depth given
;; every possible move.	
(defun generate-tree (p-node depth marker)
	(if (> depth 0)
		(let ((move-try 0))
			(loop while (< move-try 7) do
				(if (can-play-move (board-state-array (node-data p-node)) move-try)
					(let ((new-board-state (board-state-copy (node-data p-node))) (next-marker marker))
						(play-move (board-state-array new-board-state) move-try marker)
						(board-state-count new-board-state)
						(setf new-node (make-instance 'node
							:data new-board-state))
						(node-add-child p-node new-node)
						(if (equal marker "X")
							(setf next-marker "O")
							(setf next-marker "X"))
						(if (or (= (fourth (board-max-in-a-rows new-board-state)) 0) (= (fourth (board-min-in-a-rows new-board-state)) 0))
							(generate-tree new-node (- depth 1) next-marker)))))
				(incf move-try))))
							
;; Searches a game tree using the minimax algorithm and
;; alpha-beta pruning to avoid searching unnecessary
;; node branches.
;; Algorithm from
;;		http://www.wikipedia.org/wiki/Alpha-beta_pruning
(defun alphabeta (tree-node depth c-num is-max alpha beta)
	(if (= depth 0)
		(return-from alphabeta (list c-num (node-heuristic tree-node))))
	(if (equal is-max t)
		(let ((c-child (node-first-child tree-node)) (i 0) (beta-cut-off nil)) ; Current player is max player
			(loop while (and (not beta-cut-off) (node-next-sibling c-child)) do
				(setf alpha (max alpha (second (alphabeta c-child (- depth 1) i nil alpha beta))))
				(if (<= beta alpha)
					(setf beta-cut-off t)) ; Beta cut-off
				(setf c-child (node-next-sibling c-child))
				(incf i))
			(return-from alphabeta (list c-num alpha)))
		(let ((c-child (node-first-child tree-node)) (i 0) (alpha-cut-off nil)) ; Current player is min player
			(loop while (and (not alpha-cut-off) (node-next-sibling c-child)) do
				(setf beta (min beta (second (alphabeta c-child (- depth 1) i t alpha beta))))
				(if (<= beta alpha)
					(setf alpha-cut-off t)) ; Alpha cut-off
				(setf c-child (node-next-sibling c-child))
				(incf i))
			(return-from alphabeta (list c-num beta)))))

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
				((not (can-play-move board check-move))
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
			(if (can-play-move board rand-move)
				(setf move rand-move)))
		(return-from easy-ai-select move)))
		
(defun medium-ai-select ()
	(let ((root (make-instance 'node :data (board-state-new))))
		(generate-tree root 2 "X")
		(return-from medium-ai-select (first (alphabeta root 2 0 t -64000 64000)))))

(make-board)
(setf total-moves 0)
(loop while (< total-moves 42) do
	(print board)
	(format t "~%")
	(format t "Player 1 Input: ")
	(player-1-move (human-select))
	(incf total-moves)
	(player-2-move (medium-ai-select))
	(incf total-moves))
	;;;(screen:clear-window (screen:make-window))
