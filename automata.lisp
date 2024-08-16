
(defun idx-2d (matrix i j)
	; check if row and col oob
	; also literal edge case, check if out of bounds
	(cond 
		((and (>= i 0) (>= j 0) (< i (length matrix)) (< j (length (nth i matrix)))) (nth j (nth i matrix)))
		(t 0)
	)
)

; we need to fetch all neighbours for a particular cell, and then we can make decisions from that

(defun get-neighbour (board-state i j n)
	; neighbour 1 -> i+1, j
	; neighbour 2 -> i, j+1
	; neighbour 3 -> i-1, j
	; neighbour 4 -> i, j-1
	; others are diagonal
	(cond 
		((eql n 0) (idx-2d board-state (+ i 1) j))
		((eql n 1) (idx-2d board-state i (+ j 1)))
		((eql n 2) (idx-2d board-state (- i 1) j))
		((eql n 3) (idx-2d board-state i (- j 1)))
		((eql n 4) (idx-2d board-state (+ i 1) (+ j 1)))
		((eql n 5) (idx-2d board-state (- i 1) (+ j 1)))
		((eql n 6) (idx-2d board-state (+ i 1) (- j 1)))
		((eql n 7) (idx-2d board-state (- i 1) (- j 1)))
		(t nil)
	)
)

(defun get-neighbours (board-state i j &optional (n 0))
	(cond 
		((eql n 8) ())
		(t (cons (get-neighbour board-state i j n)  (get-neighbours board-state i j (+ n 1))))
	)
)


(defun count-live (nbs &optional (i 0))
	(cond 
		((eql i (length nbs)) 0)
		(t (+ (nth i nbs) (count-live nbs (+ i 1))))
	)
)

; policy for an individual cell
(defun game-of-life-cell (board-state i j)
	(let ((nbs (get-neighbours board-state i j)) (current-state (idx-2d board-state i j)))
		(let ((num-live (count-live nbs)))
			(cond 
				; if cell live
				((eql current-state 1) 
					(cond
						; any live cell dies of underpopulation or overpopulation
						((or (< num-live 2) (> num-live 3)) 0)
						; else lives
						(t 1)
					)
				)
				; if cell dead
				(t (cond ((eql num-live 3) 1) (t 0)))

			)
		)
	)
)


;(load "render.lisp")
;;; unit tests
;(write (count-live '(0 0 1 1)))
;(write (get-neighbour (fill-board 12 0) 0 0  7))
;(write (get-neighbours (fill-board 13 0) 0 3))
;(write (game-of-life-cell (fill-board 12 1) 4 4))



