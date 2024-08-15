

(defun fill-row (n val &optional (i 1))
	(cond ((eql i n) (list val))
	(t (cons val (fill-row n val (+ i 1)))))
)

(defun fill-board (n val)
	(fill-row n (fill-row n val))
)

; given a gameboard state, render board as ppm
(defun render-board-state (board-state t-val)
	(with-open-file (stream "board-state" :direction :output :if-exists :supersede)
		(dotimes (i (length board-state))
			(dotimes (j (length (nth i board-state)))
				(format stream "~d" (nth j (nth i board-state)))
			)
		)
	)
)

;(render-board-state (fill-board 12 2) 1)
(render-board-state (fill-board 12 0) 5)



