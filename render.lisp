(defun fill-row (n val &optional (i 1))
	(cond ((eql i n) (list val))
	(t (append (copy-tree (list val)) (copy-tree (fill-row n val (+ i 1))))))
)

(defun fill-board (n val) (fill-row n (fill-row n val)))

; given a gameboard state, render board as ppm
(defun render-board-state (board-state t-val)
	(let ((filename (format nil "renders/board_state_~d.ppm" t-val)))
		(with-open-file (stream filename :direction :output :if-exists :supersede)
			(format stream "P1~%~d ~d~%" (length board-state) (length board-state))
			(dotimes (i (length board-state))
				(dotimes (j (length (nth i board-state)))
					(format stream "~d " (nth j (nth i board-state)))
				)

				(format stream "~%")
			)
		)
	)
)

;(render-board-state (fill-board 12 2) 1)
;(render-board-state (fill-board 12 0) 5)

