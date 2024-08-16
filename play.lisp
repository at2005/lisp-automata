
(load "automata.lisp")
(load "render.lisp")

(defun next-board-state (board-state)
  (loop for i from 0 below (length board-state)
        collect
        (loop for j from 0 below (length  board-state)
              collect (game-of-life-cell board-state i j))))


(defun play-automata (board-state timesteps)
	(cond 
		((eql timesteps 0) ())
		(t 
			(progn (render-board-state board-state timesteps)
			(play-automata (next-board-state board-state) (- timesteps 1))))
	)
)

;(game-of-life-cell (fill-board 12 0) 0 0)

;(next-board-state (fill-board 12 0))
(play-automata (fill-board 12 0) 4)
