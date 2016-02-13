;Return the list of nodes on the k-th level of a tree of type
;'(a (b (c) (d)) (e (f) (g))))
(defun list_at_level(l k)
	(cond
		((= k 0) (list(car l)))
		(t(append (list_at_level (cadr l) (- k 1)) (list_at_level (caddr l) (- k 1)) ))
	)
)	

;11. Return the number of levels of a tree of type (2).
(defun levels(l)
	(cond
		((null l)0)
		(t(+ 1 (max(levels (cadr l))(levels (caddr l)))))
	)
)