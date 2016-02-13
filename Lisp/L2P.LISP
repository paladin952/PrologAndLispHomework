;12. Return the level of a tree of type (2) having the maximal number of nodes, 
;    as well the list of all these nodes. The level of the root element is 0.


;'(A (B) (C (D) (E)))
;'(a (b (c) (d)) (e (f) (g))))

;return the tree's depth
(defun adancime(l)
  (cond 
	((null l) 0)
	(t (+ 1 (max (adancime (cadr l)) (adancime (cddr l)))))
  )
)

;return a list of nodes at level
(defun lista_nivel(l k)
  (cond
	((= k 1) (list (car l)))
	(t (append (lista_nivel (cadr l) (- k 1)) (lista_nivel (caddr l) (- k 1))))
  )
)()

;goes through each level an calculates the max number of nodes
(defun all(l h)
  (cond
	((= h 0) nil)
	((> (length (lista_nivel l h)) 
	    (max 
		(length (lista_nivel (cadr l) (- h 1))) 
		(length (lista_nivel (caddr l) (- h 1)))
	    )
	 )
	 (lista_nivel l h)
	)		
	((> (length (lista_nivel (cadr l) (- h 1))) 
	    (length (lista_nivel (caddr l) (- h 1)))
	 )
	 (lista_nivel (cadr l) (- h 1))
	)
	(t (lista_nivel (caddr l) (- h 1))) 
  )
)

;return the level with most nodes
(defun nr_max(l h)
 (cond
	((= h 0) nil)
	 (t(max 
	(length (lista_nivel (cadr l) (- h 1))) 
	(length (lista_nivel (caddr l) (- h 1)))
    ))
  )
)


(defun start(l)
	(all l (adancime l))
)

(defun start_max(l)
		(nr_max l (adancime l))
	)
