
;a. Write a function to test if a linear list of numbers has a "mountain" aspect
;      (a list has a "mountain" aspect if the items increase to a certain point and then decreases. 
;      Eg. 10 18 29 17 11 10).

(defun mountain(l f)
	(cond
		( (and (null (cadr l)) (eq f 1)) t)
		( (and (null (cadr l)) (eq f 0)) nil)
		( (and (< (car l) (cadr l)) (eq f 1)) nil)
		( (and (> (car l) (cadr l)) (eq f 0)) (mountain(cdr l) 1))
		( (and (< (car l) (cadr l)) (eq f 0)) (mountain(cdr l) 0))
		( (and (> (car l) (cadr l)) (eq f 1)) (mountain(cdr l) 1))
	)
)

(defun start(l)
	(cond
		((null (cadr l)) nil)
		( (> (car l)(cadr l)) nil)
		(t(mountain (cdr l) 0))
	)
	
)

;b. Remove all occurrences of a maximum numerical element from a nonlinear list.
(defun maxim(l m)
  (cond
   ((null l)m)
   ((not(numberp (car l))) (maxim (cdr l) m))
   ((and (atom (Car l)) (>= (car l) m))(maxim (cdr l) (car l)))
   ((and (atom (car l)) (< (car l) m)) (maxim (cdr l) m))
   (t (maxim (cdr l) (maxim (car l) m)))
   )
)

(defun elim (el l)
  (cond
   ((null l) nil)
   ((not(numberp (car l))) (append (list (car l)) (elim el (cdr l))))
   ((and (atom (Car l)) (eq (car l) el))(elim el (cdr l)))
   ((and (atom (car l)) (not (eq (car l) el)))(append (list (car l)) (elim el (cdr l))))
   (t (append (list(elim el (car l))) (elim el (cdr l))))
   )
)

(defun del(l)
  (elim (maxim l 0) l))

;d. Write a function which returns the product of numerical even atoms from a list, to any level.
(defun evenProd(l m)
  (cond
   ((null l)m)
   ( (and (atom (car l)) (eq(mod (car l) 2) 0))  (evenProd (cdr l) (* m (car l))))
   ((listp (car l))(evenProd (cdr l) (evenProd (car l) m)))
   (t (evenProd (cdr l) m))
   )
)

(defun 13c(l)
  (evenProd l 1))

;c. Determine the least common multiple of the numerical values ​​of a nonlinear list
(defun mygcd (a b)
    (cond
     ((< a b) (mygcd a (- b a)))
     ((> a b) (mygcd (- a b) b))
     (t a)))

(defun mylcm(a b)
	(/(abs(* a b))(mygcd a b))

)

(defun lcmlist (l m)
	(cond
		((null l)m)
		((atom (car l)) (lcmlist (cdr l) ( mylcm m (car l))))
   		((listp (car l))(lcmlist (cdr l) (lcmlist (car l) m)))
   		(t (lcmlist (cdr l) m))
	)
)

(defun startlcm (l)
	 (lcmlist l 1)
)