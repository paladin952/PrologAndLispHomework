

; a. Write a function to get from a given list the list of all atoms, on any 
;      level, but on the same order. Example:
;      (((A B) C) (D E)) ==> (A B C D E)
(defun conv(l)
	(cond
			((null l) nil)
			((atom l) (list l))
			((listp l) (apply #'append(mapcar #'conv l)))
		)
	)
;  d. Write a function to return the sum of two vectors.
(defun sum_vectors(v1 v2)
	(cond
		((null v1)v2)
		((null v2)v1)
		(t  (append nil (list(+ (car v1) (car v2))) (sum_vectors(cdr v1) (cdr v2) )))
		)
	)

 ; a. Write a function to determine the successor of a number represented digit by digit 
 ;     as a list. Example: (1 9 3 5 9 9) --> (1 9 3 6 0 0)


(defun reversee(l)
	(cond
		((null l) nil)
		((listp (car l)) (append (reversee (cdr l)) (list (reversee (car l)))))
		(t (append (reversee (cdr l)) (list (car l))))
	)
)

(defun addOneMain(l)
	(reversee (addOne (reversee l)))
)

(defun addOne(l)
	(cond
		((null l) (list 1))
		((equal (car l) 9) (append nil (list 0) (addOne (cdr l))))
		(t (append nil (list (+ 1 (car l))) (cdr l)))
	)
)


(defun myAdd(l)
	(cond
		((null l) (list 1) )
		((equal (car l)9) (append nil (list 0) (myAdd(cdr l))))
		(t (append nil (list (+ 1 (car l))) (cdr l)))
		)
	)

(defun myAddMain(l)
	(reverse (myAdd(reverse l )))	
	)