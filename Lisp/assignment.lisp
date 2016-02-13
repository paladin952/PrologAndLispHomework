;; L1.
;; a. Write a function to get the list of all atoms from a given list, on any level, but in the same order.
;; Example: (((A B) C) (D E)) => (A B C D E)

(DEFUN GetAtoms (my_list)
	(COND
        ((NULL my_list) NIL)
        ((ATOM (CAR my_list)) (CONS (CAR my_list) (getAtoms (CDR my_list))))
        ((LISTP (CAR my_list)) (APPEND (getAtoms (CAR my_list)) (getAtoms (CDR my_list))))
    )
)

;; b. Write a function which, having a list given as parameter, inverts only continuous sequences of atoms.
;; Example: (a b c (d (e f) g h i)) ==> (c b a (d (f e) i h g))

(DEFUN BuildInverse (my_list)
    (COND
        ((NULL my_list) 
            NIL
        )
        ((NULL (CDR my_list))
            (list (CAR my_list))
        )
        ((NOT (NULL my_list)) 
            (APPEND (buildInverse (CDR my_list)) (list (CAR my_list)))
        )
    )
)

(DEFUN InvertContiguousAux (my_list crt_list res)
    (COND
        ((NULL my_list)
            (APPEND res (BuildInverse crt_list))
        )
        ((ATOM (CAR my_list))
            (InvertContiguousAux (CDR my_list) (APPEND crt_list (list (CAR my_list))) res)
        )
        ((LISTP (CAR my_list))
            ; if current element of my_list is a list, append the current list, and the result of InvertContiguousAux(current_element) to res, then continue parsing my_list.
            (InvertContiguousAux (CDR my_list) (list) (APPEND res (BuildInverse crt_list) (list(InvertContiguousAux (CAR my_list) (list) (list)))))
        )    
    )
)

(DEFUN InvertContiguous (my_list)
    (InvertContiguousAux my_list (list) (list))
)

;; c. Write a function which returns the maximum value of the numerical atoms of a list, at a superficial level.

(DEFUN MaxAux (my_list crt)
    (COND
        ((NULL my_list)
            crt
        )
        ((AND (INTEGERP (CAR my_list)) (> (CAR my_list) crt))
            (MaxAux (CDR my_list) (CAR my_list))
        )
        (
            (MaxAux (CDR my_list) crt)
        )
    )
)

(DEFUN GetMax (my_list)
    (MaxAux (CDR my_list) (CAR my_list))
)

;; d. Sum of two vectors.

(DEFUN GetVectDimension (vect)
    (COND
        ((NULL vect)
            0
        )
        ((NOT (NULL vect))
            (+ (GetVectDimension (CDR vect)) 1)
        )
    )
)

(DEFUN VectorSum (vect1 vect2 res)
    (COND
        ((OR (NULL vect1) (NULL vect2) (NOT (EQUAL (GetVectDimension vect1) (GetVectDimension vect2))))
            res   
        )
        ((AND (NOT (NULL vect1)) (NOT (NULL vect2)) (EQUAL (GetVectDimension vect1) (GetVectDimension vect2)))
            (VectorSum (CDR vect1) (CDR vect2) (APPEND (list (+ (CAR vect1) (CAR vect2))) res))
        )
    )
)