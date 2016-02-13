;1.
;  a. Write a function to get from a given list the list of all atoms, on any 
;     level, but on the same order. Example:
;     (((A B) C) (D E)) ==> (A B C D E)

(defun 1a(l)
  (cond
   ((null l)nil)
   ((atom l)(list l))
   ((atom (car l))(append (list(car l)) (1a (cdr l))))
   (t (append (1a (Car l)) (1a (cdr l))))
   )
)

 ; b. Write a function that, with a list given as parameter, inverts only continuous
 ;    sequences of atoms. Example:
 ;    (a b c (d (e f) g h i)) ==> (c b a (d (f e) i h g))

(defun 1b(l v)
  (cond
   ((null l) v)
   ((atom (car l))(1b (cdr l) (cons (car l) v)))
   (t (append v (list (1b (car l) '())) (1b (Cdr l) '())))
   )
)
(defun 1bRez(l)
  (1b l '()))


 ; c. Write a list to return the maximum value of the numerical atoms from a list,
 ;    at superficial level.

(defun 1c(l m)
  (cond
   ((null l)m)
   ((and (numberp (car l)) (> (car l) m)) (setq m (car l)) (1c (cdr l) m))
   ((listp (car l))(setq m (1c (car l) m)) (1c (cdr l) m))
   (t (1c (cdr l) m))
   )
)

 ; d. Write a function to return the sum of two vectors.
(Defun 1d(l1 l2)
  (cond
   ((and (null l1) (null l2))nil)
   ((and (numberp (car l1)) (numberp (car l2))) (append (list(+ (car l1) (car l2))) (1d (cdr l1) (cdr l2))))
   (t (append (car l2) (1d '() (cdr l2))))
   )
)
(defun 1dRez(l1 l2)
  (cond
   ((> (length l1) (length l2))(1d l2 l1))
   (t (1d l1 l2))
   )
)

;2.
;  a. Write a function to return the set of all pairs from a list given as parameter. 
;     Example: (a b c d) --> ((a b) (a c) (a d) (b c) (b d) (c d))
(defun 2a(elem l)
  (cond
   ((null l)nil)
   (t(cons (list elem (Car l)) (2a elem (cdr l))))
   )
)
(defun 2arez(l)
  (cond
   ((null l)nil)
   (t (append (2a (Car l) (cdr l)) (2arez (cdr l))))
   )
)

  ;c. Write a function to return the product of all the numerical atoms from a list,
  ;   at superficial level.

(defun 2c(l x)
  (cond
   ((null l)x)
   ((numberp (car l))(2c (cdr l) (* x (car l))))
   (t (2c (cdr l) x))
   )
)
(defun 2cRez(l)
  (2c l 1))

 ; d. Write a function to produce the list of pairs (atom n), where atom appears for
 ;    n times in the parameter list. Example:
 ;    (A B A B A C A) --> ((A 4) (B 2) (C 1)).
(defun countit(elem l)
  (cond
   ((null l)0)
   ((eq elem (car l))(+ 1 (countit elem (cdr l))))
   (t (countit elem (cdr l)))
   )
)
(defun isIn(elem l)
  (cond
   ((null l)nil)
   ((eq elem (car l))t)
   (t (isin elem (cdr l)))
   )
)
(defun 2d(l l1)
  (cond
   ((null l)nil)
   ((not(isin (car l) (cdr l)))(cons (list (car l) (countit (car l) l1)) (2d (cdr l) l1)))
   (t (2d (cdr l) l1))
   )
)
(defun 2drez(l)
  (2d l l))

 ; a. Write a function to determine the successor of a number represented digit by digit 
 ;    as a list. Example: (1 9 3 5 9 9) --> (1 9 3 6 0 0)
(defun invers(l)
  (cond
   ((null l) nil)
   (t (append (invers (cdr l)) (list(car l))))
   )
)

(defun 3a(l carry)
  (cond
   ((and (null l)(equal carry 1))(list 1))
   ((and (null l)(equal carry 0))nil)
   ((and (equal (car l) 9) (equal carry 1)) ( append (list 0) (3a (cdr l) 1)))
   (t (append (list (+ (car l) carry)) (3a (cdr l) 0)))
   )
)
(defun 3arez(l)
  (invers (3a (invers l) 1)))

    ;b. Write a function to return the set of all the atoms of a list.
    ;Exemplu: (1 (2 (1 3 (2 4) 3) 1) (1 4)) ==> (1 2 3 4)
(defun 3b(l)
  (cond
   ((null l) nil)
   ((isin (car l) (cdr l))(3b (cdr l)))
   (t (append (list (car l)) (3b (cdr l))))
   )
)
(defun 3bRez(l)
  (cond
   ((null l)nil)
   ((atom (car l))(3b (append (list (car l)) (3brez (cdr l)))))
   (t (3b (append (3bRez (car l)) (3bRez (cdr L)))))
   )
)

  ;c. Write a function to test whether a linear list is a set.
(Defun 3c(l)
  (cond
   ((null (cdr l))t)
   ((isin (car l) (cdr l))nil)
   (t (3c (cdr l)))
   )
)

  ;d. Write a function to eliminate the n-th element of a linear list.

(defun 3d(n l)
  (cond 
   ((< n 1)(error "Index invalid"))
   ((equal n 1)(cdr l))
   (t (append (list (car l)) (3d (decf n) (cdr l))))
   )
)

  ;a. Write a function to return the maximum value of all the numerical atoms of a list, 
  ;   at any level.

(defun 4a(l m)
  (cond
   ((null l)m)
   ((and (atom (car l)) (numberp (car l)))
       (cond
         ((> (car l) m) (setq m (car l))(4a (cdr l) m))
         (t (4a (cdr l) m))))
   ((listp (car l))(setq m (4a (car l) m))(4a (cdr l) m))
   (t (4a (cdr l) m))
   )
)

 ; d. Write a function to return the inner product of two vectors.

(defun produs(l1 l2)
    (cond
     ((null l1)0)
     ((and (null l1) (not (null l2)))l2)
     (t (+ (* (car l1) (car l2)) (produs (cdr l1) (cdr l2))))
     )
)
(defun 4d(l1 l2)
  (cond
   ((> (length l1) (length l2))(produs l2 l1))
   (t (produs l1 l2))
   )
)

;5 a. Write a function to return an association list with the two lists given as parameters. 
 ;    Example: (A B C) (X Y Z) --> ((A.X) (B.Y) (C.Z)).
(defun 5a(l l1)
  (cond
   ((null l) nil)
   (t (append (list (cons (car l) (car l1))) (5a (cdr l) (cdr l1))))
   )
)

  ;b. Write a function to determine the number of all sublists of a given list, on any level. 
  ;   A sublist is either the list itself, or any element that is a list, at any level. Example: 
  ;   (1 2 (3 (4 5) (6 7)) 8 (9 10)) => 5 (list itself, (3 ...), (4 5), (6 7), (9 10)).
(defun 5b(l m)
  (cond
   ((null l)m)
   ((listp (car l))(setq m (+ 1 ( 5b (car l) m))) (5b (cdr l) m))
   (t (5b (cdr l) m))
   )
)
(defun 5brez(l)
  (5b l 0))


 ; c. Write twice the n-th element of a linear list. Example: for (10 20 30 40 50) and n=3 will 
 ;    produce (10 20 30 30 40 50).
(defun 5c(l n)
  (cond
   ((null l)nil)
   ((equal n 1)(append (list (car l) (car l)) (5c (cdr l) (- n 1))))
   (t (append (list (car l)) (5c (cdr l) (decf n))))
   )
)

 ; d. Write a function to return the number of atoms in a list, at superficial level.
(defun 5d(l)
  (cond
   ((null l)0)
   ((atom (car l))(+ 1 (5d (cdr l))))
   (t (5d (cdr l)))
   )
)

;6.
;  a. Write a function to return the product of all numerical atoms in a list, at any level.

(defun 6a(l m)
  (cond
   ((null l)m)
   ((atom (car l))(setq m (* m (car l)))(6a (cdr l) m))
   ((listp (car l))(setq m (6a (car l) m))(6a (cdr l) m))
   (t (6a (cdr l) m))
   )
)
(defun 6aRez(l)
  (6a l 1))

  ;b. Write a function to sort a linear list with keeping the double values.
(defun elim(elem l)
  (cond
   ((null l)nil)
   ((equal elem (car l))(cdr l))
   (t (append (list (car l)) (elim elem (cdr l))))
  )
)

(defun minimum(l)
  (cond
   ((null (cdr l))(car l))
   (t (min (car l) (minimum (cdr l))))
   )
)

(defun 6b(l)
  (cond
   ((null l)nil)
   (t (append (list (minimum l)) (6b (elim (minimum l) l))))
   )
)

 ; c. Write a function to return the union of two sets.
(defun 6c (l1 l2)
  (cond
   ((null l1)l2)
   ((isin (car l1) l2)(6c (cdr l1) l2))
   (t (cons (car l1) (6c (cdr l1) l2)))
   )
)

;7.
;  a. Write a function to return the sum of all numerical atoms of a list, at any level.
(defun 7a(l m)
  (cond
   ((null l)m)
   ((numberp (car l))(setq m (+ m (car l)))(7a (cdr l) m))
   ((listp (car l))(setq m (7a (car l) m))(7a (cdr l)m))
   (t (7a (cdr l) m))
   )
)
(defun 7arez(l)
  (7a l 0))

 ; c. Write a function to test the equality of two sets, without using the difference of two sets.
(defun dif(l1 l2 x)
  (cond 
	((and (null l1)(> x 1))  nil)
	((null l2) l1)
	((member (car l1) l2)(incf x) (dif (cdr l1) l2 x))
	(t (incf x)(cons (car l1) (dif (cdr l1) l2 x)))
  )
)
(defun 7c(l1 l2)
  (setq l3 (dif l1 l2 1))
  (cond
   ((null l3)t)
   (t nil)))

;  d. Write a function to insert an element on the n-th position of a linear list.
(defun 7d (el n l)
  (cond
   ((null l)nil)
   ((equal n 1)(append (list el (car l)) (cdr l)))
   (t (append (list (car l)) (7d el (decf n) (cdr l))))
   )
)

  ;a. Write a function to check whether an atom is a member of a list not necessarily linear.
(defun 8a(el l)
  (cond
   ((null l)nil)
   ((and (atom (car l)) (eq el (car l)))t)
   ((and (atom (car l)) (not (eq el (car l)))) (8a el (cdr l)))
   (t (or (8a el (car l)) (8a el (cdr l))))
   )
)
 
  ; c. Write a function to return the n-th element of a list, or NIL if such an element 
  ;   does not exist.
(defun 8c(n l)
  (cond
   ((> n (length l))nil)
   ((eq n 1) (car l))
   (t (8c (decf n) (cdr l)))
))

 ; d. Write a function to transform a linear list into a set.
(defun 8d(l)
  (cond
   ((null l)nil)
   ((not(isin (car l) (cdr l)))(append (list (car l)) (8d (cdr l))))
   (t (8d (cdr l)))
   )
  )

 ; 10.b. Write a function to replace each sublist of a list with its last element.
  ;   A sublist is an element from the first level, which is a list.
 ;    Example: (a (b c) (d (e (f)))) ==> (a c (e (f))) ==> (a c (f)) ==> (a c f)
 ;        (a (b c) (d ((e) f))) ==> (a c ((e) f)) ==> (a c f)

(defun getlast(l)
  (cond
   ((null (cdr l))(car l))
   (t (getlast (cdr l)))
   )
)

(defun 10b(l)
  (cond
   ((null l)nil)
   ((atom (car l))(append (list(getlast l)) (10b (cdr l))))
   ((listp (car l))(append (list (getlast (car l))) (10b (cdr l))))
   (t (10b (cdr l)))
   )
)

 ;11. a. Write a function to reverse a list with its all sublists, on all levels.
(defun 11a(l)
  (cond
   ((null l)nil)
   ((atom (car l))(append (11a (cdr l)) (list (car l))))
   (t (append (11a (cdr l)) (list (11a (car l)))))
   )
)
    
 ; b. Write a function to return the list of the first elements of all list elements of 
 ;    a given list with an odd number of elements at superficial level. Example:
 ;    (1 2 (3 (4 5) (6 7)) 8 (9 10 11)) => (1 3 9).

(defun noOf(l)
  (cond
   ((null l)0)
   ((atom (car l)) (+ 1 (noof (cdr l))))
   (t (noof (cdr l)))
   )
)

(defun 11b(l)
  (cond
   ((null l)nil)
   ((and (listp (Car l))(equal (mod (noOf (car l)) 2) 1) (append (list(car(car l))) (11b (cdr l)))))
   (t (11b (cdr l)))
   )
)
(defun hasatomi(l)
  (cond
   ((null l)nil)
   ((atom (Car l)) (Car l))
   (t (hasatomi (cdr l)))
   )
)


(defun 11bRez(l)
  (cond
   ((not(eq (hasatomi l) nil))(append (list (hasatomi l))(11b l)))
   ( t (11b l))
  )
)

;12.
;  a. Write a function to replace an element E by all elements of a list L1 at all levels of a 
;     given list L.
(defun 12a(el E l)
  (cond
   ((null l) nil)
   ((and (atom (Car l)) (eq (Car l) E))(append (list el) (12a el E (cdr l))))
   ((and (atom (car l)) (Not (eq (Car l) E))) (append (list (car l)) (12a el E (cdr l))))
   (t (append (list (12a el E (car l))) (12a el E (cdr l))))
   )
)

 ; d. Write a function to return the greatest common divisor of all numbers in a linear list.
(defun cmmdc (a b)
  (cond
   ((eq a b) a)
   ((> a b)(cmmdc (- a b) b))
   (t (cmmdc a (- b a)))
   )
)
(defun 12d(l c)
  (cond
   ((null l)c)
   (t (12d (cdr l) (cmmdc c (car l))))
))
(defun 12drez(l)
  (12d l (car l)))

;13.
;   a. Write a function to test if a linear list of numbers has a "mountain" aspect
;      (a list has a "mountain" aspect if the items increase to a certain point and then decreases. 
;      Eg. 10 18 29 17 11 10).
(defun ascend(l)
  (cond
   ((null (cdr l))nil)
   ((> (car l) (cadr l))(cdr l))
   (t(ascend (cdr l)))
   )
)
(defun descend(l)
  (cond
   ((null (cdr l))nil)
   ((< (car l) (cadr l))(cdr l))
   (t (descend (cdr l)))
   )
)
(defun mountain(l)
  (cond
   ((eq (descend (ascend l)) nil)t)
   (t nil)))

(defun valley(l)
  (cond
   ((eq (ascend (descend l)) nil)t)
   (t nil)))

; b. Remove all occurrences of a maximum numerical element from a nonlinear list.
(defun maxim(l m)
  (cond
   ((null l)m)
   ((and (atom (Car l)) (>= (car l) m))(setq m (car l))(maxim (cdr l) m))
   ((and (atom (car l)) (< (car l) m)) (maxim (cdr l) m))
   (t (setq m (maxim (car l) m))(maxim (cdr l) m))
   )
)

(defun elim (el l)
  (cond
   ((null l) nil)
   ((and (atom (Car l)) (eq (car l) el))(elim el (cdr l)))
   ((and (atom (car l)) (not (eq (car l) el)))(append (list (car l)) (elim el (cdr l))))
   (t (append (list(elim el (car l))) (elim el (cdr l))))
   )
)
(defun 13b(l)
  (elim (maxim l 0) l))

 ;  d. Write a function which returns the product of numerical even atoms from a list, to any level.

(defun 13d(l p)
  (cond
   ((null l)p)
   ((and (atom (car l)) (eq (mod (car l) 2) 0))(setq p (* p (car l)))(13d (cdr l) p))
   ((and (atom (car l)) (eq (mod (car l) 2) 1))(13d (cdr l) p))
   (t (setq p (13d (car l) p))(13d (cdr l) p))
   )
)

;14.
;   a. A list of atoms is given. Define a function to get a list witl all atoms of our given list
;      in reverse order. For example: (((A B) C), (D E)) -> (E D C B A)

(defun intoarce(l)
  (cond
   ((null l) nil)
   ((atom (car l))(append (intoarce (cdr l)) (list (Car l))))
   (t (append (list (intoarce (car l))) (intoarce (cdr l))))
   )
)
(defun 14a(l)
  (cond
   ((null l) nil)
   ((atom l)(list l))
   ((list (car l))(append (14a (car l)) (14a (cdr l))))
   (t (14a (cdr l)))
   )
)

(defun 14arez(l)
  (14a (intoarce l)))

  ; b. Define a function that returns the greatest common divisor of the nonlinear list.
(defun 14b(l c)
  (cond
   ((null l)c)
   ((atom (car l))(setq c (cmmdc c (car l)))(14b (cdr l) c))
   (t (setq c (14b (car l) c))(14b (cdr l) c))
   )
)
(defun 14brez(l)
  (14b l (car l)))

   ;c. Insert into a linear list, a given atom A after the 2nd, 4th, 6th .... item.

(defun 14c(a x l)
  (cond
   ((null l)nil)
   ((eq (mod x 2) 0)(append (list a (car l)) (14c a (incf x) (cdr l))))
   (t (append (list (Car l)) (14c a (incf x) (cdr l))))
   )
)

  ; d. Write a function that determines the number of occurrences of a given atom in a nonlinear list.
(defun 14d(l a x )
  (cond
   ((null l)x)
   ((and(atom (car l)) (eq (Car l) a))(14d (cdr l) a (incf x)))
   ((and (atom (car l)) (not(eq (car l) a)))(14d (cdr l) a x))
   (t (setq x (14d (car l) a x)) (14d (cdr l) a x))
   )
)
(Defun 14drez(l a)
  (14d l a 0))

  ;15. b. A linear list is given. Eliminate from the list the elements from N to N steps, N-given.
(defun 15b(l n x)
  (cond
   ((null l) nil)
   ((eq (mod x n) 0)(15b (cdr l) n (incf x)))
   (t (append (list (car l)) (15b (cdr l) n (incf x))))
   )
)
(defun 15brez (l n)
  (15b l n 1))

  ;16. a. A linear list is given. Add a element in list from N to N steps.
(defun 16a(l el n x)
  (cond
   ((null l) nil)
   ((and (eq (mod x n) 0)(not (eq x 0)))(append (list el (Car l)) (16a (cdr l) el n (incf x))))
   (t (append (list (car l)) (16a (cdr l) el n (incf x))))
   )
)
(Defun 16arez (l el n)
  (16a l el n 0))

  ; d. Define a function to remove all occurrences of a given element from a nonlinear list.
(defun 16d(l el)
  (cond
   ((null l)nil)
   ((and(atom (car l)) (eq (car l) el)) (16d (cdr l) el))
   ((and (atom (car l)) (not(eq (car l) el))) (append (list (car l)) (16d (cdr l) el)))
   (t (append (list (16d (car l) el)) (16d (cdr l) el)))
   )
)