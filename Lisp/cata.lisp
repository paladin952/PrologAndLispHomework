(defun lungime (l)
	"Compute the length of list l(on all levels)"
	(cond
		((null l) 0)
		((atom (car l)) (+ 1 (lungime (cdr l))))
		(t (+ (lungime (car l)) (lungime (crd l))))
	)
)

(defun fact (N)
	"Compute teh factorial of N"
	(if (= N 1)
		1
		(* N (fact (- N 1)))
	)
)

(defun fib (N)
	"Compute the Nth fib number"
	(if (or (zerop N) (= N 1))
		1
		(+ (fib (- N 1)) (fib (- N 2)))
	)
)

(defun sum(l)
	"Compute the sum of all numeric atoms from list l(on all levels)"
	(cond
		((null l) 0)
		((listp (first l)) (+ (sum (first l)) (sum (rest l))))
		((numberp (first l)) (+ (first l) (sum (rest l))))
		(t (sum (rest l)))
	)
)

(defun isIn(el l)
	"Check wether or not el is in list l"
	(cond
		((null l) nil)
		((= el (first l)) t)
		(t (isIn el (rest l)))
	)
)

(defun subLst (list)
  	(if (null list) '(nil)
      	(
      		let*(
	      			(a (car list))
		            (d (cdr list))
		            (s (subLst d))
           			(v (mapcar (lambda (x) (cons a x)) s))
           		)

        	(append s v)
        )
  	)
)

(defun areEqualAux(l1 l2)
	(cond 
		((null l1) t)
		((not (isIn (first l1) l2)) nil)
		(t (areEqualAux (rest l1) l2))
	)
)

(defun areEqual(l1 l2)
	(cond
		((/= (lungime l1) (lungime l2)) nil)
		(t (areEqualAux l1 l2))
	)
)

(defun insertAux(el pos curr lst)
	(cond
		((< curr pos) (cons (first lst) (insertAux el pos (+ curr 1) (rest lst))))
		((= curr pos) (cons el (insertAux el pos (+ curr 1) lst)))
		(t lst)
	)
)

(defun insert(el pos lst)
	(if (<= pos (lungime lst))
		(insertAux el pos 0 lst)
		lst
	)
)	

(defun insert2(el pos lst)
	(cond 
		((< pos 0) nil)
		((= pos 0) (cons el lst))
		(t (cons (first lst) (insert2 el (- pos 1) (rest lst))))
	)
)

(defun visit(node)
	(write node)
)

(defun subSet(lst cnt)
	(cond
		((> cnt 0) (cons (car lst) (subSet (cdr lst) (- cnt 1))))
		((= cnt 0) nil)
		(t nil)
	)
)

(defun inorder(list)
	(cond 
		((null list) nil)
		((=/ (cadr list) 0) 
			(inorder (cddr list)))
		((> (cadr list) 0) (visit (car list)))
		(t nil)
	)
)

(defun conv(list)
	(cond
		((= (cadr list) 0) (list (car list)))
	)
)	

(defun remm(el l)
	(cond
		;((null l) nil)
		((equal l el) nil)
		((listp l) (apply #'append (mapcar #'(lambda(x) (remm el x))  l)))
		((atom l) (list l))
	)
)

(defun postorderMain (l)
  	(postorder l nil)
)

(defun postorder (l lc)
 	(cond
	  ((and (null l) (null lc)) nil)

	  ((null l) (append nil (postorder lc nil)))
	 
	  ((= (cadr l) 0) (append (list (car l)) (postorder (cddr l) lc)))

	  ((= (cadr l) 1) (append nil (postorder (cddr l) (append (list (car l) (- (cadr l) 1)) lc))))

	  (t (append nil (postorder (cddr l) (append lc (list (car l) (- (cadr l) 1))))))
 	)
)

(defun inorder (l)
	(cond
		((null l) nil)
		((= (cadr l) 0) (list (car l)))
		(t (append nil (inorder (left (cddr l) 0 0)) (list (car l)) (inorder (right (cddr l) 0 0))))
	)
)

(defun left (l v m)
	(cond
		((null l) nil)

		((equal v (+ 1 m)) nil)

		(t (append nil (list (car l) (cadr l)) (left (cddr l) (+ v 1) (+ m (cadr l)))))
	)
)

(defun right (l v m)
	(cond
		((null l) nil)

		((equal v (+ 1 m)) l)

		(t (right (cddr l) (+ v 1) (+ m (cadr l))))
	)
)

(defun level (tree node)
	(cond
		((null tree) nil)
		((equal node (car tree)) 0)
		((isInTree (cadr tree) node) (+ 1 (level (cadr tree) node)))
		((isInTree (caddr tree) node) (+ 1 (level (caddr tree) node)))
		(t nil) 
	)
)

(defun isInTree (tree node)
	(cond
		((null tree) nil)

		((equal (car tree) node) t)

		(t (or (isInTree (cadr tree) node) (isInTree (caddr tree) node)))
	)
)

(defun level2 (tree node level) 
	(cond
		((null tree) 0)
		((equal (car tree) node) level)
		(t (max (level2 (cadr tree) node (+ level 1)) (level2 (caddr tree) node (+ level 1))))
	)
)

(defun depth (tree)
	(cond 
		((null tree) 0)
		(t (+ 1 (max (depth (cadr tree)) (depth (caddr tree)))))
	)
)

(defun maxListAux(list curr)
	(cond
		((null list) curr)
		((listp (car list)) (maxListAux (cdr list) (max (car list) curr))) 
		(t (maxListAux (cdr list) (max (car list) curr)))
	)
)

(defun maxList(list)
	(cond
		((null list) nil)
		;((listp (car list)) (maxList)
		(t (maxListAux (cdr list) (car list)))
	)
)

(defun depthMain(l)
	(dep l 1 0)
)

(defun dep(l curr maxim)
	(cond
		((null l) curr)
		((listp (car l)) (max (dep (car l) (+ 1 curr) maxim) (dep (cdr l) curr maxim)))
		(t (dep (cdr l) curr maxim))
	)
)

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

