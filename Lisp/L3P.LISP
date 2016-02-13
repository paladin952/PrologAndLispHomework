;6.  Write a function that returns the depth of a list.
;'(1 2 (3 (4 (5))) 4)
(defun depth(l)
 (cond 
	((atom l) 0)
	(t (+ 1 (apply 'max(mapcar #'depth l))))
 )
)