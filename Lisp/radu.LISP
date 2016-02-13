(DEFUN in-order (my_tree)
    (COND
        ((NULL my_tree) NIL)
        ((NULL (CDR my_tree)) (LIST (CAR my_tree)))
        ((APPEND (in-order (CADR my_tree)) (LIST (CAR my_tree)) (in-order (CADDR my_tree))))
    )
)
 
(DEFUN pre-order (my_tree)
    (COND
        ((NULL my_tree) NIL)
        ((NULL (CDR my_tree)) (LIST (CAR my_tree)))
        ((APPEND (LIST (CAR my_tree)) (pre-order (CADR my_tree)) (pre-order (CADDR my_tree))))
    )
)
 
;; For post-order and pre-order, all you need to do is move the recursive calls around, in the last branch of the above function.
;; Post-order is (post-order left_child) (post-order right_child) root.
;; Pre-order is root (pre-order left_child) (pre-order right_child).
 
;; -------------------------------------------------------------------------------------------------------------------------------------
;; 8. Nodes on the n-th level of a binary tree.
 
;; Logic:
;; - if tree is null, return nada;
;; - if the destination level is less than 1 (this is a validation thing, since our tree starts at level 0), return the full tree;
;; - if the current and destination levels are equal, then we're on the level we needed to get to, so return the root of it;
;; - otherwise, go down one level (which implies increasing the current level by 1, and calling the function on the children of the root).
 
(DEFUN get-nth-level-aux (dest_level crt_level my_tree)
    (COND
        ((NULL my_tree) NIL)
        ((< dest_level 0) my_tree)
        ((EQUAL dest_level crt_level) (LIST (CAR my_tree)))
        ((APPEND (get-nth-level-aux dest_level (+ crt_level 1) (CADR my_tree)) (get-nth-level-aux dest_level (+ crt_level 1) (CADDR my_tree))))
    )
)
 
(DEFUN get-nth-level (dest_level my_tree)
        (get-nth-level-aux dest_level 0 my_tree)
)
 
;; -------------------------------------------------------------------------------------------------------------------------------------
;; 9. Get the level that a node is on. Level numbering starts at 0.
 
;; Logic:
;; - if the tree is null, you're done;
;; - if the node is equal to the root, return the current level;
;; - if the function call on the left subtree returned null, call it on the right subtree (i.e. if your node isn't on the left side, then it has to be on the right side);
;; - otherwise, call it on the left subtree.
(DEFUN get-node-level-aux (node crt_level my_tree)
    (COND
        ((NULL my_tree) NIL)
        ((EQUAL (CAR my_tree) node) crt_level)
        ((NULL (get-node-level-aux node (+ 1 crt_level) (CADR my_tree))) (get-node-level-aux node (+ 1 crt_level) (CADDR my_tree)))
        ((get-node-level-aux node (+ 1 crt_level) (CADR my_tree)))
    )
)
 
(DEFUN get-node-level (node my_tree)
    (get-node-level-aux node 0 my_tree)
)
 
;; -------------------------------------------------------------------------------------------------------------------------------------
;; Determine whether a tree is balanced or not (i.e. the difference between the depths of the subtrees is <= 1).
;; Visualization:
;;   A                    |    A
;;  / \                   |   / \
;; B   C     -> balanced  |  B   C     -> unbalanced (look at the depth difference between leaves B and F)
;;    / \                 |     / \
;;   D   E                |    D   E
;;                        |     \
;;                        |      F
 
;; max function
(DEFUN get-max (val1 val2)
    (COND
        ((NOT (NUMBERP val1)) val2)
        ((NOT (NUMBERP val2)) val1)
        ((>= val1 val2) val1)
        (val2)
    )
)
 
;; function for computing the absolute value of a variable.
(DEFUN abs-val (val)
    (COND
    ((NOT (NUMBERP val)) val)
    ((< val 0) (- 0 val))
    (val)
    )
)
 
;; The bread and butter of the algorithm.
;; Will compute the maximum height of a binary tree, and will also check if the difference in height
;; between the left and right subtrees is > 1 (it will return -1 if that's the case).
;; Logic:
;; - null tree = height of 0;
;; - if left subtree height is equal to -1, or right subtree height is equal to -1 (i.e. if either subtree is unbalanced), return -1;
;; - if the absolute value of the difference betweeen the heights of the left and right subtrees is > 1, return - 1 (this means one subtree has depth > the other subtree's depth + 1);
;; - otherwise, return 1 + whichever subtree had the greater height (the + 1 is so that we also count the current level).
(DEFUN check-heights (my_tree)
    (COND
        ((NULL my_tree) 0)
        ((OR (EQUAL -1 (check-heights (CADR my_tree))) (EQUAL -1 (check-heights (CADDR my_tree)))) -1)
        ((> (abs-val (- (check-heights (CADR my_tree)) (check-heights (CADDR my_tree)))) 1) -1)
        ((+ 1 (get-max (check-heights (CADR my_tree)) (check-heights (CADDR my_tree)))))
    )
)
 
(DEFUN is-balanced (my_tree)
    (COND
        ((EQUAL (check-heights my_tree) -1) NIL)
        (T)
    )
)
 
;; 11. The number of levels of a tree (this loosely translates to computing the lowest level of the tree and adding 1 to it, since we start at level 0).
 
;; Logic:
;; - null tree? 0 levels;
;; - otherwise, add 1 to max(get-level-count(left_subtree), get-level-count(right-subtree)).
(DEFUN get-level-count (my_tree)
    (COND
        ((NULL my_tree) 0)
        ((+ 1 (get-max (get-level-count (CADR my_tree)) (get-level-count (CADDR my_tree)))))
    )
)
 
;; 12. Get the level that has the most nodes, as well as the list of nodes on that level.
 
;; Function to get all nodes of a tree, grouped by level (element 0 has a list of all nodes on level 0, element 1 has a list of all nodes on level 1, etc.)
;; Logic:
;; - if list is empty, return nothing;
;; - if the current level is greater than the lowest level of the tree, return nothing;
;; - otherwise, return the list of all nodes on the current level, and add them to the list of whatever we'll get when we move to the next level.
(DEFUN get-nodes-by-level-aux (crt_level my_tree)
    (COND
        ((NULL my_tree) NIL)
        ((>= crt_level (get-level-count my_tree)) NIL)
        ((APPEND (LIST (get-nth-level crt_level my_tree)) (get-nodes-by-level-aux (+ 1 crt_level) my_tree)))
    )
)
 
(DEFUN get-nodes-by-level (my_tree)
    (get-nodes-by-level-aux 0 my_tree)
)
 
;; function to compute the length of a list.
(DEFUN list-len (my_list)
    (COND
        ((NULL my_list) 0)
        ((+ 1 (list-len (CDR my_list))))
    )
)
 
(DEFUN max-len-pos-aux (res_pos crt_len crt_pos my_list)
    (COND
        ((NULL my_list) res_pos)
        ((NOT (LISTP (CAR my_list))) (max-len-pos-aux res_pos crt_len (+ 1 crt_pos) (CDR my_list)))
        ((> (list-len (CAR my_list)) crt_len) (max-len-pos-aux crt_pos (list-len (CAR my_list)) (+ 1 crt_pos) (CDR my_list)))
        ((max-len-pos-aux res_pos crt_len (+ 1 crt_pos) (CDR my_list)))
    )
)
 
(DEFUN max-len-pos (my_list)
    (max-len-pos-aux 0 0 0 my_list)
)
 
(DEFUN get-level-with-most-nodes (my_tree)
    (LIST (max-len-pos (get-nodes-by-level my_tree)) (CAR (NTHCDR (max-len-pos (get-nodes-by-level my_tree)) (get-nodes-by-level my_tree))))
)
 
;; 15. Return the path from root to a given node.
 
(DEFUN has-node (node my_tree)
    (COND
        ((NULL my_tree) NIL)
        ((EQUAL node (CAR my_tree)) T)
        ((OR (has-node node (CADR my_tree)) (has-node node (CADDR my_tree))))
    )
)
 
(DEFUN find-path-to-node (node my_tree)
    (COND
        ((NULL my_tree) NIL)
        ((EQUAL node (CAR my_tree)) (LIST (CAR my_tree)))
        ((has-node node (CADR my_tree)) (APPEND (LIST (CAR my_tree)) (find-path-to-node node (CADR my_tree))))
        ((has-node node (CADDR my_tree)) (APPEND (LIST (CAR my_tree)) (find-path-to-node node (CADDR my_tree))))
        (NIL)
    )
)