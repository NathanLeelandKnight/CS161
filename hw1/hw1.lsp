;Nathan Knight
;004749179
;cs161 hw1

;Problem 1)
;Takes in a number (n) and a tree (tree). The function will return a bool
;that is true if n is in the tree and false if n is not in the tree.
(defun tree-contains (n tree)
  ;Check 1) if tree is atom and n = tree, then true.
  ;Check 2) if tree is atom and n != tree, false.
  ;Check 3) if tree has one element and n = that element, then true.
  ;Check 4) if tree has one element and n != that element, then false.
  ;Check 5/6/7) This is a basic binary search tree that will go left
  ;or right depending on the node value.
  (cond ((and (atom tree) (= n tree)) t)
	((and (atom tree) (not (= n tree))) nil)
	((and (null (rest tree)) (= n (first tree))) t)
	((and (null (rest tree)) (not (= n (first tree)))) nil)
	((= n (second tree)) t)
	((< n (second tree)) (tree-contains n (first tree)))
	((> n (second tree)) (tree-contains n (rest (rest tree))))))

;Problem 2)
;Takes in a tree (tree) and returns the left most node. Aka, the min num
(defun tree-min (tree)
  ;Check if the tree is an atome first and return the tree if so
  ;Checks if the first element is list/tree. If so, recursively call the
  ;function on the first element. If not, return the first element
  (cond ((atom tree) tree)
	((listp (first tree)) (tree-min (first tree)))
	(t (first tree))))

;Problem 3)
;Takes in a tree (tree) and returns a list of pre-ordered numbers
(defun tree-order (tree)
  ;Variable (po-list) that will store the pre-ordered numbers as a list
  (let* ((po-list '()))
    ;Check 1) if the tree is a atom, store the atom in the po-list and
    ;return po-list
    ;Check 2) if the tree is has 1 element, store and return it in po-list
    ;Check 3) catch all that will recursively call the function on the
    ;left and right sides and append everything together.
    (cond ((atom tree) (cons tree po-list))

	  ((= (length tree) 1) (cons (first tree) po-list))
	  (t (cons (second tree)
		   (append (tree-order (first tree))
			   (append (tree-order (rest (rest tree))) po-list)))))))

;Problem 4)
;Takes in as arguments list (l), starting index (start), and length (len).
;This function will return a list that starts on the index specified by
;(start) and will include any number of incremented indexes specified by (l).
(defun sub-list (l start len)
  ;Variable (new-list) that will store the sub-list.
  (let* ((new-list '()))
    ;Check 1) if start is greater than 0, then traverse the list by (start) amount.
    ;Check 2) Once the start index is found, include the appropriate length.
    (cond ((or (>= start (length l))
	       (< start -1))
	   '())
	  ((not (<= start 0)) (sub-list (rest l) (- start 1) len))
	  ((not (<= len 0)) (cons (first l) (sub-list (rest l) 0 (- len 1)))))))

;Problem 5)
;Takes in a list (l) as an argument and will split the list in half creating
;two list where the first one has 1 more element than the second or they are
;of equal length. These two list will be stored in a list themselves. 
(defun split-list (l)
  ;Check 1) if length of l is 0 just return nil.
  ;Check 2) if length of l is 1 create a list with just l in it.
  ;Check 3) Catch all that will split the list with the help of sub-list and
  ;cons. First call sub-list on the first half of the list. Then call sub-list
  ;on the second half.
  (cond ((= (length l) 0) '())
	((= (length l) 1) (cons l '()))
	(t (cons (sub-list l 0 (/ (length l) 2))
		 (cons (sub-list l (/ (length l) 2) (/ (length l) 2))
		       '())))))

;Problem 6)
;Takes in a tree (tree) as an argument and returns the height of that tree
(defun btree-height (tree)
  ;Check 1) if the tree is an atom then the height is 0.
  ;Check 2/3) add one and call the function recursively first looking left
  ;then looking right. Return which ever returns the larger number. 
  (cond ((atom tree) 0)
	((< (+ 1 (btree-height (first tree)))
	    (+ 1 (btree-height (first (rest tree)))))
	 (+ 1 (btree-height (first (rest tree)))))
	(t (+ 1 (btree-height (first tree))))))

;Problem 7)
;Takes in a list (leaves) of atoms that will be returned as a binary tree
;defined by the hw.
(defun list2btree (leaves)
  ;Check 1) if leaves is length 1, then just return the atom in leaves 
  ;Check 2) if leaves is length 2, then just return leaves
  ;Check 3) if leaves is length 3 or greater, then call the function
  ;recursively on the first half and the first element of the second half
  ;which will be the second half. Finally make a list of the two results.
  (cond ((= (length leaves) 1) (first leaves))
	((= (length leaves) 2) leaves)
	((>= (length leaves) 3)
	 (list (list2btree (first (split-list leaves)))
	       (list2btree (first (rest (split-list leaves))))))))

;Problem 8)
;Take in a tree (tree) and return a list of atoms. Inverse of Problem 7
(defun btree2list (tree)
  ;Check 1) if tree is an atom, return a list with the single atom inside.
  ;Check 2) Catch all that will call the function recursively on the first
  ;half and the second half and finally append them together to make a list
  ;of atoms.
  (cond ((atom tree) (cons tree '()))
	(t (append (btree2list (first tree)) (btree2list (first (rest tree)))))))

;Problem 9)
;Take in to LISP expressions (e1) and (e2) and return a boolean if they are
;the same or not.
(defun is-same (e1 e2)
  ;Check 1) if e1 and e2 are atoms and if e1 and e2 are both null, then true
  ;Check 2) if e1 and e2 are list and the recursive call on both the first
  ;and the rest of the expressions returns true, then true.
  ;Check 3) Catch all that returns nil.
  (cond ((and (atom e1)
	      (atom e2)
	      (or
	       (and (null e1)
		    (null e2))
	       (= e1 e2))
	      )
	 t)
	((and (listp e1)
	      (listp e2)
	      (is-same (first e1) (first e2))
	      (is-same (rest e1) (rest e2))) t)
	(t nil)))
