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

;Takes in a tree (tree) and returns the left most node. Aka, the min num
(defun tree-min (tree)
  ;Check if the tree is an atome first and return the tree if so
  ;Checks if the first element is list/tree. If so, recursively call the
  ;function on the first element. If not, return the first element
  (cond ((atom tree) tree)
	((listp (first tree)) (tree-min (first tree)))
	(t (first tree))))

;Takes in a tree (tree) and returns a list of pre-ordered numbers
(defun tree-order (tree)
  ;variable (po-list) that will store the pre-ordered numbers as a list
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
;	  ((listp (first tree))
;	   (cons (second tree)
;		 (append (tree-order (first tree))
;			 (append (tree-order (rest (rest tree))) po-list))))
;
;	  ((and (not (listp (first tree)))
;		(not (null (rest (rest tree)))))
;	   (cons (second tree)
;		 (append (tree-order (first tree))
;		       (append (tree-order (rest (rest tree))) po-list)))))))

(defun sub-list (l start len)
  (let* ((new-list '()))
    (cond ((not (= start 0)) (sub-list (rest l) (- start 1) len))
	  ((not (= len 0)) (cons (first l) (sub-list (rest l) 0 (- len 1)))))))
	  ;(t '()))))
