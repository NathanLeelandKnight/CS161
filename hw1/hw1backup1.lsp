(defun tree-contains (n tree)
  (cond ((and (atom tree) (= n tree)) t)
	((and (atom tree) (not (= n tree))) nil)
	((and (null (rest tree)) (= n (first tree))) t)
	((and (null (rest tree)) (not (= n (first tree)))) nil)
	((= n (second tree)) t)
	((< n (second tree)) (tree-contains n (first tree)))
	((> n (second tree)) (tree-contains n (rest (rest tree))))))
  
(defun tree-min (tree)
  ;Checks if the first element is list/tree. If so, recursively call the
  ;function on the first element. If not, return the first element
  (cond ((atom tree) tree)
	((listp (first tree)) (tree-min (first tree)))
	(t (first tree))))

(defun tree-order (tree)
  (let* ((po-list '()))
    (cond ((atom tree) (cons tree po-list))

	  ((= (length tree) 1) (cons (first tree) po-list))

	  ((listp (first tree))
	   (cons (second tree)
		 (append (tree-order (first tree))
			 (append (tree-order (rest (rest tree))) po-list))))

	  ((and (not (listp (first tree)))
		(not (null (rest (rest tree))))
		(not (= (length (rest (rest tree))) 1)))
	   (cons (second tree)
		 (cons (tree-order (first tree))
		       (append (tree-order (rest (rest tree))) po-list))))

	  ((and (not (listp (first tree)))
		(not (null (rest (rest tree))))
		(= (length (rest (rest tree))) 1))
	   (cons (second tree)
		 (cons (tree-order (first tree))
		       (cons (tree-order (rest (rest tree))) po-list)))))))
