(defun tree-contains (n tree)
  ;Check 1/2 checks to see if the tree is an atom to begin with. If so,
  ;check if n matches the atom tree. If so, return t if a match nil
  ;otherwise
  
  ;Check 3 checks if the first element is list/tree. If so, check
  ;recursively that n is contained in this first element. If both
  ;of these are true, then return true.

  ;Check 4 checks if the first element is not a list/tree and if so, it
  ;checks to see if that element is n.

  ;Check 5. If it makes it this far, it means we are now running through
  ;a list (not a tree) checking the elements. This check will return nil
  ;if it has reached the end of the list and n hasn't been found. This
  ;is used in check 1 to determine whether or not to return t.

  ;Check 6 is a catch all that will recursively call the function on the
  ;rest of the tree.
  (cond ((and (atom tree) (= n tree)) t)
	((and (atom tree) (not(= n tree))) nil)
	((and (listp (first tree)) (tree-contains n (first tree))) t)
	((and (not(listp (first tree))) (= (first tree) n)) t)
	((null (rest tree)) nil)
	(t (tree-contains n (rest tree)))))

(defun tree-min (tree)
  ;Checks if the first element is list/tree. If so, recursively call the
  ;function on the first element. If not, return the first element
  (cond ((atom tree) tree)
	((listp (first tree)) (tree-min (first tree)))
	(t (first tree))))

;(defun tree-order (tree)
;  (let* ((po-list '()))
;    (cond ((null 
    ;(cond ((listp (first tree)) (append (tree-order (first tree)) (tree-order (rest tree))))
	  
;	  (t po-list))))
