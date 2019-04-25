(defun dfs (tree)
  (cond ((and (atom tree)
	      (not (null tree))) (list tree))
	((not (null tree)) (append (dfs (rest tree)) (dfs (first tree))))))

(defun prune-tree (tree height)
  (cond ((atom tree) tree)
	((and (listp tree)
	      (> height 0))
	 (cons (prune-tree (first tree) (- height 1))
	       (prune-tree (rest tree) height)))))
    
(defun dfid (tree d)
  (cond ((>= d 0) (append (dfid tree (- d 1)) (dfs (prune-tree tree d))))))

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((and (= (first s) 3)     ;if num miss = 3 and num can = 3 and theyre
	      (= (second s) 3)    ;on the west side of the river, then true.
	      (null (third s))) t)
	(t nil)))                 ;else false.

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (cond ((or (> m (first s))         ;cant move more miss than there are miss
	     (> c (second s))        ;cant move more can than there are can
	     (and (= m 0)            ;boat has to have a least one person in it 
		  (= c 0))
	     (> (+ m c) 2)           ;cant move more than 2 people at a time
	     (and (> (- (second s) c);cant allow more can than miss (departing side)
		     (- (first s) m))
		  (not (= (- (first s) m) 0)))
	     (and (> (+ (- 3 (second s)) c);cant allow more can than miss (arrival side)
		     (+ (- 3 (first s)) m))
		  (not (= (+ (- 3 (first s)) m) 0))))
	 nil)
	;if valid, move boat and m c to east side
	((null (third s)) (list (+ (- 3 (first s)) m) (+ (- 3 (second s)) c) t))
	;if valid, move boat and m c to weat side
	(t (list (+ (- 3 (first s)) m) (+ (- 3 (second s)) c) nil))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (let* ((succ-list1 (cond ((null   (next-state s 0 1)) nil)
			   (t (list (next-state s 0 1)))))
	 (succ-list2 (cond ((null   (next-state s 0 2)) succ-list1)
			   (t (cons (next-state s 0 2)  succ-list1))))
	 (succ-list3 (cond ((null   (next-state s 1 0)) succ-list2)
			   (t (cons (next-state s 1 0)  succ-list2))))
	 (succ-list4 (cond ((null   (next-state s 1 1)) succ-list3)
			   (t (cons (next-state s 1 1)  succ-list3))))
	 (succ-list5 (cond ((null   (next-state s 2 0)) succ-list4)
			   (t (cons (next-state s 2 0)  succ-list4)))))
    (cond ((null succ-list5) nil)
	  (t succ-list5))))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) nil)
	((and (= (first s) (first (first states)))
	      (= (second s) (second (first states)))
	      (or (and (null (third s))
	               (null (third (first states))))
		  (and (not (null (third s)))
		       (not (null (third (first states))))))) t)
	(t (on-path s (rest states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
  (cond ((or (null states)
	     (and (listp states)
		  (null (first states))))
	 nil)
	((final-state (first states))
	 (cons (first states) path))
	((and (not (on-path (first states) path))
	      (not (null (mult-dfs (succ-fn (first states)) (cons (first states) path)))))
	 (mult-dfs (succ-fn (first states)) (cons (first states) path)))
	(t (mult-dfs (rest states) path))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) path)
	((not (null (first (mult-dfs (succ-fn s) path))))
	 (mult-dfs (succ-fn s) path))
	(t nil)))



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
