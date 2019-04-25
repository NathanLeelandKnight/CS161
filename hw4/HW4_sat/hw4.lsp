;;
;; Function that will load both hw4.lsp and parse_cnf.lsp
;;
(defun reload-all ()
  (load "hw4.lsp")
  (load "parse_cnf.lsp"))

;;
;; Function that will return nil if lit if found negated in ans.
;; It will return t otherwise
;;
(defun compare-ans-lit (ans lit)
  (let* ((cur (first ans)))
    (cond ((null ans) t)
	  ((= cur lit) t)
	  ((= (+ cur lit) 0) nil)
	  (t (compare-ans-lit (rest ans) lit)))))

;;
;; Function that will compare ans with a clause. 
;; If ans causes clause to be false then false else t
;;
(defun compare-ans-clause (ans clause)
  (cond ((null clause) nil)
	((compare-ans-lit ans (first clause)) t)
	(t (compare-ans-clause ans (rest clause)))))

;;
;; Function that will compare ans with a list of clauses (delta)
;; If ans causes any clause in delta to be false then false else t 
;;
(defun compare-ans-delta (ans delta)
  (cond ((null delta) t)
	(t (and (compare-ans-clause ans (first delta))
		(compare-ans-delta ans (rest delta))))))

;;
;; Function that returns the largest indexed variable in a clause (list)
;; Example '(1 -54 3 -5 45) ==> 54
;;
(defun list-max-var (max list)
  (cond ((null list) 0)
	((and (null (second list))
	      (< (first list) 0)
	      (> (* (first list) -1) max))
	 (* (first list) -1))
	((and (null (second list))
	      (> (first list) max))
	 (first list))
	((null (second list))
	 max)
	((and (< (first list) 0)
	      (> (* (first list) -1) max))
	 (list-max-var (* (first list) -1) (rest list)))
	((> (first list) max)
	 (list-max-var (first list) (rest list)))
	(t (list-max-var max (rest list)))))


;;
;; compare-remove takes in the number of variables used so far (n)
;; and the current clauses (delta). It will remove any clause
;; where all variables are assigned
;;
(defun compare-remove (n delta)
  (cond ((or (null delta)
	     (null n))
	 delta)
	((<= (list-max-var 0 (first delta)) n) (compare-remove n (rest delta)))
	(t (cons (first delta) (compare-remove n (rest delta))))))

;;
;; Function that will create a new variable with the specified
;; sign if ans has fewer variables than n
;;
(defun create-ans (n ans sign)
  (cond ((and (< (length ans) n)
	      (equal sign 'pos))
	 (cons (+ (length ans) 1) ans))
	((and (< (length ans) n)
	      (equal sign 'neg))
	 (cons (* (+ (length ans) 1) -1) ans))
	(t ans)))

;;
;; Function that will do backtracking dfs. If ans fails to fulfill
;; delta then backtrack else keep running dfs until a solution is found
;;
(defun sat-dfs (n delta ans)
  (let* ((ans-pos (create-ans n ans 'pos))
	 (ans-neg (create-ans n ans 'neg))
	 (delta-new (compare-remove (list-max-var 0 ans) delta)))
    (cond ((compare-ans-delta ans delta)
	   (cond ((= (length ans) n)
		  ans)
		 (t (or (sat-dfs n delta-new ans-pos)
			(sat-dfs n delta-new ans-neg))))))))

;;
;; Top level function that will call sat-dfs with empty ans
;;
(defun sat? (n delta)
  (sat-dfs n delta '()))  
