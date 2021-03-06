(defun reload-all ()
  (load "hw4.lsp")
  (load "parse_cnf.lsp"))

(defun compare-ans-lit (ans lit)
  (let* ((cur (first ans)))
    (cond ((null ans) t)
	  ((= cur lit) t)
	  ((= (+ cur lit) 0) nil)
	  (t (compare-ans-lit (rest ans) lit)))))

(defun compare-ans-clause (ans clause)
  (cond ((null clause) nil)
	((compare-ans-lit ans (first clause)) t)
	(t (compare-ans-clause ans (rest clause)))))

(defun compare-ans-delta (ans delta)
  (cond ((null delta) t)
	(t (and (compare-ans-clause ans (first delta))
		(compare-ans-delta ans (rest delta))))))

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
;; and the current constraints (delta). It will remove any constraint
;; where all variables are assigned
;;
(defun compare-remove (n delta)
  (cond ((or (null delta)
	     (null n))
	 delta)
	((<= (list-max-var 0 (first delta)) n) (compare-remove n (rest delta)))
	(t (cons (first delta) (compare-remove n (rest delta))))))

(defun create-ans (n ans sign)
  (cond ((and (< (length ans) n)
	      (equal sign 'pos))
	 (cons (+ (length ans) 1) ans))
	((and (< (length ans) n)
	      (equal sign 'neg))
	 (cons (* (+ (length ans) 1) -1) ans))
	(t ans)))

(defun sat-dfs (n delta ans)
  (let* ((ans-pos (create-ans n ans 'pos))
	 (ans-neg (create-ans n ans 'neg))
	 (delta-new (compare-remove (list-max-var 0 ans) delta)))
    (cond ((compare-ans-delta ans delta)
	   (cond ((= (length ans) n)
		  ans)
		 (t (or (sat-dfs n delta-new ans-pos)
			(sat-dfs n delta-new ans-neg))))))))
;;	  ((and (compare-ans-delta ans-pos delta-new)
;;		(sat-dfs n delta-new ans-pos))
;;	   (sat-dfs n delta-new ans-pos))
;;	  ((and (compare-ans-delta ans-neg delta-new)
;;		(sat-dfs n delta-new ans-neg))
;;	   (sat-dfs n delta-new ans-neg))
;;	  (t nil))))

(defun sat? (n delta)
  (sat-dfs n delta '()))  
