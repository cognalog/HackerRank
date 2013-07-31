;node accessor fxns
(defun state (node) (first node))
(defun action (node) (second node))
(defun parent (node) (third node))
(defun depth (node) (fourth node))
(defun g-hat (node) (fourth node))

(defun diff (l1 l2)
  (cond
    ((or (null l1) (null l2)) l1)
    ((findual (car (car l1)) (mapcar (lambda (l) (car l)) l2)) (diff (cdr l1) l2))
    (t (cons (car l1) (diff (cdr l1) l2)))))

(defun zero (n)
  (eq n 0))

(defun solution (n)
  (cond
    ((null n) (print 'Solution) n)
    (T (append (solution (parent n)) `(,(action n))))))

(defun pos (e ls)
  (cond
    ((equal e (car ls)) 0)
    (T (+ 1 (pos e (cdr ls))))))

;get the row of a tile in a state
(defun get-y (e state)
  (cond
    ((null state) nil)
    ((find e (car state)) 0)
    (t (+ 1 (get-y e (cdr state))))))

;get the column of a tile in a state
(defun get-x (e state)
  (pos e (getl (get-y e state) state)))

;compute the manhattan cost given initial & goal states
(defun manhat (s0 sg)
  (let ((sum 0) (tiles (- (expt (- (length s0) 1) 2) 1)))
    (loop for i from 0 to tiles
	 do
	 (setf sum (+ sum (+ (abs (- (get-y i s0) (get-y i sg)))
				  (abs (- (get-x i s0) (get-x i sg)))))))
    sum))

;compute how many tiles are out of place
(defun outta-place (s0 sg)
  (let ((count 0) (tiles (- (expt (- (length s0) 1) 2) 1)))
    (loop for i from 0 to tiles
	 do (if (not (and (equal (get-y i s0) 
				 (get-y i sg))
			  (equal (get-x i s0)
				 (get-x i sg))))
		(setq count (1+ count))))
    count))

(defun row-and-column (s0 sg)
  (let ((count 0) (tiles (- (expt (- (length s0) 1) 2) 1)))
    (loop for i from 0 to tiles
       do 
	 (if (not (equal (get-y i s0) (get-y i sg)))
	     (setq count (1+ count)))
	 (if (not (equal (get-x i s0) (get-x i sg)))
	     (setq count (1+ count))))
    count))
	 

(defun get-0-x (state) (cadr (getl (- (length state) 1) state)))
(defun get-0-y (state) (car (getl (- (length state) 1) state)))

(defun getl (n ls)
  (cond
    ((zero n) (car ls))
    (T (getl (- n 1) (cdr ls)))))

(defun setl (n ls e)
  (cond
    ((null ls) ls)
    ((zero n) (cons e (setl (- n 1) (cdr ls) e)))
    (T (cons (car ls) (setl (- n 1) (cdr ls) e)))))

(defun grid-get (x y grid)
  (cond
    ((null grid) grid)
    ((zero y) (getl x (car grid)))
    (T (grid-get x (- y 1) (cdr grid)))))

(defun grid-set (x y grid e)
    (cond
      ((null grid) grid)
      ((zero y) (cons (setl x (car grid) e) (grid-set x (- y 1) (cdr grid) e)))
      (T (cons (car grid) (grid-set x (- y 1) (cdr grid) e)))))

(defun succ-fxn (node ops)
  (let ((s-nodes nil) (s-states nil) (n-state (state node)))
    (loop for op in ops
       do
	 (setf s-states `(,(funcall op n-state))) ;new states created
	 (setf s-nodes 
	       (append s-nodes
		      (mapcar (lambda (s-state) ;new states --> nodes
				(cond
				((null s-state) nil)
				(t `(,s-state ,op ,node))))
			      s-states))))
    s-nodes))

;OPERATOR FUNCTIONS FOR N-PUZZLE

(defun north (parent)
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (zero y) (return-from north nil))
    (setf child (grid-set x y child (grid-get x (- y 1) child)))
    (setf child (grid-set x (- y 1) child 0)) ;swap blank
    (setf child (grid-set 0 (- (length child) 1) child (- y 1))) ;update coords
    child))

(defun west (parent)
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (zero x) (return-from west nil))
    (setf child (grid-set x y child (grid-get (- x 1) y child)))
    (setf child (grid-set (- x 1) y child 0)) ;swap blank
    (setf child (grid-set 1 (- (length child) 1) child (- x 1))) ;update coords
    child))

(defun south (parent)
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (eq y (- (length child) 2)) (return-from south nil))
    (setf child (grid-set x y child (grid-get x (+ y 1) child)))
    (setf child (grid-set x (+ y 1) child 0)) ;swap blank
    (setf child (grid-set 0 (- (length child) 1) child (+ y 1))) ;update coords
    child))


(defun east (parent)
  (let ((x (get-0-x parent)) (y (get-0-y parent)) (child parent))
    (if (eq x (- (length (car child)) 1)) (return-from east nil))
    (setf child (grid-set x y child (grid-get (+ x 1) y child)))
    (setf child (grid-set (+ x 1) y child 0)) ;swap blank
    (setf child (grid-set 1 (- (length child) 1) child (+ x 1))) ;update coords
    child))
