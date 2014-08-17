(defun state (node) (first node))
(defun action (node) (second node))
(defun parent (node) (third node))
(defun g-hat (node) (fourth node))
(defun s-hat (node) (fifth node))

(defun diff (l1 l2)
  (cond
    ((or (null l1) (null l2)) l1)
    ((findual (car (car l1)) (mapcar (lambda (l) (car l)) l2)) (diff (cdr l1) l2))
    (t (cons (car l1) (diff (cdr l1) l2)))))

;compute the manhattan cost given initial & goal states
(defun manhat (s0 sg)
  (let ((sum 0) (tiles (- (expt (- (length s0) 1) 2) 1)))
    (loop for i from 0 to tiles
	 do
	 (setf sum (+ sum (+ (abs (- (get-y i s0) (get-y i sg)))
				  (abs (- (get-x i s0) (get-x i sg)))))))
    sum))

(defun star-succ (node ops h sg)
  (let ((s-nodes nil) (s-states nil) (n-state (state node)))
    (loop for op in ops
       do
	 (setf s-states `(,(funcall op n-state))) ;new states created
	 (setf s-nodes 
	       (append s-nodes
		      (mapcar (lambda (s-state) ;new states --> nodes
				(cond
				((null s-state) nil)
				(t `(,s-state 
				     ,op 
				     ,node
				     ,(+ 1 (g-hat node))
				     ,(+ 1 (g-hat node) (funcall h s-state sg))))))
			      s-states))))
    s-nodes))

(defun stupdate (kids ahead)
  (let ((curr-node nil) (sub-open nil))
    (loop 
       (if (null kids) (return-from stupdate ahead))
       (setf curr-node (pop kids))
       (setf sub-open(member-state (state curr-node) ahead))
       (if sub-open
	   (let ((old-node (first sub-open)))
	     (if (< (s-hat curr-node) (s-hat old-node))
		 (setf (first sub-open) curr-node)))
	   (push curr-node ahead)))))

(defun a-star (s0 sg heur stats?)
;list of unexplored nodes, starting with s0
  (let ((ahead `((,s0 ,nil ,nil ,0 (funcall heur s0 sg))))
	(behind nil) ;list of explored nodes
	(n nil)
	(children nil)
	(node-count 1)
	(redundancy 0))
    (loop
       (if (null ahead) (return-from a-star "no goal state found :("))
       (setf n (pop ahead)) ;set N to first explored node
       (if (equal (state n) sg) ;if goal state has been reached
	   (if stats?
	       (return-from a-star (cons (remove nil (solution n))
				      `(,node-count 
					,redundancy
					,(length ahead)
					,(length behind))))
	       (return-from a-star (remove nil (solution n)))))
       (push n behind)
					;collect n's child nodes into a list
       (setf children (remove nil (star-succ n `(,#'north ,#'south ,#'east ,#'west) 
					     heur
					     sg)))
					;get rid of nodes with states we already have covered
       (setf node-count (+ node-count (length children)))
       (let ((temp (length children)))
	 (setf children (diff children behind))
	 (setf redundancy (+ redundancy (- temp (length children)))))
       (setf ahead (stupdate children ahead))
       (setf ahead
	     (sort ahead (lambda (n1 n2) (< (s-hat n1) (s-hat n2))))))))