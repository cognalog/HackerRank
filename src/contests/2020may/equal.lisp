(defun start-states ()
  (loop for i from 1 to (read) collect
       (loop for j from 1 to (read) collect
	    (read))))

(defun state (node) (first node))
(defun g-hat (node) (second node))
(defun h-hat (node) (third node))
(defun parent (node) (fourth node))

(defun fair (state)
  (cond
    ((or (null state) (null (cdr state))) t)
    ((eql (car state) (cadr state)) (fair (cdr state)))
    (t nil)))

(defun exclude (co n state)
  (cond
    ((null state) state)
    ((eql co 0) (cons (car state) (exclude (- co 1) n (cdr state))))
    (t (cons (+ (car state) n) (exclude (- co 1) n (cdr state))))))

(defun next-moves (state)
  (loop for i from 0 to (- (length state) 1) append
       (loop for j in '(1 2 5) collect
	    (exclude i j state))))

(defun std (state)
  (let ((avg (/ (apply #'+ state) (length state))))
    (expt (/ (apply #'+ (mapcar (lambda (x) (expt (- avg x) 2)) state))
	     (length state))
	  0.5)))

(defun kids (node)
  (mapcar (lambda (new-state)
	    `(,new-state
	      ,(+ (g-hat node) 1)
	      ,#|(+ 1 (g-hat node)|#(std new-state) ;greedy seems to work best
	      ,node))
	  (next-moves (state node))))

(defun solution (node)
  (if (null (parent node))
      0
      (+ 1 (solution (parent node)))))

(defun findual (e ls)
  (cond
    ((null ls) ls)
    ((equal (car ls) e) t)
    (t (findual e (cdr ls)))))

(defun diff (l1 l2)
  (cond
    ((or (null l1) (null l2)) l1)
    ((findual (car (car l1)) (mapcar (lambda (l) (car l)) l2)) (diff (cdr l1) l2))
    (t (cons (car l1) (diff (cdr l1) l2)))))

(defun a* (s0)
  (let ((opn `((,s0 0 ,(std s0) nil)))
	(cls nil)
	(node nil)
	(children nil))
    (loop
       (if (null opn) (return-from a* "failed"))
       (setf node (pop opn))
       (if (fair (state node)) (return-from a* (solution node)))
       (push node cls)
       (setf children (kids node))
       (setf children (diff children cls))
       (setf opn (append opn children))
       (setf opn (sort opn (lambda (n1 n2) (< (h-hat n1) (h-hat n2))))))))

(defun driver ()
  (loop for s in (start-states) do
       (format t "~a" (a* s))))

(driver)