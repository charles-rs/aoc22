(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)

(defun parse-line (line)
  (destructuring-bind (start end) (mapcar (curry 'cl-ppcre:split " ") (cl-ppcre:split "; " line))
    (let ((name (cadr start))
	  (rate (parse-integer
		 (cadr (cl-ppcre:split "=" (car (reverse start))))
		 :junk-allowed t))
	  (child (cadr (split-sequence "valve" end :test 'equal)))
	  (children (cadr (split-sequence "valves" end :test 'equal))))
      (list (read-from-string name) rate (mapcar 'read-from-string (revappend child children))))))

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (parse-line line))))

(defun dist (graph nd others)
  (let ((visited (make-hash-table)))
    (labels ((bfs (acc frontier depth)
	       (if frontier
		   (let* ((found (intersection frontier others))
			  (next-acc
			    (revappend (mapcar (lambda (n) (cons n depth)) found) acc))
			  (new-frontier (loop :for n in frontier
					      :append (progn
							(setf (gethash n visited) t)
							(remove-if (rcurry 'gethash visited)
								   (gethash n graph))))))
		     (bfs next-acc new-frontier (1+ depth)))
		   acc)))
      (bfs nil (list nd) 0))))

(defun get-graph (valves)
  (let ((graph (make-hash-table))
	(potential (make-hash-table))
	(non-empty '(AA))
	(weighted (make-hash-table)))
    (loop :for v in valves
	  :do (destructuring-bind (name amt children) v
		(setf (gethash name graph) children)

		(if (> amt 0)
		    (push name non-empty))))
    (mapc (lambda (n) (setf (gethash n weighted)
		       (delete-duplicates
			(dist graph n (remove n non-empty))
			:test 'equal)))
	    non-empty)
    weighted))

(defun max-by-key (lst)
  (labels ((helper (acc lst)
	     (if lst
		 (if (> (caar lst) (car acc))
		     (helper (car lst) (cdr lst))
		     (helper acc (cdr lst)))
		 acc)))
    (helper (car lst) (cdr lst))))

(defun part-1 ()
  (let* ((valves (get-nums))
	 (potential (make-hash-table))
	 (graph (get-graph valves)))
    (loop :for v in valves
	  :do (*let (((name amt _) v))
		(setf (gethash name potential) amt)))
    (labels ((best-score (loc time-left open)
	       (if (<= time-left 0)
		   '(0)
		   (let ((children (remove-if (lambda (x) (find (car x) open))
					      (gethash loc graph))))
		     (let ((children-scores-op (mapcar (lambda (child) (best-score (car child)
									      (- time-left (cdr child) 1)
									   (cons loc open)))
						       children)))
		       (let ((best-child (max-by-key (cons '(0) children-scores-op))))
			 (cons
			  (+ (* time-left (gethash loc potential)) (car best-child))
			  (cons loc (cdr best-child)))))))))
      (best-score 'AA 30 nil))))

(defun part-2 ()
  (let* ((valves (get-nums))
	 (potential (make-hash-table))
	 (graph (get-graph valves)))
    (loop :for v in valves
	  :do (*let (((name amt _) v))
		(setf (gethash name potential) amt)))
    (labels ((best-score (loc time-left open)
	       (if (<= time-left 0)
		   '(0)
		   (let ((children (remove-if (lambda (x) (find (car x) open))
					      (gethash loc graph))))
		     (let ((children-scores-op (mapcar (lambda (child) (best-score (car child)
									      (- time-left (cdr child) 1)
									   (cons loc open)))
						       children)))
		       (let ((best-child (max-by-key (cons '(0) children-scores-op))))
			 (cons
			  (+ (* time-left (gethash loc potential)) (car best-child))
			  (cons loc (cdr best-child)))))))))
      (let* ((my-score (best-score 'AA 26 nil))
	     (elephant-score (best-score 'AA 26 (cdr my-score))))
	(+ (car my-score) (car elephant-score))))))
