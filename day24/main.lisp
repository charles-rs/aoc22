(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)

(defun parse-char (c)
  (case c
    (#\# 'wall)
    (#\> '(right))
    (#\< '(left))
    (#\^ '(up))
    (#\v '(down))
    (#\. nil)))

(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lst (loop :for line = (read-line stream nil)
		      :while line
		      :collect (coerce line 'list)))
	   (arr (make-array (list (length lst) (length (car lst)))
			    :initial-contents lst))
	   (tbl (make-hash-table :test 'equal)))
      (loop :for i from 0 below (array-dimension arr 0)
	    :do (loop :for j from 0 below (array-dimension arr 1)
		      :do (setf (gethash (cons (1- i) (1- j)) tbl) (parse-char (aref arr i j)))))
      (values tbl (mapcar (rcurry '- 2) (array-dimensions arr))))))



(defun move (loc rows cols dir)
  (case dir
    (up    (if (equal loc (cons 0 0))
	       (cons -1 0)
	       (cons (mod (1- (car loc)) rows) (cdr loc))))
    (down  (if (equal loc (cons (1- rows) (1- cols)))
	       (cons rows (1- cols))
	       (cons (mod (1+ (car loc)) rows) (cdr loc))))
    (right (cons (car loc) (mod (1+ (cdr loc)) cols)))
    (left  (cons (car loc) (mod (1- (cdr loc)) cols)))))


(defun step-tbl (tbl rows cols)
  (let ((new (make-hash-table :test 'equal)))
    (loop :for loc being the hash-keys of tbl
	  :for storms being the hash-values of tbl
	  :do (if (eql storms 'wall)
		  (setf (gethash loc new) 'wall)
		  (mapcar (lambda (dir)
			    (push dir
				  (gethash (move loc rows cols dir) new)))
			  storms)))
    new))

(defun l1-dist (a b)
  (+ (abs (- (car a) (car b)))
     (abs (- (cdr a) (cdr b)))))

(defun print-map (map rows cols)
  (format t "#.")
  (dotimes (_ cols)
    (format t "#"))
  (terpri)
  (loop :for i from 0 below rows
	:do (progn
	      (format t "#")
	      (loop :for j from 0 below cols
		    :do (format t "~a"
				(let ((itm (gethash (cons i j) map)))
				  (cond
				    ((eql itm 'wall) "#")
				    ((null itm) ".")
				    ((= 1 (length itm))
				     (case (car itm)
				       (right ">")
				       (left "<")
				       (up "^")
				       (down "v")))
				    (T (length itm))))))
	      (format t "#~%")))
  (dotimes (_ cols) (format t "#"))
  (format t ".#~%"))

(defun part-1 ()
  (multiple-value-bind (map dims) (get-nums)
    (destructuring-bind (rows cols) dims
      (let ((start (cons -1 0))
	    (end (cons rows (1- cols))))
	(labels
	    ((bfs (steps frontier map)
	       (let ((frontier (remove-if (rcurry 'gethash map) frontier)))
		 (if (member end frontier :test 'equal)
		     steps
		     (let ((new-front
			     (delete-duplicates
			      (loop :for loc in frontier
				    :appending (cons loc (remove-if
							  (lambda (new-loc) (> (l1-dist loc new-loc) 2))
							  (mapcar (curry 'move loc rows cols)
								  '(left right up down)))))
			      :test 'equal)))
		       (bfs (1+ steps) new-front (step-tbl map rows cols)))))))
	  (bfs 0 (list start) map))))))

(defun part-2 ()
  (multiple-value-bind (map dims) (get-nums)
    (destructuring-bind (rows cols) dims
      (let ((start (cons -1 0))
	    (end (cons rows (1- cols))))
	(labels
	    ((bfs (steps frontier map goal)
	       (let ((frontier (remove-if (rcurry 'gethash map) frontier)))
		 (if (member goal frontier :test 'equal)
		     (values steps map)
		     (let ((new-front
			     (delete-duplicates
			      (loop :for loc in frontier
				    :appending (cons loc (remove-if
							  (lambda (new-loc) (> (l1-dist loc new-loc) 2))
							  (mapcar (curry 'move loc rows cols)
								  '(left right up down)))))
			      :test 'equal)))
		       (bfs (1+ steps) new-front (step-tbl map rows cols) goal))))))
	  (multiple-value-bind (steps1 map1) (bfs 0 (list start) map end)
	    (multiple-value-bind (steps2 map2) (bfs steps1 (list end) map1 start)
	      (bfs steps2 (list start) map2 end))))))))
