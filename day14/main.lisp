(ql:quickload '(:split-sequence :alexandria :cl-ppcre))
(use-package :alexandria)
(use-package :split-sequence)

(defun str-rep (old new str)
  (cl-ppcre:regex-replace-all old str new))

(defun read-pair (str)
  (destructuring-bind (left right) (cl-ppcre:split "," str)
    (cons (parse-integer left) (parse-integer right))))

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (mapcar 'read-pair
			   (cl-ppcre:split " -> " line)))))

(defun construct-map ()
  (let ((maps (get-nums))
	(tbl (make-hash-table :test 'equal))
	(lowest 0))
    (labels ((add-path (from rest)
	       (when rest
		 (destructuring-bind ((fromx . fromy) . (tox . toy))
		     (cons from (car rest))
		   (maxf lowest fromy toy)
		   (if (= fromx tox)
		       (loop :for i from (min fromy toy) to (max fromy toy)
			     :do (setf (gethash (cons fromx i) tbl) 'rock))
		       (loop :for i from (min fromx tox) to (max fromx tox)
			     :do (setf (gethash (cons i fromy) tbl) 'rock))))
		 (add-path (car rest) (cdr rest)))))
      (mapc (lambda (path) (add-path (car path) (cdr path))) maps)
      (values tbl lowest))))

(defun dbg-print (tbl left right top bottom)
  (loop :for y from top to bottom
	:do (progn
	      (format t "~%")
	      (loop :for x from left to right
		    :do (format t "~a" (case (gethash (cons x y) tbl)
					 (rock #\#)
					 (sand #\o)
					 (T #\.)))))))

(defun part-1 ()
  (multiple-value-bind (map lowest) (construct-map)
    (labels ((next (pos)
	       (destructuring-bind (x . y) pos
		 (let ((poses (list (cons x (1+ y)) (cons (1- x) (1+ y)) (cons (1+ x) (1+ y)))))
		   (find-if (compose 'null (rcurry 'gethash map)) poses))))
	     (percolate (pos)
	       (let ((n (next pos)))
		 (if n
		     (if (> (cdr n) lowest)
			 'off-board
			 (percolate n))
		     pos)))
	     (add-sand ()
	       (let ((sand-pos (percolate (cons 500  0))))
		 (when (not (eql sand-pos 'off-board))
		   (setf (gethash sand-pos map) 'sand)
		   (add-sand)))))
      (add-sand)
      (loop :for v being the hash-values of map
	    :if (eql v 'sand)
	      :summing 1))))



(defun part-2 ()
  (multiple-value-bind (map lowest) (construct-map)
    (labels ((next (pos)
	       (destructuring-bind (x . y) pos
		 (let ((poses (list (cons x (1+ y)) (cons (1- x) (1+ y)) (cons (1+ x) (1+ y)))))
		   (find-if (lambda (pos)
			      (and (null (gethash pos map))
				   (/= (cdr pos) (+ 2 lowest))))
			    poses))))
	     (percolate (pos)
	       (let ((n (next pos)))
		 (if n
		     (percolate n)
		     pos)))
	     (add-sand ()
	       (let ((sand-pos (percolate (cons 500  0))))
		 (when (not (= (cdr sand-pos) 0))
		   (setf (gethash sand-pos map) 'sand)
		   (add-sand)))))
      (setf (gethash (cons 500 0) map) 'sand)
      (add-sand)
      (loop :for v being the hash-values of map
	    :if (eql v 'sand)
	      :summing 1))))

