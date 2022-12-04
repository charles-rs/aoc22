(ql:quickload "cl-ppcre")

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (mapcar (lambda (pr) (mapcar #'parse-integer
					   (cl-ppcre:split "-" pr)))
			   (cl-ppcre:split "," line)))))

(defun part-1 ()
  (labels ((contains (r1 r2)
	     (and (<= (car r1) (car r2))
		  (>= (cadr r1) (cadr r2))))
	   (contains-count (lst)
	     (if (or (contains (car lst) (cadr lst))
		     (contains (cadr lst) (car lst)))
		 1 0)))
    (reduce #'+ (mapcar #'contains-count (get-nums)))))

(defun part-2 ()
  (labels ((overlap (r1 r2)
	     (if (not (or (> (apply 'min r1) (apply 'max r2))
			  (> (apply 'min r2) (apply 'max r1))))
		 1 0)))
    (reduce #'+ (mapcar (lambda (l) (apply #'overlap l)) (get-nums)))))
