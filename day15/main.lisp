(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria :defstar)
(use-package :split-sequence)

(defun parse-line (line)
  (*let (((_ _ sx sy _ _ _ _ bx by) (cl-ppcre:split " " line)))
    (mapcar (compose (rcurry 'parse-integer :junk-allowed t)
		      'cadr
		      (curry 'cl-ppcre:split "="))
	     (list sx sy bx by))))

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (parse-line line))))


(defun dist (p1 p2)
  (reduce '+ (mapcar (compose 'abs '-) p1 p2)))

(defun dist-lst (l)
  (destructuring-bind (a b c d) l
    (dist (list a b) (list c d))))

(defun overlap (i1 i2)
  (and (<= (car i1) (cadr i2))
       (<= (car i2) (cadr i1))))

(defun iunion (i1 i2)
  (list (min (car i1) (car i2))
	(max (cadr i1) (cadr i2))))

(defun join-to-list (acc lst ival)
  (if lst
      (if (overlap (car lst) ival)
	  (join-to-list acc (cdr lst) (iunion ival (car lst)))
	  (join-to-list (cons (car lst) acc) (cdr lst) ival))
      (cons ival acc)))

(defun reduce-intervals (acc intervals)
  (if intervals
      (reduce-intervals (join-to-list nil acc (car intervals))
			(cdr intervals))
      acc))

(defun part-1 ()
  (let ((nums (get-nums))
	(y 2000000))
    (reduce '+
	    (mapcar (lambda (lst) (- (cadr lst) (car lst)))
		    (reduce-intervals
		     nil
		     (remove-if-not
		      (lambda (x) x)
		      (loop :for lst in nums
			    :collect (destructuring-bind (sx sy bx by) lst
				       (let* ((d (dist (list sx sy) (list bx by)))
					      (to-y (abs (- sy y)))
					      (rad (- d to-y)))
					 (if (> rad 0)
					     (list (- sx rad) (+ sx rad))
					     nil))))))))))

(defun to-pointed-dist (l)
  (let ((pt (list (car l) (cadr l)))
	(d (dist-lst l)))
    (list pt d)))

(defun one-between (p1 p2)
  (destructuring-bind ((p1 d1) (p2 d2)) (list p1 p2)
    (let ((d (dist p1 p2)))
      (= d (+ 2 d1 d2)))))

(defun closer (a b dest)
  (if (< (abs (- a dest))
	 (abs (- b dest)))
      a b))

(defun corners (pt)
  (destructuring-bind ((x y) d) pt
    (list (list (+ x d 1) y)
	  (list (- x d 1) y)
	  (list x (+ y d 1))
	  (list x (- y d 1)))))

(defun pt-add (p1 p2)
  (mapcar '+ p1 p2))

(defun points-between (p1 p2)
  (let* ((d1 (cadr p1))
	 (d2 (cadr p2))
	 (all-corners (append
		       (remove-if-not (lambda (corner) (= (dist corner (car p2)) (1+ d2)))
				      (corners p1))
		       (remove-if-not (lambda (corner) (= (dist corner (car p1)) (1+ d1)))
				      (corners p2))))
	 (corner1 (car all-corners))
	 (corner2 (cadr all-corners))
	 (dir (mapcar (compose 'signum '-) corner2 corner1)))
    (cons corner2
	  (loop :while (not (equal corner1 corner2))
		:collect (let ((tmp corner1))
			   (setf corner1 (pt-add corner1 dir))
			   tmp)))))

(defun part-2 ()
  (let* ((nums (mapcar 'to-pointed-dist (get-nums)))
	 (diamonds
	   (delete-duplicates
	    (mapcar (lambda (pair) (sort pair '< :key 'cadr))
		    (remove-if-not (lambda (c) (one-between (car c) (cadr c)))
				   (map-product 'list nums nums)))
	    :test 'equal))
	 (points (make-hash-table :test 'equal)))
    (destructuring-bind (pair1 pair2) diamonds
      (mapc (lambda (pt) (setf (gethash pt points) t)) (points-between (car pair1) (cadr pair1)))
      (let ((position
	      (loop :for pt in (points-between (car pair2) (cadr pair2))
		    :if (gethash pt points)
		      :return pt)))
	(+ (* (car position) 4000000) (cadr position))))))
