(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)


(defun repeat (val n)
  (loop :for i from 1 to n :collect val))


(defun read-from-char (c) (read-from-string (string c)))

(defun parse-insns (lst)
  (labels ((get-number (acc lst)
	     (let ((rd (read-from-char (car lst))))
	       (if (not (integerp rd))
		   (values acc lst)
		   (get-number (+ (* acc 10) rd) (cdr lst)))))
	   (helper (acc lst)
	     (if lst
		 (let ((rd (read-from-char (car lst))))
		   (if (integerp rd)
		       (multiple-value-bind (num rest) (get-number rd (cdr lst))
			 (helper (cons num acc) rest))
		       (helper (cons rd acc) (cdr lst))))
		 (reverse acc))))
    (helper nil lst)))

(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lst (loop :for line = (read-line stream nil)
		      :while (not (= 0 (length line)))
		      :collect (mapcar (lambda (c) (case c
						(#\Space 'empty)
						(#\# 'wall)
						(#\. 'air)))
				       (coerce line 'list))))
	   (width (apply 'max (mapcar 'length lst)))
	   (rows (make-array (length lst) :initial-element nil))
	   (cols (make-array width :initial-element nil))
	   (extended (mapcar (lambda (row) (append row (repeat 'empty (- width (length row))))) lst))
	   (arr (make-array (list (length lst) width) :initial-contents extended))
	   (columns (loop :for col from 0 below width
			  :collect (loop :for row from 0 below (length lst)
					 :collect (aref arr row col)))))

      (loop :for row in lst
	    :for i from 0
	    :do (let ((len (length row))
		      (rest (length (member-if-not (curry 'eql 'empty) row))))
		  (setf (aref rows i) (cons (- len rest) (1- len)))))

      (loop :for col in columns
	    :for i from 0
	    :do (let* ((len (length col))
		       (middle (member-if-not (curry 'eql 'empty) col))
		       (ending (length
				(member-if (curry 'eql 'empty) middle)))
		       (mid-len (length middle)))
		  (setf (aref cols i) (cons (- len mid-len) (- len ending 1)))))

      (let ((start (loop :for c in (car lst)
			 :for j from 0
			 :if (not (eql c 'empty))
			   :return (cons 0 j))))
	(values start arr rows cols
		(parse-insns (coerce (read-line stream nil) 'list)))))))


(defun inc-in-range (v range)
  (if (> (1+ v) (cdr range))
      (car range)
      (1+ v)))

(defun dec-in-range (v range)
  (if (< (1- v) (car range))
      (cdr range)
      (1- v)))

(defun move (dir pos rows cols)
  (destructuring-bind (r . c) pos
    (case dir
      (right (cons r (inc-in-range c (aref rows r))))
      (left (cons r (dec-in-range c (aref rows r))))
      (up (cons (dec-in-range r (aref cols c)) c))
      (down (cons (inc-in-range r (aref cols c)) c)))))


(defun turn (dir amt)
  (case dir
    (right (if (eql amt 'L) 'up 'down))
    (down (if (eql amt 'L) 'right 'left))
    (left (if (eql amt 'L) 'down 'up))
    (up (if (eql amt 'L) 'left 'right))))

(defun dec-top (lst)
  (cons (1- (car lst)) (cdr lst)))

(defun dir-score (dir)
  (case dir
    (right 0)
    (down 1)
    (left 2)
    (up 3)))

(defun part-1 ()
  (multiple-value-bind (start map rows cols insns) (get-nums)
    (labels
	((at (pos)
	   (aref map (car pos) (cdr pos)))
	 (move-checked (dir pos)
	   (let ((mvd (move dir pos rows cols)))
	     (if (eql (at mvd) 'wall) pos mvd)))
	 (interpret (insns dir pos)
	   (if insns
	       (if (numberp (car insns))
		   (if (= (car insns) 0)
		       (interpret (cdr insns) dir pos)
		       (interpret (dec-top insns) dir (move-checked dir pos)))
		   (interpret (cdr insns) (turn dir (car insns)) pos))
	       (+ (* 1000 (1+ (car pos))) (* 4 (1+ (cdr pos))) (dir-score dir)))))
      (interpret insns 'right start))))

(Defun rev-dir (dir)
  (case dir
    (left 'right)
    (right 'left)
    (up 'down)
    (down 'up)))

(defun part-2 ()
  (let ((cube-names '(((0 . 50) . A) ((0 . 100) . B)
		      ((50 . 50) . C) ((100 . 50) . D)
		      ((100 . 0) . E) ((150 . 0) . F))))
    (labels ((wrap-left (cube row dir)
	       (case cube
		 (a (list 'e (cons (- 49 row) 0) (rev-dir dir)))
		 (b (list 'a (cons row 49) dir))
		 (c (list 'e (cons 0 row) (turn dir 'L)))
		 (d (list 'e (cons row 49) dir))
		 (e (list 'a (cons (- 49 row) 0) (rev-dir dir)))
		 (f (list 'a (cons 0 row) (turn dir 'L)))))
	     (wrap-right (cube row dir)
	       (case cube
		 (a (list 'b (cons row 0) dir))
		 (b (list 'd (cons (- 49 row) 49) (rev-dir dir)))
		 (c (list 'b (cons 49 row) (turn dir 'L)))
		 (d (list 'b (cons (- 49 row) 49) (rev-dir dir)))
		 (e (list 'd (cons row 0) dir))
		 (f (list 'd (cons 49 row) (turn dir 'L)))))
	     (wrap-up (cube col dir)
	       (case cube
		 (a (list 'f (cons col 0) (turn dir 'R)))
		 (b (list 'f (cons 49 col) dir))
		 (c (list 'a (cons 49 col) dir))
		 (d (list 'c (cons 49 col) dir))
		 (e (list 'c (cons col 0) (turn dir 'R)))
		 (f (list 'e (cons 49 col) dir))))
	     (wrap-down (cube col dir)
	       (case cube
		 (a (list 'c (cons 0 col) dir))
		 (b (list 'c (cons col 49) (turn dir 'R)))
		 (c (list 'd (cons 0 col) dir))
		 (d (list 'f (cons col 49) (turn dir 'R)))
		 (e (list 'f (cons 0 col) dir))
		 (f (list 'b (cons 0 col) dir)))))
      (multiple-value-bind (start map rows cols insns) (get-nums)
	(declare (ignore rows cols start))
	(let  ((all-cubes
		 (alist-hash-table
		  (mapcar
		   (lambda (a)
		     (destructuring-bind (pos . name) a
		       (let ((a (make-array '(50 50))))
			 (loop :for i from 0 below 50
			       :do (loop :for j from 0 below 50
					 :do (setf (aref a i j)
						   (aref map (+ i (car pos))
							 (+ j (cdr pos))))))
			 (cons name a))))
		   cube-names))))
	  (labels
	      ((move-checked (cube pos dir do-check)
		 (destructuring-bind (row . col) pos
		   (destructuring-bind (mcube mpos mdir)
		       (case dir
			 (right (if (>= (1+ col) 50)
				    (wrap-right cube row dir)
				    (list cube (cons row (1+ col)) dir)))
			 (left (if (< (1- col) 0)
				   (wrap-left cube row dir)
				   (list cube (cons row (1- col)) dir)))
			 (up (if (< (1- row) 0)
				 (wrap-up cube col dir)
				 (list cube (cons (1- row) col) dir)))
			 (down (if (>= (1+ row) 50)
				   (wrap-down cube col dir)
				   (list cube (cons (1+ row) col) dir))))
		     (when do-check
			   ;;;this is smart debugging it gets to stay
		       (assert (equal (list cube pos (rev-dir dir))
				      (move-checked mcube mpos (rev-dir mdir) nil))))
		     (if (eql 'wall (aref (gethash mcube all-cubes) (car mpos) (cdr mpos)))
			 (list cube pos dir)
			 (list mcube mpos mdir)))))
	       (score (cube pos dir)
		 (destructuring-bind ((row . col) . _) (rassoc cube cube-names)
		   (declare (ignore _))
		   (let ((row (+ row (car pos)))
			 (col (+ col (cdr pos))))
		     (+ (* 1000 (1+ row)) (* 4 (1+ col)) (dir-score dir)))))
	       (interpret (insns cube pos dir)
		 (if insns
		     (if (numberp (car insns))
			 (if (= (car insns) 0)
			     (interpret (cdr insns) cube pos dir)
			     (destructuring-bind (cube pos dir)
				 (move-checked cube pos dir t)
			       (interpret (dec-top insns) cube pos dir)))
			 (interpret (cdr insns) cube pos (turn dir (car insns))))
		     (score cube pos dir))))
	    (interpret insns 'a '(0 . 0) 'right)))))))




