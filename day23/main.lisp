(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)

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
		      :if (char= (aref arr i j) #\#)
			:do (setf (gethash (cons i j) tbl) t)))
      tbl)))


(defun coords (start dir)
  (case dir
    (west (list (cons (car start) (1- (cdr start)))
		 (cons (1+ (car start)) (1- (cdr start)))
		 (cons (1- (car start)) (1- (cdr start)))))
    (east (list (cons (car start) (1+ (cdr start)))
		 (cons (1+ (car start)) (1+ (cdr start)))
		 (cons (1- (car start)) (1+ (cdr start)))))
    (south (list (cons (1+ (car start)) (cdr start))
		(cons (1+ (car start)) (1- (cdr start)))
		(cons (1+ (car start)) (1+ (cdr start)))))
    (north (list (cons (1- (car start)) (cdr start))
		(cons (1- (car start)) (1- (cdr start)))
		(cons (1- (car start)) (1+ (cdr start)))))))


(defun all-neighbors (loc)
  (remove loc
	  (map-product (lambda (x y) (cons (+ x (car loc)) (+ y (cdr loc))))
		       '(-1 0 1)
		       '(-1 0 1))
	  :test 'equal))


;; amazing debugging when my issue was finding the area of a rectangle
(defun print-table (tbl)
  (let* ((rows (mapcar 'car (hash-table-keys tbl)))
	 (cols (mapcar 'cdr (hash-table-keys tbl)))
	 (left (apply 'min cols))
	 (right (apply 'max cols))
	 (bottom (apply 'max rows))
	 (top (apply 'min rows)))
    (loop :for i from (1- top) to (1+ bottom)
	  :do (progn
		(loop :for j from (1- left) to (1+ right)
		      :do (format t "~a" (if (gethash (cons i j) tbl) #\# #\.)))
		(format t "~%")))))

(defun part-1 ()
  (let ((tbl (get-nums))
	(changed t))
    (labels ((get-checks (step)
	       (rotate (copy-list '(north south west east)) (- step))))
      (loop :while changed
	    :for step from 0 to 10
	    :do (let ((tmp-tbl (make-hash-table :test 'equal))
		      (next-tbl (make-hash-table :test 'equal))
		      (order (get-checks step)))
		  (setf changed nil)
		  (loop :for elf being the hash-keys of tbl
			:do (let ((target
				    (find-if (lambda (locs)
					       (not (find-if (lambda (loc) (gethash loc tbl)) locs)))
					     (mapcar (curry 'coords elf) order))))
			      (when (not (find-if (lambda (loc) (gethash loc tbl))
						  (all-neighbors elf)))
				(setf target nil))
			      (if target
				  (let ((target (car target)))
				    (setf (gethash elf tbl) target)
				    (if (gethash target tmp-tbl)
					(setf (Gethash target tmp-tbl) 'forbidden)
					(setf (gethash target tmp-tbl) t)))
				  (progn
				    (setf (Gethash elf tbl) elf)
				    (setf (Gethash elf tmp-tbl) t)))))
		  (loop :for elf being the hash-keys of tbl
			:for target being the hash-values of tbl
			:do (if (and (not (equal elf target))
				     (not (eql (gethash target tmp-tbl) 'forbidden)))
				(progn
				  (setf changed t)
				  (setf (gethash target next-tbl) t))
				(setf (gethash elf next-tbl) t)))
		  (setf tbl next-tbl)))
      (let* ((final-pos (hash-table-keys tbl))
	     (rows (mapcar 'car final-pos))
	     (cols (mapcar 'cdr final-pos)))
	(- (* (1+ (- (apply 'max rows) (apply 'min rows)))
	      (1+ (- (apply 'max cols) (apply 'min cols))))
	   (hash-table-count tbl))))))

;; bit of a copy paste and hacky since i was so ready for the question
(defun part-2 ()
  (let ((tbl (get-nums))
	(changed t)
	(res nil))
    (labels ((get-checks (step)
	       (rotate (copy-list '(north south west east)) (- step))))
      (loop :while changed
	    :for step from 0
	    :do (let ((tmp-tbl (make-hash-table :test 'equal))
		      (next-tbl (make-hash-table :test 'equal))
		      (order (get-checks step)))
		  (setf changed nil)
		  (setf res step)
		  (loop :for elf being the hash-keys of tbl
			:do (let ((target
				    (find-if (lambda (locs)
					       (not (find-if (lambda (loc) (gethash loc tbl)) locs)))
					     (mapcar (curry 'coords elf) order))))
			      (when (not (find-if (lambda (loc) (gethash loc tbl))
						  (all-neighbors elf)))
				(setf target nil))
			      (if target
				  (let ((target (car target)))
				    (setf (gethash elf tbl) target)
				    (if (gethash target tmp-tbl)
					(setf (Gethash target tmp-tbl) 'forbidden)
					(setf (gethash target tmp-tbl) t)))
				  (progn
				    (setf (Gethash elf tbl) elf)
				    (setf (Gethash elf tmp-tbl) t)))))
		  (loop :for elf being the hash-keys of tbl
			:for target being the hash-values of tbl
			:do (if (and (not (equal elf target))
				     (not (eql (gethash target tmp-tbl) 'forbidden)))
				(progn
				  (setf changed t)
				  (setf (gethash target next-tbl) t))
				(setf (gethash elf next-tbl) t)))
		  (setf tbl next-tbl)))
      (1+ res))))
