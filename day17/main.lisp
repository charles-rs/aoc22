(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)


(defun get-nums ()
  (with-open-file (stream "input")
    (mapcar (lambda (x) (if (char= x #\>) 'right 'left)) (coerce (read-line stream nil) 'list))))



(defconstant all-rocks (coerce '(((2 0) (3 0) (4 0) (5 0))
				 ((2 1) (3 0) (3 1) (3 2) (4 1))
				 ((2 0) (3 0) (4 0) (4 1) (4 2))
				 ((2 0) (2 1) (2 2) (2 3))
				 ((2 0) (2 1) (3 0) (3 1)))
			       'vector))

(defun blow-rock (rock dir arr)
  (let ((moved
	  (if (eql dir 'left)
	      (mapcar (lambda (pair) (cons (1- (car pair)) (cdr pair))) rock)
	      (mapcar (lambda (pair) (cons (1+ (car pair)) (cdr pair))) rock))))
    (if (or (find-if (lambda (pair) (or (< (car pair) 0) (> (car pair) 6))) moved)
	    (invalid-rock arr moved))
	rock
	moved)))

(defun drop-rock (rock)
  (mapcar (lambda (p) (list (car p) (1- (cadr p)))) rock))

(defun rock-at-height (rock height)
  (mapcar (lambda (p) (list (car p) (+ height (cadr p)))) rock))

(defun blit-rock (arr rock)
  (apply 'max
	 (loop :for pair in rock
	       :collect (progn (setf (aref arr (car pair) (cadr pair)) t)
			       (cadr pair)))))

(defun invalid-rock (arr rock)
  (loop :for pair in rock
	:if (or (<= (cadr pair) 0)
		(aref arr (car pair) (cadr pair)))
	  :return t))

(defun print-arr (arr max-height)
  (loop :for i from (+ 4 max-height) downto 1
	:do (progn
	      (format t "|")
	      (loop :for j from 0 to 6
		    :do (if (aref arr j i)
			    (format t "#")
			    (format t ".")))
	      (format t "|~%")))
  (format t "+-------+~%"))

(defun part-1 ()
  (let* ((dirs (coerce (get-nums) 'vector))
	 (num-dirs (array-dimension dirs 0))
	 (arr (make-array '(7 5000) :initial-element nil))
	 (rock 0)
	 (dir 0)
	 (max-height 0))
    (loop :for i from 0 to 6
	  :do (setf (aref arr i 0) t))
    (loop :for i from 1 to 2022
	  :do (let ((cur-rock (rock-at-height (aref all-rocks rock) (+ max-height 4)))
		    (old-rock nil))
		(loop :while (not (invalid-rock arr cur-rock))
		      :do (let ((cur-dir (aref dirs dir)))
			    (setf cur-rock (blow-rock cur-rock cur-dir arr))
			    (setf old-rock cur-rock)
			    (setf dir (mod (1+ dir) num-dirs))
			    (setf cur-rock (drop-rock cur-rock))))
		(maxf max-height (blit-rock arr old-rock))
		(setf rock (mod (1+ rock) 5))))
    max-height))



(defun part-2 ()
  (let* ((dirs (coerce (get-nums) 'vector))
	 (num-dirs (array-dimension dirs 0))
	 (arr (make-array '(7 10000) :initial-element nil))
	 (rock 0)
	 (dir 0)
	 (max-height 0)
	 (prev-height 0)
	 (prev 0)
	 (y-offset 0)
	 (delta-rocks -1)
	 (delta-height 0)
	 (trillion 1000000000000))
    (loop :for i from 0 to 6
	  :do (setf (aref arr i 0) t))
    (loop :for i from 1 to trillion
	  :do (let ((cur-rock (rock-at-height (aref all-rocks rock) (+ max-height 4)))
		    (old-rock nil))
		(loop :while (not (invalid-rock arr cur-rock))
		      :do (let ((cur-dir (aref dirs dir)))
			    (when (and t (= rock 1) (= dir 0))
			      (when (= delta-rocks (- i prev))
				(let* ((rocks-left (- trillion i))
				       (chunks (floor rocks-left delta-rocks)))
				  (setf y-offset (* chunks delta-height))
				  (incf i (* chunks delta-rocks))))

			      (setf delta-rocks (- i prev))
			      (setf delta-height (- max-height prev-height))
			      (setf prev i)
			      (setf prev-height max-height))
			    (setf cur-rock (blow-rock cur-rock cur-dir arr))
			    (setf old-rock cur-rock)
			    (setf dir (mod (1+ dir) num-dirs))
			    (setf cur-rock (drop-rock cur-rock))))
		(maxf max-height (blit-rock arr old-rock))
		(setf rock (mod (1+ rock) 5))))
    (+ y-offset max-height)))
