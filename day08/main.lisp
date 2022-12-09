(defun get-nums ()
  (with-open-file (stream "input")
    (let ((lst (loop :for line = (read-line stream nil)
		     :while line
		     :collect (mapcar (lambda (char) (- (char-code char) (char-code #\0)))
				      (coerce line 'list)))))
      (make-array (list (length lst) (length (car lst)))
		  :initial-contents lst))))

(defmacro test-and-set (test location tracker setee)
  `(when (,test ,location ,tracker)
     (setf ,tracker ,location)
     (setf ,setee t)))

(defun part-1 ()
  (let* ((array (get-nums))
	 (len (array-dimension array 0))
	 (vt (make-array `(,len ,len) :initial-element nil)))
    (loop :for i from 0 below len
	    :do (let ((ml -1) (mr -1) (mt -1) (mb -1))
		  (loop :for j from 0 below len
			:do (progn
			      (test-and-set > (aref array i j) ml
					    (aref vt i j))
			      (test-and-set > (aref array i (- len j 1)) mr
					    (aref vt i (- len j 1)))
			      (test-and-set > (aref array j i) mt
					    (aref vt j i))
			      (test-and-set > (aref array (- len j 1) i) mb
					    (aref vt (- len j 1) i))))))
    (loop :for i from 0 below len
	  :summing (loop :for j from 0 below len
			 :summing (if (aref vt i j) 1 0)))))

(defmacro timesf (loc val)
  `(setf ,loc (* ,loc ,val)))

(defun part-2 ()
  (let* ((trees (get-nums))
	 (len (array-dimension trees 0))
	 (scores (make-array `(,len len) :initial-element 1)))
    (loop :for i from 0 below len
	  :do (let ((left-view (make-array 10 :initial-element 0))
		    (top-view (make-array 10 :initial-element 0))
		    (right-view (make-array 10 :initial-element (1- len)))
		    (bot-view (make-array 10 :initial-element (1- len))))
		(setf (aref scores i 0) 0)
		(setf (aref scores i (1- len)) 0)
		(setf (aref scores 0 i) 0)
		(setf (aref scores (1- len) i) 0)
		(loop :for j from 1 below len
		      :do (progn
			    (timesf (aref scores i j) (- j (aref left-view (aref trees i j))))
			    (loop :for k from 0 to (aref trees i j)
				  :do (setf (aref left-view k) j))
			    (timesf (aref scores j i) (- j (aref top-view (aref trees j i))))
			    (loop :for k from 0 to (aref trees j i)
				  :do (setf (aref top-view k) j))))
		(loop :for j from (- len 2) downto 0
		      :do  (progn
			     (timesf (aref scores i j) (- (aref right-view (aref trees i j)) j))
			     (loop :for k from 0 to (aref trees i j)
				   :do (setf (aref right-view k) j))
			     (timesf (aref scores j i) (- (aref bot-view (aref trees j i)) j))
			     (loop :for k from 0 to (aref trees j i)
				   :do (setf (aref bot-view k) j))))))
    (let ((max 0))
      (loop :for i from 1 below (1- len)
	    :do (loop :for j from 1 below (1- len)
		      :if (> (aref scores i j) max)
			:do (setf max (aref scores i j))))
    max)))
