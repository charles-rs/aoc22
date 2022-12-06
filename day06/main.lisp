(defun get-nums ()
  (with-open-file (stream "input")
    (coerce (read-line stream nil) 'list)))

(defun dup-p (lst)
  (let ((mp (make-hash-table :test 'equal))
	(dup nil))
    (loop for i in lst
	  :do (if (gethash i mp)
		  (setf dup t)
		  (setf (gethash i mp) t)))
    dup))

(defun helper (chars recent count)
  (if (not (dup-p recent))
      count
      (helper (cdr chars)
	      (reverse (cons (car chars) (reverse (cdr recent))))
	      (1+ count))))

(defun split-list (lst n)
  (labels ((helper (acc lst n)
	     (if (= 0 n)
		 (values (reverse acc) lst)
		 (helper (cons (car lst) acc) (cdr lst) (1- n)))))
    (helper nil lst n)))

(defun find-start-token (tok-len)
  (let ((chars (get-nums)))
    (multiple-value-bind (recent chars) (split-list chars tok-len)
      (helper chars recent tok-len))))

(defun part-1 () (find-start-token 4))

(defun part-2 () (find-start-token 14))
