(defun to-num (c)
  (if (lower-case-p c)
      (1+ (- (char-code c) (char-code #\a)))
      (+ 27 (- (char-code c) (char-code #\A)))))

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (mapcar #'to-num (coerce line 'list)))))

(defun part-1 ()
  (reduce #'+
	  (mapcar (lambda (lst)
		    (let ((len (/ (length lst) 2)))
		      (car (intersection (subseq lst 0 len) (subseq lst len)))))
		  (get-nums))))

(defun part-2 ()
  (labels ((helper (lst used-items acc)
	     (if lst
		 (let ((tag (car (intersection
				  (car lst)
				  (intersection (cadr lst) (caddr lst))))))
		   (helper (cdddr lst) (cons tag used-items) (+ acc tag)))
		 acc)))
    (helper (get-nums) NIL 0)))
