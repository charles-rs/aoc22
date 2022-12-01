(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lst (loop :for line = (read-line stream nil)
		      :while line
		      :collect (if (> (length line) 0)
				   (parse-integer line)
				   NIL))))
      (loop :with sublist
	    :for el in lst
	    :if el
	      :do (push el sublist)
	    :else
	      :collect (nreverse sublist)
	      :and :do (setf sublist nil)))))

(defun part-1 ()
  (apply #'max (mapcar (lambda (lst) (reduce #'+ lst)) (get-nums))))


(defun part-2 ()
  (labels ((helper (lst maxes)
	     (if lst
		 (if (> (car lst) (car maxes))
		     (helper (cdr lst) (sort (cons (car lst) (cdr maxes)) #'<))
		     (helper (cdr lst) maxes))
		 maxes)))
    (reduce '+ (helper (mapcar (lambda (lst) (reduce (lambda (a b) (+ a b)) lst)) (get-nums)) '(0 0 0)))))
