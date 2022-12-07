(ql:quickload "cl-ppcre")
(ql:quickload "alexandria")
(ql:quickload "cl-algebraic-data-type")

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (cl-ppcre::split " " line))))

(adt:defdata node
  (file string fixnum)
  (dir string))


(defun collect-directories (commands)
  (let ((dirs (make-hash-table :test 'equal)))
    (labels ((handle-ls (commands cur-dir)
	       (when commands
		 (let ((line (car commands)))
		   (alexandria:switch ((car line) :test 'string=)
		     ("$" commands)
		     ("dir"
		      (push (dir (cadr line)) (gethash cur-dir dirs))
		      (handle-ls (cdr commands) cur-dir))
		     (otherwise
		      (push (file (cadr line) (parse-integer (car line)))
			    (gethash cur-dir dirs))
		      (handle-ls (cdr commands) cur-dir))))))
	     (handle-commands (commands directories)
	       (let ((cmd (car commands)))
		 (when commands
		   (if (string= "$" (car cmd))
		       (if (string= "ls" (cadr cmd))
			   (handle-commands
			    (handle-ls (cdr commands) (car directories))
			    directories)
			   (alexandria:switch ((caddr cmd) :test 'string=)
			     ("/" (handle-commands (cdr commands) '("/")))
			     (".." (handle-commands (cdr commands) (cdr directories)))
			     (otherwise (handle-commands
					 (cdr commands)
					 (cons (concatenate 'string (car directories) "/" (caddr cmd))
					       directories))))))))))
      (handle-commands commands nil)
      dirs)))

(defun get-sizes (dirs)
  (let ((sizes (make-hash-table :test 'equal)))
    (labels ((get-dir-size (dir)
	       (when (not (gethash dir sizes))
		 (setf (gethash dir sizes)
		       (loop :for child in (gethash dir dirs)
			     :summing (adt:match node child
					((file _ size) size)
					((dir name) (get-dir-size (concatenate 'string dir "/" name)))))))
	       (gethash dir sizes)))
      (get-dir-size "/")
      sizes)))

(defun hash-values (ht)
  (loop :for v being the hash-values of ht :collect v))

(defun part-1 ()
  (let* ((commands (get-nums))
	 (dirs (collect-directories commands))
	 (sizes (hash-values (get-sizes dirs))))
    (apply #'+ (remove-if (lambda (sz) (> sz 100000)) sizes))))


(defun part-2 ()
  (let* ((commands (get-nums))
	 (dirs (collect-directories commands))
	 (sizes (get-sizes dirs))
	 (space-needed (- 30000000 (- 70000000 (gethash "/" sizes)))))
      (apply 'min (remove-if (lambda (size) (< size space-needed)) (hash-values sizes)))))

