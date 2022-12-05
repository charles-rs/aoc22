(ql:quickload "cl-ppcre")

(defun translate-stacks (raw num-lines)
  (let ((arr (make-array `(,num-lines) :initial-element nil)))
    (labels ((helper (inpt)
	       (if inpt
		   (progn
		     (loop :for i from 0 below num-lines
			   :do (let ((c (aref (car inpt) (+ (* i 4) 1))))
				 (when (char-not-equal c #\Space)
				   (push c (aref arr i)))))
		     (helper (cdr inpt)))
		   arr)))
      (helper raw))))

(defun translate-insn (insn)
  (remove-if-not (lambda (x) x)
		 (mapcar (lambda (x) (parse-integer x :junk-allowed t))
			 (cl-ppcre:split " " insn))))

(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((prestacks
	     (reverse
	      (loop :for line = (read-line stream nil)
		    :while (> (length line) 0)
		    :collect line)))
	   (num-lines (length (cl-ppcre:split "\\s*" (string-trim " " (car prestacks)))))
	   (insns
	     (loop :for line = (read-line stream nil)
		   :while line
		   :collect line)))
      (values (translate-stacks (cdr prestacks) num-lines)
	      (mapcar #'translate-insn insns)))))

(defun part-1 ()
  (multiple-value-bind (stacks insns) (get-nums)
    (labels ((move-crate (amt src dest)
	       (when (> amt 0)
		 (let ((crate (car (aref stacks src))))
		   (setf (aref stacks src) (cdr (aref stacks src)))
		   (push crate (aref stacks dest))
		   (move-crate (1- amt) src dest))))
	     (helper (insns)
	       (when insns
		 (destructuring-bind (amount src dest) (car insns)
		   (move-crate amount (1- src) (1- dest))
		   (helper (cdr insns))))))
      (helper insns)
      (coerce (loop :for l across stacks
		    :collect (car l))
	      'string))))

(defun part-2 ()
  (multiple-value-bind (stacks insns) (get-nums)
    (labels ((move-crate (amt src dest)
	       (mapcar
		(lambda (crate) (push crate (aref stacks dest)))
		(reverse
		 (loop for i from 1 to amt
		       :collect
		       (let ((crate (car (aref stacks src))))
			 (setf (aref stacks src) (cdr (aref stacks src)))
			 crate)))))
	     (helper (insns)
	       (when insns
		 (destructuring-bind (amount src dest) (car insns)
		   (move-crate amount (1- src) (1- dest))
		   (helper (cdr insns))))))
      (helper insns)
      (coerce (loop :for l across stacks
		    :collect (car l))
	      'string))))

