(ql:quickload '(:cl-ppcre :alexandria))
(use-package :alexandria)

(defun get-nums ()
  (labels ((parse-line (str)
	     (let ((split (cl-ppcre:split " " str)))
	       (if (cdr split)
		   (list (read-from-string (car split)) (parse-integer (cadr split)))
		   (cons (read-from-string (car split)) nil)))))
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (parse-line line)))))

(defun part-1 ()
  (let ((insns (get-nums)))
    (labels ((interpret (acc insns cyclecount register important-cycles)
	       (if (and important-cycles insns)
		   (multiple-value-bind (acc important-cycles)
		       (if (= cyclecount (car important-cycles))
			   (values (+ acc (* register cyclecount))
				   (cdr important-cycles))
			   (values acc important-cycles))
		     (destructuring-case (car insns)
		       ((noop)
			(interpret acc (cdr insns) (1+ cyclecount)
				   register important-cycles))
		       ((addx amt)
			(interpret acc (cons `(addy ,amt) (cdr insns)) (1+ cyclecount)
				   register important-cycles))
		       ((addy amt)
			(interpret acc (cdr insns) (1+ cyclecount)
				   (+ register amt) important-cycles))))
		   acc)))
      (interpret 0 insns 1 1 '(20 60 100 140 180 220)))))


(defun part-2 ()
  (let ((insns (get-nums)))
    (labels ((interpret (insns cyclecount register)
	       (when insns
		 (when (= 1 (mod cyclecount 40))
		   (format t "~%"))
		 (format t "~a" (if (<= (abs (- (mod (1- cyclecount) 40) register)) 1)
				    #\Full_block
				    #\.))
		 (destructuring-case (car insns)
		   ((noop)
		    (interpret (cdr insns) (1+ cyclecount) register))
		   ((addx amt)
		    (interpret (cons `(addy ,amt) (cdr insns))
			       (1+ cyclecount) register))
		   ((addy amt)
		    (interpret (cdr insns) (1+ cyclecount) (+ register amt)))))))
      (interpret insns 1 1))))
