(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (mapcar (compose 'read-from-string 'string) (coerce line 'list)))))

(defun snafu-digit-val (digit)
  (case digit
    (- -1)
    (= -2)
    (T digit)))

(defun snafu-to-dec (snafu)
  (labels ((helper (acc snafu)
	     (if snafu
		 (helper (+ (snafu-digit-val (car snafu))
			    (* acc 5))
			 (cdr snafu))
		 acc)))
    (helper 0 snafu)))

(defun dec-to-snafu (num)
  (let ((base-5 (let ((*print-base* 5))
		  (mapcar (compose 'read-from-string 'string)
			  (coerce (format nil "~a" num) 'list)))))
    (labels ((base5-to-snafu (acc cin reversed)
	       (if reversed
		   (let* ((value (+ cin (car reversed)))
			  (cout (if (> value 2) 1 0))
			  (digit (case value
				   (3 '=)
				   (4 '-)
				   (5 0)
				   (T value))))
		     (base5-to-snafu (cons digit acc) cout (cdr reversed)))
		   (if (= cin 0) acc (cons cin acc)))))
      (base5-to-snafu nil 0 (reverse base-5)))))

(defun part-1 ()
  (let ((snafus (get-nums)))
    (mapc (lambda (c) (format t "~a" c))
	  (dec-to-snafu
	   (apply '+ (mapcar 'snafu-to-dec snafus))))
    (terpri)))
