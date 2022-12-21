(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)

(defun parse-line (line)
    (destructuring-bind (lval rest)
      (cl-ppcre:split ": " line)
    (let ((rest (cl-ppcre:split " " rest))
	  (lval (read-from-string lval)))
      (if (= 1 (length rest))
	  (cons lval (parse-integer (car rest)))
	  (destructuring-bind (l op r)
	      (mapcar 'read-from-string rest)
	    (cons lval
		  (list op l r)))))))

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (parse-line line))))

(defun construct-ast (map root)
  (labels ((helper (symb)
	     (let ((v (gethash symb map)))
	       (if (listp v)
		   (destructuring-bind (op l r) v
		     (list op (helper l) (helper r)))
		   v))))
    (helper root)))

(defun part-1 ()
  (let ((map (alist-hash-table (get-nums))))
    (eval (construct-ast map 'root))))

(defun invert-left (op value)
  (case op
    (+ (rcurry '- value))
    (* (rcurry '/ value))
    (- (lambda (n) (- (- n value))))
    (/ (lambda (n) (/ 1 (/ n value))))))

(defun invert-right (op value)
  (case op
    (+ (rcurry '- value))
    (* (rcurry '/ value))
    (- (curry '+ value))
    (/ (curry '* value))))

(defun balance (expr val)
  (if (eql expr 'hole)
      val
      (destructuring-bind (op left right) expr
	(let ((lval (ignore-errors (eval left))))
	  (if (null lval)
	      (balance left (funcall (invert-right op (eval right)) val))
	      (balance right (funcall (invert-left op (eval left)) val)))))))

(defun part-2 ()
  (let ((map (alist-hash-table (get-nums))))
    (setf (gethash 'humn map) 'hole)
    (*let (((_ left right) (gethash 'root map)))
      (let* ((left (construct-ast map left))
	     (right (construct-ast map right))
	     (lval (ignore-errors (eval  left)))
	     (rval (ignore-errors (eval right))))
	(if lval
	    (balance right lval)
	    (balance left rval))))))
