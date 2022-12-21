(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (parse-integer line))))

(defun init-table (lst)
  (let ((tbl (make-hash-table))
	(len (length lst)))
    (labels ((helper (lst idx)
	       (when lst
		 (push idx (gethash (car lst) tbl))
		 (helper (cdr lst) (+ idx len)))))
      (helper lst 0))))

(defun zip-index (lst)
  (loop :for v in lst
	:for i from 0
	:collect (cons v i)))

(defclass circle (t)
  ((val :initarg :val
       :initform (error ":val must be specified")
       :reader val
       :type fixnum)
   (next :initarg :next
	 :initform nil
	 :accessor next)
   (prev :initarg :prev
	 :initform nil
	 :accessor prev)))

(defmethod circle-of-lst (head prev lst)
  (if lst
      (let ((new (make-instance 'circle :val (car lst) :prev prev)))
	(if head
	    (progn
	      (setf (next prev) new)
	      (circle-of-lst head new (cdr lst)))
	    (circle-of-lst new new (cdr lst))))
      (progn (setf (prev head) prev)
	     (setf (next prev) head)
	     head)))

(defmethod list-of-nodes ((c circle))
  (let ((tmp (next c)))
    (cons c
	  (loop :while (not (eql tmp c))
		:collect (progn
			   (setf tmp (next tmp))
			   (prev tmp))))))

(defmethod rlist-of-nodes ((c circle))
  (let ((tmp (prev c)))
    (cons c
	  (loop :while (not (eql tmp c))
		:collect (progn
			   (setf tmp (prev tmp))
			   (next tmp))))))


(defmethod link ((fst circle) (snd circle))
  (setf (next fst) snd)
  (setf (prev snd) fst))

(defmethod move-right ((c circle))
  (let* ((nxt (next c))
	 (2nxt (next nxt))
	 (prv (prev c)))
    (link prv nxt)
    (link nxt c)
    (link c 2nxt)))

(defmethod move-left ((c circle))
  (let* ((prv (prev c))
	 (2prv (prev prv))
	 (nxt (next c)))
    (link 2prv c)
    (link c prv)
    (link prv nxt)))

(defun part-1 ()
  (let* ((circ (circle-of-lst nil nil (get-nums)))
	 (lst (list-of-nodes circ))
	 (len (length lst))
	 (zero (find-if (lambda (nd) (= 0 (val nd))) lst)))
    (labels ((normalize (num)
	       (mod num (1- len))))
      (loop :for nd in lst
	    :do (dotimes (_ (normalize (val nd)))
		  ;;(declare (ignore _))
		  (move-right nd)))
      (let ((nd zero)
	    (acc 0))
	(dotimes (_ 3)
	  (dotimes (_ 1000)
	    (setf nd (next nd)))
	  (incf acc (val nd)))
	acc))))

(defconstant DEC-KEY 811589153)

(defun part-2 ()
  (let* ((circ (circle-of-lst nil nil (mapcar (curry '* DEC-KEY) (get-nums))))
	 (lst (list-of-nodes circ))
	 (len (length lst))
	 (zero (find-if (lambda (nd) (= 0 (val nd))) lst)))
    (labels ((normalize (num)
	       (mod num (1- len))))
      (dotimes (_ 10)
	(loop :for nd in lst
	      :do (dotimes (_ (normalize (val nd)))
		    (move-right nd))))
      (let ((nd zero)
	    (acc 0))
	(dotimes (_ 3)
	  (dotimes (_ 1000)
	    (setf nd (next nd)))
	  (incf acc (val nd)))
	acc))))
