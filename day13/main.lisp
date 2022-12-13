(ql:quickload '(:split-sequence :alexandria :cl-ppcre))
(use-package :alexandria)
(use-package :split-sequence)


(defun str-rep (old new str)
  (cl-ppcre:regex-replace-all old str new))

(defun my-read (str)
  (if (string= str "")
      'break
      (read-from-string str)))

(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lines
	     (loop :for line = (read-line stream nil)
		   :while line
		   :collect (funcall
			     (compose 'my-read
				      (curry 'str-rep "," " ")
				      (curry 'str-rep "\\[" "(")
				      (curry 'str-rep "\\]" ")"))
			     line))))
      (split-sequence 'break lines))))

(defun in-order (left right)
  (cond
    ((and (null left) (null right)) 'unsure)
    ((null left) 'good)
    ((null right) 'bad)
    (T
     (let ((l (car left))
	   (r (car right)))
       (cond
	 ((and (numberp l) (numberp r))
	  (cond ((< l r) 'good)
		((> l r) 'bad)
		(t (in-order (cdr left) (cdr right)))))
	 ((and (listp l) (listp r))
	  (let ((order (in-order l r)))
	    (if (eql order 'unsure)
		(in-order (cdr left) (cdr right))
		order)))
	 (T (in-order (cons (ensure-list l) (cdr left))
		      (cons (ensure-list r) (cdr right)))))))))


(defun part-1 ()
  (let ((packets (get-nums)))
    (labels ((helper (acc idx pairs)
	       (if pairs
		   (destructuring-bind (left right) (car pairs)
		     (if (eql 'good (in-order left right))
			 (helper (+ acc idx) (1+ idx) (cdr pairs))
			 (helper acc (1+ idx) (cdr pairs))))
		   acc)))
      (helper 0 1 packets))))

(defun part-2 ()
  (let ((packets (get-nums)))
    (labels ((all-packs (acc packets)
	       (if packets
		   (destructuring-bind (left right) (car packets)
		     (all-packs (cons left (cons right acc)) (cdr packets)))
		   acc))
	     (decode (acc idx lst)
	       (if lst
		   (if (or (equal (car lst) '((6)))
			   (equal (car lst) '((2))))
		       (decode (* acc idx) (1+ idx) (cdr lst))
		       (decode acc (1+ idx) (cdr lst)))
		   acc)))
      (decode 1 1
	      (sort (copy-list (all-packs '(((2)) ((6))) packets))
		    (lambda (l r) (eql 'good (in-order l r))))))))
