(ql:quickload '(:split-sequence :alexandria))
(use-package :alexandria)
(use-package :split-sequence)

(defclass monkey (t)
  ((items :initarg :items
	  :initform (error ":items must be specified")
	  :accessor items
	  :type list)
   (operation :initarg :operation
	      :initform (error ":operation must be specified")
	      :accessor operation
	      :type compiled-function)
   (test :initarg :test
	 :initform (error ":test must be specified")
	 :accessor test
	 :type fixnum)
   (true :initarg :true
	 :initform (error ":true must be specified")
	 :accessor true
	 :type fixnum)
   (false :initarg :false
	  :initform (error ":false must be specified")
	  :accessor false
	  :type fixnum)
   (inspections :initform 0
		:accessor inspections
		:reader inspections-changed
		:writer set-inspections
		:type fixnum)))

(defun parse-operation (raw-lst)
  (destructuring-bind (arg1 op arg2) (mapcar 'read-from-string raw-lst)
    (eval `(lambda (old) (,op ,arg1 ,arg2)))))

(defun parse-monkey (lines)
  (destructuring-bind (items operation test true false)
      (mapcar (curry 'split-sequence #\ ) (cdr lines))
    (make-instance
     'monkey
     :items (mapcar (rcurry 'parse-integer :junk-allowed t)
		    (cddr items))
     :operation (parse-operation (cdddr operation))
     :test (parse-integer (car (reverse test)))
     :true (parse-integer (car (reverse true)))
     :false (parse-integer (car (reverse false))))))

(defun get-nums ()
  (with-open-file (stream "input")
    (let* ((lines
	     (loop :for line = (read-line stream nil)
		   :while line
		   :collect (string-trim " " line)))
	   (monkeys (mapcar 'parse-monkey (split-sequence "" lines :test 'equal))))
      (make-array (length monkeys) :initial-contents monkeys))))


(defmethod give-to ((monkey monkey) item)
  (setf (items monkey) (reverse (cons item (reverse (items monkey))))))

(defun part-1 ()
  (let ((monkeys (get-nums)))
    (labels
	((turn (monkey)
	   (loop :while (items monkey)
		 :do
		    (let ((item (floor (funcall (operation monkey)
						(pop (items monkey)))
				       3)))
		      (incf (inspections monkey))
		      (if (= 0 (mod item (test monkey)))
			  (give-to (aref monkeys (true monkey)) item)
			  (give-to (aref monkeys (false monkey)) item)))))
	 (run-round ()
	   (loop :for monkey across monkeys
		 :do (turn monkey))))
      (dotimes (_ 20) (run-round))
      (let ((inspections
	      (sort (mapcar 'inspections (coerce monkeys 'list)) '>)))
	(* (car inspections) (cadr inspections))))))



(defun part-2 ()
  (let* ((monkeys (get-nums))
	 (reduce-factor (reduce '* (mapcar 'test (coerce monkeys 'list)))))
    (labels
	((turn (monkey)
	   (loop :while (items monkey)
		 :do
		    (let ((item (mod (funcall (operation monkey)
					      (pop (items monkey)))
				     reduce-factor)))
		      (incf (inspections monkey))
		      (if (= 0 (mod item (test monkey)))
			  (give-to (aref monkeys (true monkey)) item)
			  (give-to (aref monkeys (false monkey)) item)))))
	 (run-round ()
	   (loop :for monkey across monkeys
		 :do (turn monkey))))
      (print reduce-factor)
      (dotimes (_ 10000)
	(run-round))
      (let ((inspections
	      (sort (mapcar 'inspections (coerce monkeys 'list)) '>)))
	(* (car inspections) (cadr inspections))))))
