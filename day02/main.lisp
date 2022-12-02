(ql:quickload "cl-ppcre")

(defun to-num (x)
  (cond
    ((or (equal x "X") (equal x "A")) 1)
    ((or (equal x "Y") (equal x "B")) 2)
    ((or (equal x "Z") (equal x "C")) 3)))

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (mapcar #'to-num (cl-ppcre:split " " line)))))

(defun outcome (x y)
  (cond ((= x y) 3)
	((= (- y 1) (mod x 3)) 6)
	(T 0)))

(defun score (round)
  (+ (cadr round) (apply #'outcome round)))

(defun part-1 () (reduce #'+ (mapcar #'score (get-nums))))


(defun norm (x)
  (case x (4 1) (0 3) (T x)))

(defun part-2-result (round)
  (case (cadr round)
    (1 (norm (- (car round) 1)))
    (2 (+ 3 (car round)))
    (3 (+ 6 (norm (1+ (car round)))))))

(defun part-2 ()
  (reduce #'+ (mapcar #'part-2-result (get-nums))))
