(ql:quickload '(:split-sequence :alexandria :cl-ppcre :defstar))
(use-package :alexandria)
(use-package :split-sequence)
(use-package :defstar)

(defstruct (blueprint (:conc-name nil))
  (id 0 :type fixnum)
  (ore-ore 0 :type fixnum)
  (clay-ore 0 :type fixnum)
  (obs-ore 0 :type fixnum)
  (obs-clay 0 :type fixnum)
  (geode-ore 0 :type fixnum)
  (geode-obs 0 :type fixnum))


(defun parse-blueprint (lst)
  (destructuring-bind (id ore-ore clay-ore obs-ore obs-clay geode-ore geode-obs) lst
      (make-blueprint :id id :ore-ore ore-ore :clay-ore clay-ore :obs-ore obs-ore
		      :obs-clay obs-clay :geode-ore geode-ore :geode-obs geode-obs)))

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (parse-blueprint
		    (remove-if-not
		     (lambda (x) x)
		     (mapcar (rcurry 'parse-integer :junk-allowed t)
			     (cl-ppcre:split " " line)))))))


(defconstant bot-of '((ore . ore-bots) (clay . clay-bots)
		      (obsidian . obsidian-bots) (geode . geode-bots)))

(defun max-ore-per-sec (blueprint)
  (max (ore-ore blueprint)
       (clay-ore blueprint)
       (obs-ore blueprint)
       (geode-ore blueprint)))

(defun collect (state blueprint)
  (let ((map (alist-hash-table state)))
    (mapc (lambda (pr) (incf (gethash (car pr) map)
			(gethash (cdr pr) map)))
	  bot-of)
    (when (>= (1- (gethash 'ore-bots map))
	      (max-ore-per-sec blueprint))
      (minf (gethash 'ore map) (* 1 (max-ore-per-sec blueprint))))
    (hash-table-alist map)))

(defun build-bot (type state blueprint)
  (let ((fn
	  (case type
	    (ore (lambda (pr) (case (car pr)
			   (ore-bots (1+ (cdr pr)))
			   (ore (- (cdr pr) (ore-ore blueprint)))
			   (forbidden nil)
			   (t (cdr pr)))))
	    (clay  (lambda (pr) (case (car pr)
			     (clay-bots (1+ (cdr pr)))
			     (ore (- (cdr pr) (clay-ore blueprint)))
			     (forbidden nil)
			     (t (cdr pr)))))
	    (obsidian  (lambda (pr) (case (car pr)
				 (obsidian-bots (1+ (cdr pr)))
				 (ore (- (cdr pr) (obs-ore blueprint)))
				 (clay (- (cdr pr) (obs-clay blueprint)))
				 (forbidden nil)
				 (t (cdr pr)))))
	    (geode (lambda (pr) (case (car pr)
			     (geode-bots (1+ (cdr pr)))
			     (ore (- (cdr pr) (geode-ore blueprint)))
			     (obsidian (- (cdr pr) (geode-obs blueprint)))
			     (forbidden nil)
			     (t (cdr pr))))))))
    (mapcar (lambda (pr) (let ((v (funcall fn pr)))
		      (cons (car pr) v)))
	    (collect state blueprint))))

(defun aget (key lst)
  (cdr (assoc key lst)))

(defun repeat (f n arg)
  (labels ((helper (acc n)
	     (if (= n 0) acc
		 (helper (funcall f acc) (1- n)))))
    (helper arg n)))

(defun upper-bound (state time-left)
  (+ (aget 'geode state)
     (* (aget 'geode-bots state) time-left)
     (floor (* time-left (1+ time-left)) 2)))

(defun best-for-state (blueprint state time-left best-so-far)
  (cond
    ((< (upper-bound state time-left) best-so-far)
     -1)
    ((= time-left 1)
     (+ (aget 'geode state) (aget 'geode-bots state)))
    ((= time-left 0)
     (aget 'geode state))
    ((< time-left 0)
     -1)
    (T
     (when (> (aget 'obsidian-bots state) 0)
       (let* ((steps (max (max 0 (ceiling (- (geode-obs blueprint) (aget 'obsidian state))
					  (aget 'obsidian-bots state)))
			  (max 0 (ceiling (- (geode-ore blueprint) (aget 'ore state))
					  (aget 'ore-bots state))))))
	 (when (< steps time-left)
	   (maxf best-so-far
		 (best-for-state
		  blueprint
		  (build-bot 'geode (repeat (rcurry 'collect blueprint) steps state)
			     blueprint)
		  (- time-left steps 1)
		  best-so-far)))))
     (when (> (aget 'clay-bots state) 0)
       (let* ((steps (max (max 0 (ceiling (- (obs-clay blueprint) (aget 'clay state))
					  (aget 'clay-bots state)))
			  (max 0 (ceiling (- (obs-ore blueprint) (aget 'ore state))
					  (aget 'ore-bots state))))))
	 (when (< steps time-left)
	   (maxf best-so-far
		 (best-for-state
		  blueprint
		  (build-bot 'obsidian (repeat (rcurry 'collect blueprint) steps state)
			     blueprint)
		  (- time-left steps 1)
		  best-so-far)))))
     (let* ((steps (max 0 (ceiling (- (clay-ore blueprint) (aget 'ore state))
				   (aget 'ore-bots state)))))
       (when (< steps time-left)
	 (maxf best-so-far
	       (best-for-state
		blueprint
		(build-bot 'clay (repeat (rcurry 'collect blueprint) steps state)
			   blueprint)
		(- time-left steps 1)
		best-so-far))))
     (let* ((steps (max 0 (ceiling (- (ore-ore blueprint) (aget 'ore state))
				   (aget 'ore-bots state)))))
       (when (and (<= (aget 'ore-bots state) (max-ore-per-sec blueprint)) (< steps time-left))
	 (maxf best-so-far
	       (best-for-state
		blueprint
		(build-bot 'ore (repeat (rcurry 'collect blueprint) steps state)
			   blueprint)
		(- time-left steps 1)
		best-so-far))))
     (max best-so-far (+ (aget 'geode state) (* (aget 'geode-bots state) time-left))))))

(defun part-1 ()
  (let ((blueprints (get-nums))
	(start '((ore . 0) (clay . 0) (obsidian . 0) (geode . 0)
		 (ore-bots . 1) (clay-bots . 0) (obsidian-bots . 0) (geode-bots . 0))))
    (reduce '+ (mapcar (lambda (b) (* (slot-value b 'id) (best-for-state b start 24 0))) blueprints))))

(defun part-2 ()
  (let ((blueprints (get-nums))
	(start '((ore . 0) (clay . 0) (obsidian . 0) (geode . 0)
		 (ore-bots . 1) (clay-bots . 0) (obsidian-bots . 0) (geode-bots . 0))))
    (* (best-for-state (car blueprints) start 32 0)
       (best-for-state (cadr blueprints) start 32 0)
       (best-for-state (caddr blueprints) start 32 0))))

