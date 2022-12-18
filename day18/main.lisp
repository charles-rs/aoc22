(ql:quickload '(:alexandria :cl-ppcre))
(use-package :alexandria)

(defun get-nums ()
  (with-open-file (stream "input")
    (loop :for line = (read-line stream nil)
	  :while line
	  :collect (mapcar 'parse-integer (cl-ppcre:split "," line)))))

(defun adjacent-to (voxel)
  (if voxel
      (let ((prev (mapcar (curry 'cons (car voxel))
			  (adjacent-to (cdr voxel)))))
	(revappend
	 (list
	  (cons (1+ (car voxel)) (cdr voxel))
	  (cons (1- (car voxel)) (cdr voxel)))
	 prev))
      nil))

(defun part-1 ()
  (let ((voxels (get-nums))
	(all-voxels (make-hash-table :test 'equal)))
    (mapc (lambda (v) (setf (gethash v all-voxels) T)) voxels)
    (loop :for v in voxels
	  :summing  (length (remove-if (lambda (other) (gethash other all-voxels))
				       (adjacent-to v))))))
(defun part-2 ()
  (let ((voxels (get-nums))
	(all-voxels (make-hash-table :test 'equal))
	(external (make-hash-table :test 'equal)))
    (labels ((bfs (frontier)
	       (mapc (lambda (v) (setf (gethash v external) t)) frontier)
	       (when frontier
		 (bfs (delete-duplicates
		       (loop :for v in frontier
			     :appending (remove-if
					 (lambda (adj) (or (find-if (lambda (c) (or (< c -1) (> c 21))) adj)
						      (gethash adj external)
						      (gethash adj all-voxels)))
					 (adjacent-to v)))
		       :test 'equal)))))
      (mapc (lambda (v) (setf (gethash v all-voxels) t)) voxels)
      (bfs '((0 0 0)))
      (loop :for v being the hash-keys of external
	    :summing (length (remove-if-not (lambda (other) (gethash other all-voxels))
					    (adjacent-to v)))))))
