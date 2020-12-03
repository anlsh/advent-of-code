(uiop:define-package :advent/src/day-3
  (:use #:cl #:iterate)
  (:export
   #:solve-3a
   #:solve-3b))

(in-package :advent/src/day-3)

(defun get-map ()
  (iter (for line in-file "inputs/3.txt" using #'read-line)
    (until (zerop (length line)))
    (collect line)))

(defun solve-3-helper (colstep rowstep)
  (let* ((map (get-map))
         (num-rows (length map))
         (num-cols (length (elt map 0)))
         (num-trees 0))
    (loop for row below num-rows by rowstep
          for i from 0
          for col = (mod (* i colstep) num-cols) do
            (if (equalp (elt (elt map row) col) #\#)
                (incf num-trees)))
    num-trees))

(defun solve-3a ()
  (solve-3-helper 3 1))

(defun solve-3b ()
  (reduce #'* (mapcar (lambda (steps) (apply #'solve-3-helper steps))
                      '((1 1) (3 1) (5 1) (7 1) (1 2)))))
