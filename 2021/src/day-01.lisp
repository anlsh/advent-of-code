(uiop:define-package :advent/2021/src/day-01
  (:use #:cl #:iterate #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-1a
   #:solve-1b))

(in-package :advent/2021/src/day-01)

(defun count-increases (ls)
  (let ((vector (make-array (length ls) :initial-contents ls)))
    (loop for i from 1 below (array-total-size vector)
          counting (> (aref vector i) (aref vector (1- i))))))

(defun solve-1a ()
  (-<> (uiop:read-file-lines "../inputs/01.txt")
    (mapcar #'parse-integer <>)
    (count-increases <>)))

(defun solve-1b ()
  (-<> (uiop:read-file-lines "../inputs/01.txt")
    (mapcar #'parse-integer <>)
    (list <> (cdr <>) (cddr <>))
    (apply #'mapcar #'+ <>)
    (count-increases <>)))
