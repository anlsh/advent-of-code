(uiop:define-package :advent/2021/src/day-01
  (:use #:cl #:iterate #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-1a
   #:solve-1b))

(defun solve-1a ()
  (length (uiop:read-file-lines "../inputs/21.txt")))
