(uiop:define-package :advent/2021/src/day-04
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-4a
   #:solve-4b))

(in-package :advent/2021/src/day-04)

(defparameter *input* (uiop:read-file-lines "../inputs/day-04.txt"))
