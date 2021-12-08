(uiop:define-package :advent/2021/src/day-08
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-8a
   #:solve-8b))

(in-package :advent/2021/src/day-08)

(defparameter *input*
  (uiop:read-file-lines "../inputs/07.txt"))
