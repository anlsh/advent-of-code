
(uiop:define-package :advent/2020/src/day-25
  (:use #:cl #:arrow-macros #:iterate)
  (:export
   #:day-25))

(in-package :advent/2020/src/day-25)

(defparameter *subject-num* 7)
(defparameter *mod-base* 20201227)

(defun modlog (remainder)
  (loop for num = 1 then (mod (* num *subject-num*) *mod-base*)
        for i from 0
        when (= num remainder)
          do (return i)))

(defun modexpt (subject-num pwr)
  (loop for n = 1 then (mod (* n subject-num) *mod-base*)
        for i below pwr
        finally (return n)))

(defun solve-25 ()
  (modexpt 16915772 (modlog 18447943)))
