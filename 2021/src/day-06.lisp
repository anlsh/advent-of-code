(uiop:define-package :advent/2021/src/day-06
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-6a
   #:solve-6b))

(in-package :advent/2021/src/day-06)

(defparameter *input*
  (mapcar #'parse-integer (str:split "," (car (uiop:read-file-lines "../inputs/06.txt")))))

(defun simulate-fish (num-days)
  (let ((days-to-lanternfish (fset:empty-map 0))
        (num-lanternfish (length *input*)))
    (loop for lfish in *input*
          do (incf (fset:@ days-to-lanternfish lfish)))
    (loop for day from 0 below num-days
          for n-pregnant-lanternfish = (fset:@ days-to-lanternfish day)
          do (incf num-lanternfish n-pregnant-lanternfish)
             (incf (fset:@ days-to-lanternfish (+ day 7)) n-pregnant-lanternfish)
             (incf (fset:@ days-to-lanternfish (+ day 9)) n-pregnant-lanternfish)
          finally
             (return num-lanternfish))))

(print (simulate-fish 80))
(print (simulate-fish 256))
