
(uiop:define-package :advent/2020/src/day-15
  (:use #:cl)
  (:local-nicknames)
  (:export
   #:day-15a
   #:day-15b))

(in-package :advent/2020/src/day-15)

(defun num-on-turn (final-turn)
  (loop with starting-nums = (apply #'vector (mapcar #'parse-integer
                                                     (str:split "," "9,12,1,4,17,0,18")))
        with num-to-index = (make-array (+ (reduce #'max starting-nums) final-turn)
                                        :element-type 'fixnum :initial-element -1)
          initially
             (loop for num across starting-nums
                   for i from 0
                   when (> i 0)
                     do (setf (aref num-to-index (aref starting-nums (1- i))) i))
        for curr-turn from (1+ (length starting-nums)) upto final-turn
        for prev-num
          = (aref starting-nums (1- (length starting-nums)))
            then curr-num
        for curr-num = (if (minusp (aref num-to-index prev-num))
                           0
                           (- (1- curr-turn) (aref num-to-index prev-num)))
        do (setf (aref num-to-index prev-num) (1- curr-turn))
        finally
           (return curr-num)))

(defun day-15a ()
  (num-on-turn 2020))

(defun day-15b ()
  (num-on-turn 30000000))
