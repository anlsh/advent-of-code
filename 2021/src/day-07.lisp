(uiop:define-package :advent/2021/src/day-07
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-7a
   #:solve-7b))

(in-package :advent/2021/src/day-07)

(defparameter *input*
  (mapcar #'parse-integer (str:split "," (car (uiop:read-file-lines "../inputs/07.txt")))))

(defun solve-7a ()
  (loop with sorted-nums = (sort *input* #'<)
        with size = (length sorted-nums)
        with lsum = 0
        with rsum = (- (reduce #'+ (cdr sorted-nums)) (* (1- size) (car sorted-nums)))
        for i from 0
        for (hd tl) on sorted-nums
        unless tl
          do (return smallest-sum)
        minimizing (+ lsum rsum) into smallest-sum
        do (when (/= hd tl)
             (incf lsum (* (1+ i) (- tl hd)))
             (decf rsum (* (- size 1 i) (- tl hd))))
        finally (return smallest-sum)))

(defun solve-7 (distance-fn)
  (loop for pos from (apply #'min *input*) to (apply #'max *input*)
        minimizing (loop for crab in *input* summing (funcall distance-fn crab pos))))

(solve-7 (lambda (x y) (abs (- x y))))
(solve-7 (lambda (x y) (let ((diff (abs (- x y)))) (* diff (1+ diff) 1/2))))
