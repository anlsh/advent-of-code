
(uiop:define-package :advent/2020/src/day-13
  (:use #:cl #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-13a
   #:day-13b))

(in-package :advent/2020/src/day-13)

(defun wait-time (timestamp interval)
  (mod (* -1 timestamp) interval))

(defun get-inputs (input-file)
  (with-open-file (stream input-file)
    (list (parse-integer (read-line stream))
          (loop for i from 0
                for s in (str:split "," (read-line stream))
                when (not (equalp s "x"))
                  collect (list (parse-integer s) i)))))

(defun day-13a (&optional (input-file "../inputs/13.txt"))
  (iterate
    (with (min-time intervals) = (get-inputs input-file))
    (for (interval off) in intervals)
    (finding interval minimizing (wait-time min-time interval)
             into best-ival)
    (finally
     (return (* best-ival (wait-time min-time interval))))))

(defun get-time-and-step (req-list)
  (destructuring-bind ((ival off) . rest) req-list
    (if (not rest)
        (values (- ival off)
                ival)
        (multiple-value-bind (time step) (get-time-and-step rest)
          (values (loop for tnext from time by step
                        until (zerop (mod (+ tnext off) ival))
                        finally (return tnext))
                  (lcm ival step))))))

(defun day-13b (&optional (input-file "../inputs/13.txt"))
  (get-time-and-step (second (get-inputs input-file))))
