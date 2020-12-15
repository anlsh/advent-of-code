
(uiop:define-package :advent/src/day-15
  (:use #:cl #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-15a
   #:day-15b))

(in-package :advent/src/day-15)

(defun get-inputs (final-turn)
  (let ((num-to-info-map (fset:empty-map))
        prev-num
        curr-turn)
    (loop for num in (mapcar #'parse-integer (str:split "," "9,12,1,4,17,0,18"))
          for i from 1
          do (setf prev-num num)
             (push i (fset:@ num-to-info-map num))
          finally (setf curr-turn (1+ i)))
    (loop while (< curr-turn final-turn)
          for curr-turn from curr-turn
          for prev-num = (if (cdr (fset:@ num-to-info-map prev-num))
                             (destructuring-bind (t1 t0 . rest) (fset:@ num-to-info-map prev-num)
                               (declare (ignore rest))
                               (- t1 t0))
                             0)
          do (push curr-turn (fset:@ num-to-info-map prev-num))
          finally (return prev-num))))

(defun day-15a ()
  (get-inputs 2020))

(defun day-15b ()
  (get-inputs 30000000))
