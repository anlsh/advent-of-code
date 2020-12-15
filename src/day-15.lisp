
(uiop:define-package :advent/src/day-15
  (:use #:cl #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-15a
   #:day-15b))

(in-package :advent/src/day-15)

(defun get-inputs (final-turn)
  (let ((num-to-info-map (make-hash-table))
        last-starting-num
        curr-turn)
    (loop for num in (mapcar #'parse-integer (str:split "," "9,12,1,4,17,0,18"))
          for i from 1
          do (setf last-starting-num num)
             (setf (gethash num num-to-info-map) (cons i nil))
          finally (setf curr-turn (1+ i)))
    (loop while (< curr-turn final-turn)
          for curr-turn from curr-turn
          for prev-num = last-starting-num then curr-num
          for curr-num = (let* ((prev-indices (gethash prev-num num-to-info-map))
                                (t0 (cdr prev-indices)))
                            (if t0 (- (car prev-indices) t0) 0))
          for indices = (gethash curr-num num-to-info-map)
          do (if (null indices)
                 (setf (gethash curr-num num-to-info-map) (cons curr-turn nil))
                 (progn
                   (setf (cdr indices) (car indices))
                   (setf (car indices) curr-turn)))
          finally (return curr-num))))

(defun day-15a ()
  (get-inputs 2020))

(defun day-15b ()
  (get-inputs 30000000))
