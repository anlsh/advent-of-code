(uiop:define-package :advent/2020/src/day-9
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-9a
   #:day-9b))

(in-package :advent/2020/src/day-9)

(defun get-nums ()
  (apply #'vector (mapcar #'parse-integer (uiop:read-file-lines "../inputs/9.txt"))))

(defun day-9a ()
  (let ((num-bag (fset:empty-bag))
        (nums (get-nums)))
    (loop for i below 25
          do (fset:adjoinf num-bag (aref nums i)))
    (loop for i from 25 below (length nums)
          for next-num = (aref nums i)
          do (if (not (fset:contains? (fset:image (lambda (i) (fset:contains? (fset:less num-bag i)
                                                                              (- next-num i)))
                                                  num-bag)
                                      t))
                 (return next-num)
                 (progn
                   (fset:removef num-bag (aref nums (- i 25)))
                   (fset:adjoinf num-bag next-num))))))

(defun day-9b ()
  (let ((nums (get-nums))
        (bad-num (day-9a)))
    (loop with start = 0
          with end = 1
          with sum = (aref nums start)
          while t
          do (cond ((= sum bad-num) (return (+ (reduce #'min (subseq nums start end))
                                               (reduce #'max (subseq nums start end)))))
                   ((< sum bad-num) (incf sum (aref nums end)) (incf end))
                   ((> sum bad-num) (decf sum (aref nums start)) (incf start))))))
