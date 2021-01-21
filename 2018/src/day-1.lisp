(uiop:define-package :advent/2018/src/day-1
  (:use :cl :arrow-macros :iterate)
  (:local-nicknames (:alx :alexandria))
  (:export #:day-1a
           #:day-1b))

(in-package :advent/2018/src/day-1)

(defun day-1a (&optional (filename "../inputs/1.txt"))
  (-<> (uiop:read-file-lines filename)
    (mapcar #'parse-integer <>)
    (reduce #'+ <>)))

(defun day-1b (&optional (filename "../inputs/1.txt"))
  (-<> (uiop:read-file-lines filename)
    (mapcar #'parse-integer <>)
    (iterate
      (with cache = (fset:empty-set))
      (for num in-it (picl:cycle <>))
      (summing num into sum)
      (if (fset:contains? cache sum)
          (return sum)
          (fset:adjoinf cache sum)))))
