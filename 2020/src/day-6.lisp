(uiop:define-package :advent/2020/src/day-6
  (:use #:cl #:iterate #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria) (#:split #:split-sequence))
  (:export
   #:day-6a
   #:day-6b))

(in-package :advent/2020/src/day-6)

(defun day-6a ()
  (-<> (uiop:read-file-lines "../inputs/6.txt")
    (split-sequence:split-sequence "" <> :test #'equalp)
    (mapcar (lambda (strs) (str:join "" strs)) <>)
    (loop for group-strs in <>
          summing (fset:size (fset:convert 'fset:set group-strs)))))

(defun day-6b ()
  (-<> (uiop:read-file-lines "../inputs/6.txt")
    (split-sequence:split-sequence "" <> :test #'equalp)
    (mapcar (lambda (group-strs) (mapcar (lambda (x) (fset:convert 'fset:set x))
                                         group-strs))
            <>)
    (mapcar (lambda (group-sets) (reduce #'fset:intersection group-sets)) <>)
    (loop for group-common-set in <>
          summing (fset:size group-common-set))))
