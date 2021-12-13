(uiop:define-package :advent/2021/src/day-13
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-13)

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/13.txt")
    (split-sequence:split-sequence "" <> :test #'equalp)
    (list (fset:convert 'fset:set
                        (mapcar (lambda (str) (cl-ppcre:register-groups-bind ((#'parse-integer x y)) ("(.*),(.*)" str)
                                                (list x y)))
                                (car <>)))
          (mapcar (lambda (str) (list (str:contains? "x" str)
                                      (cl-ppcre:register-groups-bind ((#'parse-integer split))
                                          (".*=(.*)" str)
                                        split)))
                  (cadr <>)))))

(defun fold-left (point-set pivot)
  (fset:image (lambda (pos) (destructuring-bind (x y) pos
                              (if (<= x pivot)
                                  (list x y)
                                  (list (- pivot (- x pivot))
                                        y))))
              point-set))

(defun fold-up (point-set pivot)
  (fset:image (lambda (pos) (destructuring-bind (x y) pos
                              (if (<= y pivot)
                                  (list x y)
                                  (list x
                                        (- pivot (- y pivot))))))
              point-set))

(defun solve-13a () (fset:size (fold-left (car *input*)
                                          (car (cdaadr *input*)))))

(defun solve-13b ()
  (let ((final-points
          (loop with point-set = (car *input*)
                for (fold-left? pivot) in (cadr *input*)
                do (setf point-set (funcall (if fold-left? #'fold-left #'fold-up)
                                            point-set pivot))
                finally
                   (return point-set))))))
