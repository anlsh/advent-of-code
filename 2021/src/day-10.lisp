(uiop:define-package :advent/2021/src/day-10
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-10)

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/10.txt")))

(defparameter *o2c* (fset:map (#\{ #\}) (#\( #\)) (#\< #\>) (#\[ #\])))
(defparameter *c2o* (fset:image (lambda (k v) (values v k))
                                *o2c*))
(defparameter *c2score* (fset:map (#\) 3) (#\] 57) (#\} 1197) (#\> 25137)))

(defun is-corrupted (str)
  (loop with stack = nil
        for char across str
        do (if (fset:contains? (fset:domain *o2c*) char)
               (push char stack)
               (progn (if (not stack)
                          (return-from is-corrupted (values t char)))
                      (if (equalp (car stack) (fset:@ *c2o* char))
                          (setf stack (cdr stack))
                          (return-from is-corrupted (values t char)))))
        finally
           (return (values nil stack))))

(defun solve-10a ()
  (loop for line in *input*
        summing (multiple-value-bind (is-corrupted char) (is-corrupted line)
                  (if is-corrupted
                      (fset:@ *c2score* char)
                      0))))

(defun stack-score (stack curr-score)
  (let ((c2f (fset:map (#\( 1) (#\[ 2) (#\{ 3) (#\< 4))))
    (if (null stack)
        curr-score
        (stack-score (cdr stack)
                     (+ (@ c2f (car stack))
                        (* 5 curr-score))))))

(defun solve-10b ()
  (alx:median (loop for line in *input*
                    for (is-corrupted stack) = (multiple-value-list (is-corrupted line))
                    when (not is-corrupted)
                      collecting (stack-score stack 0))))
