(uiop:define-package :advent/2021/src/day-11
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-11)

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/11.txt")
    (mapcar (lambda (line) (loop for c across line collect (parse-integer (string c))))
            <>)
    (make-array (list (length <>) (length (elt <> 0))) :initial-contents <>)))

(defparameter *size* (array-dimension *input* 0))

(defparameter *valid-positions*
  (loop with set = (fset:empty-set)
        for r below *size*
        do (loop for c below *size*
                 do (fset:adjoinf set (list r c)))
        finally
           (return set)))

(defun get-nbors (pos)
  (fset:filter (lambda (pos) (destructuring-bind (row col) pos
                               (and (< -1 row *size*)
                                    (< -1 col *size*))))
               (fset:image (lambda (off) (mapcar #'+ pos off))
                           (fset:set '(0 1) '(0 -1) '(1 0) '(-1 0)
                                     '(1 1) '(-1 1) '(1 -1) '(-1 -1)))))


(defun step-state (state)
  (let ((state (alx:copy-array state))
        (flashed (fset:empty-set)))
    (labels ((incf-pos (pos) (incf (apply #'aref state pos)))
             (zero-pos (pos) (setf (apply #'aref state pos) 0))
             (flash (pos)
               (if (<= (apply #'aref state pos) 9)
                   (return-from flash nil))
               (unless (fset:contains? flashed pos)
                 (fset:adjoinf flashed pos)
                 (let ((nbors (get-nbors pos)))
                   (fset:image #'incf-pos nbors)
                   (fset:image #'flash nbors)))))
      (fset:image #'incf-pos *valid-positions*)
      (fset:image #'flash (fset:filter (lambda (pos) (> (apply #'aref state pos) 9))
                                       *valid-positions*))
      (fset:image #'zero-pos flashed)
      (values state (fset:size flashed)))))

(defun solve-11a ()
  (loop with state = *input*
        for num-steps below 100
        for (new-state num-flashed) = (multiple-value-list (step-state state))
        do (setf state new-state)
        summing num-flashed))

(defun solve-11b ()
  (loop with state = *input*
        for num-steps from 1
        for (new-state num-flashed) = (multiple-value-list (step-state state))
        do (setf state new-state)
        when (= num-flashed (* *size* *size*))
          do (return num-steps)))
