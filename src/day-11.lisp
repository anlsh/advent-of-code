(uiop:define-package :advent/src/day-11
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-11a
   #:day-11b))

(in-package :advent/src/day-11)

(defun get-inputs ()
  (let* ((lines (uiop:read-file-lines "../inputs/11.txt"))
         (rows (length lines))
         (cols (length (elt lines 0))))
    (make-array (list rows cols) :initial-contents lines)))

(defvar *nbor-offsets* '((1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1) (0 -1) (1 -1)))

(defun pos-plus-dir (pos dir)
  (mapcar #'+ pos dir))

(defun pos-valid? (arr-dims pos)
  (destructuring-bind (row col) pos
    (and (< -1 row (elt arr-dims 0))
         (< -1 col (elt arr-dims 1)))))

(defun adjacent-nbor-fn (arr)
  (lambda (pos)
    (let ((arr-dims (array-dimensions arr)))
      (remove-if-not (lambda (pos) (pos-valid? arr-dims pos))
                     (mapcar (lambda (off) (pos-plus-dir pos off))
                             *nbor-offsets*)))))

(defun los-nbor-fn (arr)
  (let ((nbor-maps (fset:empty-map))
        (arr-dims (array-dimensions arr)))
    (labels ((calc-nbor-in-dir (pos dir)
               (unless (fset:@ nbor-maps pos)
                 (setf (fset:@ nbor-maps pos) (fset:empty-map)))
               (multiple-value-bind (val done) (fset:@ (fset:@ nbor-maps pos) dir)
                 (when done (return-from calc-nbor-in-dir val)))
               (let ((npos (pos-plus-dir pos dir)))
                 (unless (pos-valid? arr-dims npos)
                   (return-from calc-nbor-in-dir nil))
                 (setf (fset:@ (fset:@ nbor-maps pos) dir)
                       (if (equalp (apply #'aref arr npos) #\.)
                           (calc-nbor-in-dir npos dir)
                           npos)))))
      (loop with (nrows ncols) = (array-dimensions arr)
            for row below nrows
            do (loop for col below ncols
                     for pos = (list row col)
                     do (mapcar (lambda (off) (calc-nbor-in-dir pos off))
                                *nbor-offsets*)))
      (loop with (nrows ncols) = (array-dimensions arr)
            for row below nrows
            do (loop for col below ncols
                     for pos = (list row col)
                     do (setf (fset:@ nbor-maps pos)
                              (remove-if-not #'identity
                                             (fset:convert 'list
                                                           (fset:range (fset:@ nbor-maps pos)))))))
      (lambda (pos) (fset:@ nbor-maps pos)))))



(defun step-seating (seats neighbor-fn threshold)
  (labels ((count-neighbors (pos)
             (loop for (row col) in (funcall neighbor-fn pos)
                   counting (equalp (aref seats row col) #\#))))
    (loop with new-seats = (make-array (array-dimensions seats))
          with (nrows ncols) = (array-dimensions seats)
          for row below nrows do
            (loop for col below ncols do
              (setf (aref new-seats row col)
                    (ecase (aref seats row col)
                      (#\L (if (zerop (count-neighbors (list row col)))
                               #\# #\L))
                      (#\# (if (>= (count-neighbors (list row col))
                                   threshold)
                               #\L #\#))
                      (#\. #\.))))
          finally
             (return new-seats))))

(defun day-11-helper (neighbor-fn threshold)
  (loop for seats = (get-inputs) then new-seats
        for new-seats = (step-seating seats neighbor-fn threshold)
        until (equalp seats new-seats)
        finally
           (return (loop for i below (array-total-size seats)
                         counting (equalp (row-major-aref seats i) #\#)))))

(defun day-11a ()
  (day-11-helper (adjacent-nbor-fn (get-inputs)) 4))

(defun day-11b ()
  (day-11-helper (los-nbor-fn (get-inputs)) 5))
