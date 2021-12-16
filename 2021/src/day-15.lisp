(uiop:define-package :advent/2021/src/day-15
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria) (#:pq #:priority-queue))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-15)

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/15.txt")
    (mapcar (lambda (line) (loop for c across line collecting (parse-integer (string c))))
            <>)
    (make-array (list (length <>) (length (elt <> 0)))
                :initial-contents <>)))

(defun helper (pos-valid-fn get-val-fn target)
  (let ((pq (pq:make-pqueue #'<))
        (visited (fset:empty-map)))
    (pq:pqueue-push (list 0 0) 0 pq)
    (loop while (not (pq:pqueue-empty-p pq))
          for (pos cost) = (multiple-value-list (pq:pqueue-pop pq))
          do (unless (fset:domain-contains? visited pos)
               (when (equalp pos target)
                 (print (/ (fset:size visited) (* (1+ (elt target 0))
                                                  (1+ (elt target 1)))))
                 (return-from helper cost))
               (fset:adjoinf visited pos cost)
               (mapcar (lambda (pos) (pqueue:pqueue-push pos (+ cost (funcall get-val-fn pos))
                                                         pq))
                       (remove-if-not pos-valid-fn
                                      (mapcar (lambda (off) (mapcar #'+ pos off))
                                              '((0 1) (0 -1) (-1 0) (1 0)))))))))

(defun solve-15a ()
  (helper (lambda (pos) (destructuring-bind (row col) pos
                          (and (< -1 row (array-dimension *input* 0))
                               (< -1 col (array-dimension *input* 1)))))
          (lambda (pos) (apply #'aref *input* pos))
          (mapcar #'+ (array-dimensions *input*) '(-1 -1))))

(defun solve-15b ()
  (let* ((multiplier 5)
         (width (array-dimension *input* 0))
         (height (array-dimension *input* 1))
         (big-width (* multiplier width))
         (big-height (* multiplier height)))
    (helper (lambda (pos) (destructuring-bind (row col) pos
                            (and (< -1 row big-height)
                                 (< -1 col big-width))))
            (lambda (pos) (destructuring-bind (row col) pos
                            (multiple-value-bind (rmult orow) (truncate row height)
                              (multiple-value-bind (cmult ocol) (truncate col width)
                                (1+ (mod (+ (1- (aref *input* orow ocol))
                                            rmult cmult)
                                         9)
                                    )))))
            (list (1- big-height) (1- big-width)))))
