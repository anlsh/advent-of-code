(uiop:define-package :advent/2021/src/day-09
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-9a
   #:solve-9b))

(in-package :advent/2021/src/day-09)

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/09.txt")
    (mapcar (lambda (line) (loop for char across line collect (parse-integer (string char)))) <>)
    (make-array (list (length <>) (length (elt <> 0)))
                :initial-contents <>)))

(defparameter *height* (array-dimension *input* 0))
(defparameter *width* (array-dimension *input* 1))
(defparameter *valid-positions* (loop with set = (fset:empty-set)
                                      for row from 0 below *height*
                                      do (loop for col from 0 below *width*
                                               do (fset:adjoinf set (list row col)))
                                      finally
                                         (return set)))

(defun get-nbors (pos)
  (destructuring-bind (row col) pos
    (fset:filter (lambda (pos) (fset:contains? *valid-positions* pos))
                 (fset:image (lambda (off) (destructuring-bind (roff coff) off
                                             (list (+ row roff) (+ col coff))))
                             '((1 0) (-1 0) (0 1) (0 -1))))))

(defun is-low-point (pos)
  (destructuring-bind (row col) pos
    (< (aref *input* row col)
       (fset:reduce #'min
                    (fset:image (lambda (pos) (apply #'aref *input* pos))
                                (get-nbors (list row col)))))))
(defun solve-9a ()
  (fset:reduce #'+ (fset:image (lambda (pos) (1+ (apply #'aref *input* pos)))
                               (fset:convert 'fset:seq (fset:filter #'is-low-point *valid-positions*)))))

(defun basin-size (pos)
  (let ((visited-set (fset:empty-set)))
    (labels ((dfs-helper (pos)
               (unless (or (fset:contains? visited-set pos)
                           (= 9 (apply #'aref *input* pos)))
                 (fset:adjoinf visited-set pos)
                 (fset:image #'dfs-helper (get-nbors pos)))))
      (dfs-helper pos)
      (fset:size visited-set))))

(defun solve-9b ()
  (-<> *valid-positions*
    (fset:filter #'is-low-point <>)
    (fset:convert 'fset:seq <>)
    (fset:image #'basin-size <>)
    (fset:convert 'list <>)
    (sort <> #'>)
    (destructuring-bind (a b c . nil) <>
      (* a b c))))
