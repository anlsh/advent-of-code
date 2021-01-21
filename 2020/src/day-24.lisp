
(uiop:define-package :advent/2020/src/day-24
  (:use #:cl #:arrow-macros #:iterate)
  (:export
   #:day-24a
   #:day-24b))

(in-package :advent/2020/src/day-24)

(defun str-to-dirs (str)
  (-<> str
    (str:replace-all "ne" "1" <>)
    (str:replace-all "nw" "2" <>)
    (str:replace-all "sw" "4" <>)
    (str:replace-all "se" "5" <>)
    (str:replace-all "e" "0" <>)
    (str:replace-all "w" "3" <>)
    (iterate (for c in-it <>) (collecting (parse-integer (string c))))))

(defparameter *d-to-offset*
  (fset:map (0 #C(1 0)) (1 #C(0 1)) (2 #C(-1 1))
            (3 #C(-1 0)) (4 #C(0 -1)) (5 #C(1 -1))))

(defun d-to-nbor (pos dir-code)
  (+ pos (fset:@ *d-to-offset* dir-code)))

(defun day-24a (&optional (file "../inputs/24.txt"))
  (loop with loc-map = (fset:empty-map nil)
        for line in (uiop:read-file-lines file)
        do (-<> line
             (str-to-dirs <>)
             (reduce #'d-to-nbor <> :initial-value #C(0 0))
             (setf (fset:@ loc-map <>) (not (fset:@ loc-map <>))))
        finally
           (return (-<> loc-map
                     (fset:domain (fset:filter (lambda (key val) (declare (ignore key)) val)
                                  <>))
                     (values <> (fset:size <>))))))

(defun get-nbors (pos)
  (fset:image (lambda (off) (+ pos off))
              (fset:range *d-to-offset*)))

(defun day-24b (&optional (file "../inputs/24.txt"))
  (loop with black-tiles = (day-24a file)
        for i below 100
        for nbor-count = (fset:empty-map 0)
        do (fset:do-set (loc black-tiles)
             (fset:do-set (loc (get-nbors loc))
               (incf (fset:@ nbor-count loc))))
           (setf black-tiles
                 (fset:filter (lambda (pos)
                                (let ((nbor-count (fset:@ nbor-count pos)))
                                  (if (fset:contains? black-tiles pos)
                                      (<= 1 nbor-count 2)
                                      (= nbor-count 2))))
                              (fset:domain nbor-count)))
        finally (return (fset:size black-tiles))))
