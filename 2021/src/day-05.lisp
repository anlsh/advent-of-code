(uiop:define-package :advent/2021/src/day-05
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-5a
   #:solve-5b))

(in-package :advent/2021/src/day-04)

(defparameter *input* (uiop:read-file-lines "../inputs/05.txt"))

(defparameter *specs*
  (-<> *input*
    (mapcar (lambda (line)
              (cl-ppcre:register-groups-bind
                  ((#'parse-integer x1) (#'parse-integer y1)
                   (#'parse-integer x2) (#'parse-integer y2))
                  ("(.*),(.*) -> (.*),(.*)" line)
                (list (list x1 y1) (list x2 y2))))
            <>)))

(defun solve-5a ()
  (loop with point-to-count = (fset:empty-map 0)
        for ((x1 y1) (x2 y2)) in *specs*
        for xs = (sort (list x1 x2) #'<=)
        for ys = (sort (list y1 y2) #'<=)
        do (when (or (= x1 x2) (= y1 y2))
             ;; This actually sets the box defined by two opposite corners, and
             ;; only works for horizontal/vertical lines LOL
             (loop for x from (car xs) to (cadr xs)
                   do (loop for y from (car ys) to (cadr ys)
                            do (incf (fset:@ point-to-count (list x y))))))
        finally
           (return (fset:size (fset:filter (lambda (k) (>= (fset:@ point-to-count k) 2))
                                           (fset:domain point-to-count))))))


(defun solve-5b ()
  (loop with point-to-count = (fset:empty-map 0)
        for unsorted in *specs*
        for ((x1 y1) (x2 y2)) = (sort unsorted (lambda (x y) (<= (car x) (car y))))
        for xd = (if (= x1 x2) 0 1)
        for yd = (cond ((= y1 y2) 0)
                       ((> y1 y2) -1)
                       (t 1))
        do (loop with x = x1
                 with y = y1
                 with seen-last-point = nil
                 while (not seen-last-point)
                 do (progn (incf (fset:@ point-to-count (list x y)))
                           (when (and (= x x2)
                                      (= y y2))
                             (setf seen-last-point t))
                           (incf x xd)
                           (incf y yd)))
        finally
           (return (fset:size (fset:filter (lambda (k) (>= (fset:@ point-to-count k) 2))
                                           (fset:domain point-to-count))))))
