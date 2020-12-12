;; Note: since CL defines * appropriately for complex numbers, a lot of the
;; direction management code here cna be heavily simplified
;; See https://www.reddit.com/r/adventofcode/comments/kbj5me/2020_day_12_solutions/gfhsobd/

(uiop:define-package :advent/src/day-12
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-12a
   #:day-12b))

(in-package :advent/src/day-12)

(defun get-inputs ()
  (mapcar (lambda (str)
            (ppcre:register-groups-bind
                (dir (#'parse-integer dist))
                ("(.)(.*)$" str)
              (list dir dist)))
          (uiop:read-file-lines "../inputs/12.txt")))

(defvar *card-to-vel*
  (fset:map ("N" #C(0 1))
            ("S" #C(0 -1))
            ("E" #C(1 0))
            ("W" #C(-1 0))))

(defvar *dir-to-vel*
  (fset:map ("L" #C(0 1))
            ("R" #C(0 -1))))

(defun day-12a ()
  (loop with vel = #C(1 0)
        with pos = #C(0 0)
        for (letter num) in (get-inputs) do
          (cond ((str:contains? letter "NSEW") (incf pos (* num (fset:@ *card-to-vel* letter))))
                ((equalp letter "F") (incf pos (* num vel)))
                (t (setf vel (* vel (expt (fset:@ *dir-to-vel* letter) (/ num 90))))))
        finally
           (return (+ (abs (realpart pos))
                      (abs (imagpart pos))))))

(defun day-12b ()
  (loop with waypoint = #C(10 1)
        with pos = #C(0 0)
        for (letter num) in (get-inputs) do
          (cond ((str:contains? letter "NSEW") (incf waypoint (* num (fset:@ *card-to-vel* letter))))
                ((equalp letter "F") (incf pos (* num waypoint)))
                (t (setf waypoint (* waypoint (expt (fset:@ *dir-to-vel* letter) (/ num 90))))))
        finally
           (return (+ (abs (realpart pos))
                      (abs (imagpart pos))))))
