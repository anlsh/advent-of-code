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
                (((lambda (char) (elt char 0)) dir) (#'parse-integer dist))
                ("(N|S|E|W|F|L|R)([0-9]*)$" str)
              (list dir dist)))
          (uiop:read-file-lines "../inputs/12.txt")))

(defun cardinal-to-dir (card)
  (ecase card
    (#\N #C(0 1))
    (#\S #C(0 -1))
    (#\E #C(1 0))
    (#\W #C(-1 0))))

(defun angle-to-dir (angle)
  (ecase (mod angle 360)
    (90 #C(0 1))
    (270 #C(0 -1))
    (0 #C(1 0))
    (180 #C(-1 0))))

(defun day-12a ()
  (loop with angle = 0
        with pos = #C(0 0)
        for (mdir mdist) in (get-inputs) do
          (cond ((find mdir "NSEW") (incf pos (* mdist (cardinal-to-dir mdir))))
                ((equalp mdir #\F) (incf pos (* mdist (angle-to-dir angle))))
                (t (incf angle (* mdist (if (equalp mdir #\L) 1 -1)))))
        finally
           (return (+ (abs (realpart pos))
                      (abs (imagpart pos))))))

(defun rotate (waypoint angle)
  (let ((r (realpart waypoint))
        (i (imagpart waypoint)))
    (ecase (mod angle 360)
      (90 (complex (* -1 i) r))
      (270 (complex i (* -1 r)))
      (0 waypoint)
      (180 (complex (* -1 r) (* -1 i))))))

(defun day-12b ()
  (loop with waypoint = #C(10 1)
        with pos = #C(0 0)
        for (mdir mdist) in (get-inputs) do
          (cond ((find mdir "NSEW") (incf waypoint (* mdist (cardinal-to-dir mdir))))
                ((equalp mdir #\F) (incf pos (* mdist waypoint)))
                (t (setf waypoint (rotate waypoint (* mdist (if (equalp mdir #\L) 1 -1))))))
        finally
           (return (+ (abs (realpart pos))
                      (abs (imagpart pos))))))
