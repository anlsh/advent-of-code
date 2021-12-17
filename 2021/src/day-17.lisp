(uiop:define-package :advent/2021/src/day-17
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria) (#:pq #:priority-queue))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-17)

(defparameter *input*
  ;; (-<> (uiop:read-file-lines "../inputs/17.txt")
  ;;   (cl-ppcre:register-groups-bind
  ;;       (xmin xmax ymin ymax)
  ;;       ("target area: x=(.*)\.\.(.*), y=(.*)\.\.(.*)") (car <>)
  ;;     (list xmin xmax ymin ymax)))
  (list 207 263 -115 -63)
  )

(defparameter *min-x* 207)
(defparameter *max-x* 263)
(defparameter *min-y* -115)
(defparameter *max-y* -63)

(defun min-x-velocity (min-x)
  (loop for n from 0 while (< (* 1/2 n (1+ n))
                              min-x)
        finally (return n)))

(defun velocity-hits-target (dx dy)
  (labels ((step-probe (x y dx dy)
             (list (+ x dx)
                   (+ y dy)
                   (* (signum dx) (1- (abs dx)))
                   (1- dy))))
    (loop with probe-state = (list 0 0 dx dy)
          while t
          do (setf probe-state (apply #'step-probe probe-state))
             (destructuring-bind (x y dx dy) probe-state
               (cond ((and (<= *min-x* x *max-x*)
                           (<= *min-y* y *max-y*))
                      (return-from velocity-hits-target t))
                     ((or (< y *min-y*)
                          (> x *max-x*)) (return-from velocity-hits-target nil)))))))

(let ((max-yvel (abs (1+ *min-y*))))
  (defun solve-17a ()
    (* 1/2 max-yvel (1+ max-yvel)))

  (defun solve-17b ()
    (loop for dx from (min-x-velocity *min-x*) upto *max-x*
          appending (loop for dy from *min-y* upto max-yvel
                          when (velocity-hits-target dx dy)
                            collect (list dx dy)))))

(defun solve-stupid ()
  "Lmao this problem can just be brute forced"
  (loop for dx from 0 upto 1000
        ;; 1000 is a total fucking guess god I'm disappointed
        appending (loop for dy from -1000 upto 1000
                        when (velocity-hits-target dx dy)
                          collect (list dx dy))))
