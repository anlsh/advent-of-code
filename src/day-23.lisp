
(uiop:define-package :advent/src/day-23
  (:use #:cl #:arrow-macros #:iterate)
  (:export
   #:day-23a
   #:day-23b))

(in-package :advent/src/day-23)

(defparameter *input* "135468729")

(defun get-cups (&optional bigboi)
  (iterate (for n in-it (picl:chain (picl:map (lambda (x) (parse-integer (string x)))
                                              *input*)
                                    (if bigboi (picl:range 10 (1+ (expt 10 6))))))
    (with nmap = (fset:empty-map))
    (with ls)
    (maximizing n into max)
    (progn (push n ls)
           (fset:adjoinf nmap n ls))
    (finally (setf ls (nreverse ls))
             (return (values (nconc ls ls) nmap 1 max)))))

(defun simulate (n input-thunk)
  (multiple-value-bind (ls nmap min max) (funcall input-thunk)
    (let ((curr (car ls)))
      (labels ((make-move ()
                 (let* ((curr-ring (fset:@ nmap curr))
                        (removed-nums (cdr curr-ring)))
                   (setf (cdr curr-ring) (nthcdr 4 curr-ring))
                   (loop with target = (1- curr)
                         if (< target min)
                           do (setf target max)
                         while (find target (subseq removed-nums 0 3))
                         do (decf target)
                         finally
                            (let ((ring (fset:@ nmap target)))
                              (setf (cdddr removed-nums) (cdr ring))
                              (setf (cdr ring) removed-nums)
                              (setf curr (cadr curr-ring)))))))
        (dotimes (_ n) (make-move))))
    nmap))

(defun day-23a ()
  (-<> (simulate 100 #'get-cups)
    (fset:@ <> 1)
    (subseq <> 1 9)
    (mapcar #'write-to-string <>)
    (car (multiple-value-list (parse-integer (str:join "" <>))))))

(defun day-23b ()
  (-<> (simulate (expt 10 7) (lambda () (get-cups t)))
    (fset:@ <> 1)
    (subseq <> 1 3)
    (reduce #'* <>)))
