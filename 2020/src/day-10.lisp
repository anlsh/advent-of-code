(uiop:define-package :advent/2020/src/day-10
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-10a
   #:day-10b))

(in-package :advent/2020/src/day-10)

(defun get-nums ()
  (sort (mapcar #'parse-integer (uiop:read-file-lines "../inputs/10.txt"))
        #'<=))

(defun day-10a ()
  (loop with jolcounts = (fset:empty-bag)
        with adapters = (get-nums)
        for (n1 n2) on adapters
        while n2
        do
           (fset:adjoinf jolcounts (- n2 n1))
        finally
           ;; Accunt for first adapter and final step of +3 to device
           (fset:adjoinf jolcounts 3)
           (fset:adjoinf jolcounts (elt adapters 0))
           (return (* (fset:multiplicity jolcounts 1)
                      (fset:multiplicity jolcounts 3)))))

(defun day-10b ()
  (loop with adapters = (apply #'vector (get-nums))
        with n = (length adapters)
        with dp = (make-array n)
          initially
             (setf (aref dp (1- n)) 1)

        for i downto 0 from (- n 2)
        do
           (setf (aref dp i)
                 (loop for inext from (1+ i)
                       while (and (< inext n)
                                  (<= (- (aref adapters inext) (aref adapters i)) 3))
                       summing (aref dp inext)))
        finally (return (loop for i from 0
                              while (<= (aref adapters i) 3)
                              summing (aref dp i)))))
