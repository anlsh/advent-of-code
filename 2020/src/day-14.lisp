
(uiop:define-package :advent/2020/src/day-14
  (:use #:cl #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-14a
   #:day-14b))

(in-package :advent/2020/src/day-14)

(defparameter *word-size* 36)

(defun num-to-bits (num)
  (loop with bitvec = (make-array *word-size*)
        for i below *word-size*
        do (setf (aref bitvec i) (if (oddp num) 1 0))
           (setf num (ash num -1))
        finally
           (return bitvec)))

(defun bits-to-num (bitvec)
  (loop with res = 0
        for i below *word-size*
        do (incf res (* (aref bitvec i)
                        (expt 2 i)))
        finally
           (return res)))

(defun apply-mask (num-bitvec mask)
  (loop with retvec = (make-array *word-size*)
        for i below *word-size*
        do (setf (aref retvec i)
                 (alx:switch ((aref mask i) :test #'equalp)
                   (#\X (aref num-bitvec i))
                   (#\1 1)
                   (#\0 0)))
        finally
           (return retvec)))

(defun day-14-helper (address-decoder value-decoder filename)
  (loop with mask = (make-array *word-size* :initial-element #\0)
        with memory = (fset:empty-map)
        with sum = 0
        for line in (uiop:read-file-lines filename)
        do (if (str:starts-with? "mask" line)
               (setf mask
                     (ppcre:register-groups-bind (mask) ("mask = (.*)" line)
                       (reverse mask)))
               (ppcre:register-groups-bind
                   ((#'parse-integer locspec valspec)) ("mem\\[(.*)\\] = (.*)" line)
                 (loop for loc in (funcall address-decoder locspec mask)
                       with val = (funcall value-decoder valspec mask)
                       do (setf (fset:@ memory loc) val))))
        finally
           (fset:do-map (_ val memory)
             (declare (ignore _))
             (incf sum val))

           (return sum)))

(defun decode-loc-by-mask (loc-num mask)
  (let (decoded-locs
        (loc-bitvec (num-to-bits loc-num))
        (curr-bitvec (make-array *word-size*)))
    (labels ((decode-helper (i)
               (if (= i *word-size* )
                   (push (bits-to-num curr-bitvec) decoded-locs)
                   (alx:switch ((aref mask i) :test #'equalp)
                     (#\0 (setf (aref curr-bitvec i) (aref loc-bitvec i)) (decode-helper (1+ i)))
                     (#\1 (setf (aref curr-bitvec i) 1) (decode-helper (1+ i)))
                     (#\X (setf (aref curr-bitvec i) 0)
                          (decode-helper (1+ i))
                          (setf (aref curr-bitvec i) 1)
                          (decode-helper (1+ i)))))))
      (decode-helper 0)
      decoded-locs)))

(defun day-14a (&optional (filename "../inputs/14.txt"))
  (day-14-helper (lambda (loc mask) (declare (ignore mask)) (list loc))
                 (lambda (val mask) (bits-to-num (apply-mask (num-to-bits val) mask)))
                 filename))

(defun day-14b (&optional (filename "../inputs/14.txt"))
  (day-14-helper #'decode-loc-by-mask
                 (lambda (val mask) (declare (ignore mask)) val)
                 filename))
