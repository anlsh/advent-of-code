(uiop:define-package :advent/2021/src/day-14
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-14)

(defparameter *input* (uiop:read-file-lines "../inputs/14.txt"))

(defparameter *initstr* (car *input*))
(defparameter *rules*
  (let ((rules (fset:empty-map "")))
    (mapcar (lambda (line) (ppcre:register-groups-bind (e1 e2 p) ("(.)(.) -> (.)" line)
                             (fset:adjoinf rules (concatenate 'string e1 e2) p)))
            (cddr *input*))
    rules))

(defun get-letter-counts (str num-steps)
  (let ((memo-map (fset:empty-map)))
    (labels ((letter-counts (c1 c2 num-steps)
               (let* ((strkey (concatenate 'string c1 c2))
                      (key (list strkey num-steps)))
                 (when (= num-steps 0)
                   (return-from letter-counts (fset:bag c1 c2)))
                 (when (fset:contains? (fset:domain memo-map) key)
                   (return-from letter-counts (fset:@ memo-map key)))

                 (setf (fset:@ memo-map key)
                       (multiple-value-bind (val present?) (fset:@ *rules* strkey)
                         (if present?
                             (fset:less (fset:bag-sum (letter-counts c1 val (1- num-steps))
                                                      (letter-counts val c2 (1- num-steps)))
                                        val)
                             (fset:convert 'fset:bag key)))))))
      (loop with count-bag = (fset:empty-bag)
            for (a b) on (loop for c across str collecting (string c))
            for i from 0
            for key = (concatenate 'string a b)
            while b
            do (setf count-bag
                     (fset:bag-sum count-bag (letter-counts a b num-steps)))
               (when (> i 0) (setf count-bag (fset:less count-bag a)))
            finally (return count-bag)))))

(defun bag-diffcounts (bag)
  (let* ((letter-count-map (fset:convert 'fset:map bag)))
    (- (fset:reduce #'max (fset:range letter-count-map))
       (fset:reduce #'min (fset:range letter-count-map)))))

(defun solve-14a ()
  (bag-diffcounts (get-letter-counts *initstr* 10)))

(defun solve-14b ()
  (bag-diffcounts (get-letter-counts *initstr* 40)))
