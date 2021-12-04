(uiop:define-package :advent/2021/src/day-03
  (:use #:cl #:iterate #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-3a
   #:solve-3b))

(in-package :advent/2021/src/day-03)

(defun solve-3a ()
  (loop with lines = (uiop:read-file-lines "../inputs/03.txt")
        with num-lines = (length lines)
        with bit-counts = (make-array (length (elt lines 0)) :initial-element 0)
        for line in lines
        do
           (loop for char across line
                 for idx from 0
                 when (equalp char #\1)
                   do
                      (incf (aref bit-counts idx)))
        finally
           (return (-<> bit-counts
                     (map 'list (lambda (c) (> c (/ num-lines 2))) <>)
                     (* (parse-integer (map 'string (lambda (b) (if b #\1 #\0)) <>) :radix 2)
                        (parse-integer (map 'string (lambda (b) (if b #\0 #\1)) <>) :radix 2))))))

(defun solve-3b ()
  (labels ((filter-repeatedly (lines most-common bit-pos)
             (if (= 1 (length lines))
                 (parse-integer (elt lines 0) :radix 2)
                 (loop with zero-lines = nil
                       with one-lines = nil
                       for line in lines
                       do (if (equalp #\0 (aref line bit-pos))
                              (push line zero-lines)
                              (push line one-lines))
                       finally
                          (return
                            (if (= (length zero-lines) (length one-lines))
                                (if most-common
                                    (filter-repeatedly one-lines most-common (1+ bit-pos))
                                    (filter-repeatedly zero-lines most-common (1+ bit-pos)))
                                (let ((sorted-lines (sort (list zero-lines one-lines)
                                                          (lambda (a b) (< (length a) (length b))))))
                                  (if most-common
                                      (filter-repeatedly (elt sorted-lines 1) most-common (1+ bit-pos))
                                      (filter-repeatedly (elt sorted-lines 0) most-common (1+ bit-pos))))))))))
    (-<> (uiop:read-file-lines "../inputs/03.txt")
      (* (filter-repeatedly <> t 0)
         (filter-repeatedly <> nil 0)))))
