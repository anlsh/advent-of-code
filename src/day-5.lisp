
(uiop:define-package :advent/src/day-5
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-4a
   #:solve-4b))

(in-package :advent/src/day-5)

(defun id (row col)
  (+ col (* 8 row)))

(defun get-seat-ids ()
  (loop for line in (uiop:read-file-lines "../inputs/5.txt")
        for row = (-<> line
                    (subseq <> 0 7)
                    (str:replace-all "F" "0" <>)
                    (str:replace-all "B" "1" <>)
                    (parse-integer <> :radix 2))
        for col = (-<> line
                    (subseq <> 7)
                    (str:replace-all "L" "0" <>)
                    (str:replace-all "R" "1" <>)
                    (parse-integer <> :radix 2))
        collect (id row col)))

(defun day-5a ()
  (apply #' max (get-seat-ids)))

(defun day-5b ()
  (loop for id in (sort (get-seat-ids) #'<=)
        when (= next (+ curr 2))
          do (return (+ curr 1))))
