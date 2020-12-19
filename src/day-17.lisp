
(uiop:define-package :advent/src/day-17
  (:use #:cl #:arrow-macros)
  (:local-nicknames)
  (:export
   #:day-17a
   #:day-17b))

(in-package :advent/src/day-17)

(defun get-initial-set (pos-fn)
  (loop with cubes = (fset:empty-set)
        for line in (uiop:read-file-lines "../inputs/17.txt")
        for row from 0
        do (loop for let across line
                 for col from 0
                 when (equalp let #\#)
                   do (fset:adjoinf cubes (funcall pos-fn row col)))
        finally
           (return cubes)))

(defun count-active-nbors (nbor-fn active-cubes pos)
  (fset:size (fset:intersection active-cubes (funcall nbor-fn pos))))

(defun simulate (initial-active-cubes nbor-fn num-steps)
  ;; A rather inefficient way to do it (I generate all the neighbors and then check all of *their*
  ;; neighbors too in another pass). This can be done better by maintaining a location-to-count map
  ;; and incrementing the nbor count for each neighbor of each active cell.
  ;;
  ;;I had trouble actually implementing that approach tho :(
  (loop for active-cubes
          = initial-active-cubes
            then (-<> active-cubes
                   (fset:image nbor-fn <>)
                   (fset:reduce #'fset:union <> :initial-value (fset:empty-set))
                   (fset:filter (lambda (pos)
                                  (let ((nbor-count (count-active-nbors nbor-fn active-cubes pos)))
                                    (if (fset:contains? active-cubes pos)
                                        (<= 2 nbor-count 3)
                                        (= nbor-count 3))))
                                <>))
        for i below num-steps
        finally
           (return (fset:size active-cubes))))

(defun simulate-in-n-dims (n steps)
  (let ((nbor-offsets
          (fset:less (fset:convert 'fset:set
                                   (picl:iter-to-list (picl:apply #'picl:product
                                                                  (picl:repeat n '(-1 0 1)))))
                     (make-array n :initial-element 0))))
    (simulate (get-initial-set
               (lambda (row col) (make-array n :initial-contents
                                             (append (list row col)
                                                     (picl:iter-to-list (picl:repeat (- n 2) 0))))))
              (lambda (pos) (fset:image (lambda (off) (map 'vector #'+ pos off)) nbor-offsets))
              steps)))

(defun day-17a ()
   (simulate-in-n-dims 3 6))

(defun day-17b ()
   (simulate-in-n-dims 4 6))
