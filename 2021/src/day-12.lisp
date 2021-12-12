(uiop:define-package :advent/2021/src/day-12
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-12)

(defun is-lower (str)
  (string= str (string-downcase str)))

(defparameter *nbors* (fset:empty-map (fset:empty-set)))

(defun build-graph ()
  (-<> (uiop:read-file-lines "../inputs/12.txt")
    (mapcar (lambda (str) (ppcre:register-groups-bind (start end) ("(.*)-(.*)" str)
                            (fset:adjoinf (fset:@ *nbors* start) end)
                            (fset:adjoinf (fset:@ *nbors* end) start)))
            <>)))

(build-graph)

(defun solve-12-helper (max-visit-count)
  (labels ((count-ending-paths (node visit-counts)
             (if (equalp node "end")
                 1
                 (fset:reduce #'+ (fset:image (lambda (new-node)
                                                (count-ending-paths new-node
                                                                    (if (is-lower node)
                                                                        (fset:with visit-counts node)
                                                                        visit-counts)))
                                              (fset:convert 'fset:seq (fset:filter
                                                                       (lambda (node) (if (is-lower node)
                                                                                          (< (fset:multiplicity visit-counts node) max-visit-count)
                                                                                          t))
                                                                       (fset:@ *nbors* node))))
                              :initial-value 0))))
    (count-ending-paths "start" (fset:bag "start" "start" "end"))))

(defun solve-12a ()
  (solve-12-helper 1))
