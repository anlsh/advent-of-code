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
  (-<> (uiop:read-file-lines "../inputs/12.example")
    (mapcar (lambda (str) (ppcre:register-groups-bind (start end) ("(.*)-(.*)" str)
                            (fset:adjoinf (fset:@ *nbors* start) end)
                            (fset:adjoinf (fset:@ *nbors* end) start)))
            <>)))

(build-graph)

(defun solve-12-helper (graph)
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
                                                                                          (< (fset:multiplicity visit-counts node) 1)
                                                                                          t))
                                                                       (fset:@ graph node))))
                              :initial-value 0))))
    (count-ending-paths "start" (fset:bag "start" "start"))))

(defun solve-12a ()
  (solve-12-helper *nbors*))

(defparameter *nbors-p2* (fset:empty-map (fset:empty-set)))

(defun inflate-node (node)
  (if (is-lower node)
      (fset:set node (concatenate 'string node "--dup"))
      (fset:set node)))

(defun build-graph-p2 ()
  (labels ((add-nbors (root nbor)
             (fset:image (lambda (root)
                           (let ((nbors (inflate-node nbor)))
                             (fset:image (lambda (nbor)
                                           (fset:adjoinf (fset:@ *nbors-p2* root) nbor)
                                           (fset:adjoinf (fset:@ *nbors-p2* nbor) root))
                                         nbors)))
                         (inflate-node root))))
    (-<> (uiop:read-file-lines "../inputs/12.txt")
      (mapcar (lambda (str) (ppcre:register-groups-bind (start end) ("(.*)-(.*)" str)
                              (add-nbors start end)))
              <>))))

(build-graph-p2)
