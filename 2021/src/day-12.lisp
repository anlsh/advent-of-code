(uiop:define-package :advent/2021/src/day-12
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-12)

(defun is-lower (str)
  (string= str (string-downcase str)))

(defun get-graph ()
  (let ((graph (fset:empty-map (fset:empty-set))))
    (-<> (uiop:read-file-lines "../inputs/12.txt")
      (mapcar (lambda (str) (ppcre:register-groups-bind (start end) ("(.*)-(.*)" str)
                              (fset:adjoinf (fset:@ graph start) end)
                              (fset:adjoinf (fset:@ graph end) start)))
              <>))
    graph))

(defun solve-12-helper (graph used-duplicate)
  (labels ((count-ending-paths (node visited-smalls used-duplicate)
             (if (string= node "end")
                 1
                 (-<> (fset:less (fset:@ graph node) "start")
                   (fset:filter (lambda (node) (if (is-lower node)
                                                   (or (not (fset:contains? visited-smalls node))
                                                       (not used-duplicate))
                                                   t))
                                <>)
                   (fset:convert 'fset:seq <>)
                   (fset:image (lambda (node)
                                 (if (is-lower node)
                                     (count-ending-paths node (fset:with visited-smalls node)
                                                         (or used-duplicate (fset:contains? visited-smalls node)))
                                     (count-ending-paths node visited-smalls used-duplicate)))
                               <>)
                   (fset:reduce #'+ <> :initial-value 0)))))
    (count-ending-paths "start" (fset:empty-set) used-duplicate)))

(defun solve-12a ()
  (solve-12-helper (get-graph) t))

(defun solve-12b ()
  (solve-12-helper (get-graph) nil))
