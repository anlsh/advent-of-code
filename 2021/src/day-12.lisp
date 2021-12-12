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

(defun solve-12-helper (graph used-duplicate)
  (labels ((count-ending-paths (node visited-smalls used-duplicate)
             (if (string= node "end")
                 1
                 (fset:reduce #'+ (fset:image (lambda (node)
                                                (if (is-lower node)
                                                    (count-ending-paths node (fset:with visited-smalls node)
                                                                        (or used-duplicate (fset:contains? visited-smalls node)))
                                                    (count-ending-paths node visited-smalls used-duplicate)))
                                              (fset:convert 'fset:seq (fset:filter
                                                                       (lambda (node) (if (is-lower node)
                                                                                          (or (not (fset:contains? visited-smalls node))
                                                                                              (not used-duplicate))
                                                                                          t))
                                                                       (fset:less (fset:@ graph node) "start"))))
                              :initial-value 0))
             ))
    (count-ending-paths "start" (fset:empty-set) used-duplicate)))

(defun solve-12a ()
  (solve-12-helper *nbors* t))

;; (defparameter *nbors-p2* (fset:empty-map (fset:empty-set)))

;; (defun inflate-node (node)
;;   (if (is-lower node)
;;       (fset:set node (concatenate 'string node "--dup"))
;;       (fset:set node)))

;; (defun build-graph-p2 ()
;;   (labels ((add-nbors (root nbor)
;;              (fset:image (lambda (root)
;;                            (let ((nbors (inflate-node nbor)))
;;                              (fset:image (lambda (nbor)
;;                                            (fset:adjoinf (fset:@ *nbors-p2* root) nbor)
;;                                            (fset:adjoinf (fset:@ *nbors-p2* nbor) root))
;;                                          nbors)))
;;                          (inflate-node root))))
;;     (-<> (uiop:read-file-lines "../inputs/12.txt")
;;       (mapcar (lambda (str) (ppcre:register-groups-bind (start end) ("(.*)-(.*)" str)
;;                               (add-nbors start end)))
;;               <>))))

;; (build-graph-p2)
