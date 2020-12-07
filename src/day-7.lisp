(uiop:define-package :advent/src/day-7
  (:use #:cl)
  (:export
   #:day-7a
   #:day-7b))

(in-package :advent/src/day-7)

(defun get-inputs ()
  (let ((parent-map (fset:empty-map))
        (child-map (fset:empty-map)))
    (loop for line in (uiop:read-file-lines "../inputs/7.txt")
          do
             (ppcre:register-groups-bind
                 (parent child-strs) ("(.*) bags contain(.*)\." line)
               (mapcar
                (lambda (child-str)
                  (ppcre:register-groups-bind
                      (num child) (" (\\d*) (.*) bag" child-str)
                    (push parent (fset:@ parent-map child))
                    (push (list child (parse-integer num)) (fset:@ child-map parent))))
                (str:split "," child-strs))))
    (values parent-map child-map)))

(defun day-7a ()
  (let ((parent-map (get-inputs)))
    (labels ((get-subtree-set (key)
               (reduce #'fset:union (fset:@ parent-map key)
                       :initial-value (fset:set key) :key #'get-subtree-set)))
      (1- (fset:size (get-subtree-set "shiny gold"))))))

(defun day-7b ()
  (let ((child-map (second (multiple-value-list (get-inputs))))
         (dp-table (fset:empty-map)))
    (labels
        ((count-subbags (key)
           (multiple-value-bind (val done) (fset:@ dp-table key)
             (if done
                 val
                 (setf (fset:@ dp-table key)
                       (1+ (reduce #'+ (fset:@ child-map key)
                                   :key (lambda (edge) (* (second edge)
                                                          (count-subbags (first edge)))))))))))
       (1- (count-subbags "shiny gold")))))
