(uiop:define-package :advent/src/day-21
  (:use #:cl #:arrow-macros)
  (:export
   #:day-21a
   #:day-22b))

(in-package :advent/src/day-21)

(defun get-inputs ()
  (-<> (uiop:read-file-lines "../inputs/21.txt")
    (mapcar (lambda (line) (str:split " (contains " (subseq line 0 (1- (length line)))))
            <>)
    (mapcar (lambda (s)
              (destructuring-bind (ings alls) s
                (list (str:split " " ings)
                      (str:split ", " alls))))
            <>)))

(defun get-allergen-possibs ()
  (loop with lines = (get-inputs)
        with allergen-possibs = (fset:empty-map (fset:complement (fset:empty-set)))
        for (foods alls) in lines
        for food-set = (fset:convert 'fset:set foods)
        do (fset:do-set (all (fset:convert 'fset:set alls))
             (setf (fset:@ allergen-possibs all)
                   (fset:intersection (fset:@ allergen-possibs all)
                                      food-set)))
        finally (return allergen-possibs)))

(defun get-food-to-allergens ()
  (let ((allergen-possibs (get-allergen-possibs)))
    (labels ((assign-allergen (remaining-allergens food-assignments)
               (if (null remaining-allergens)
                   (return-from get-food-to-allergens food-assignments)
                   (destructuring-bind (all . rest-alls) remaining-allergens
                     (fset:do-set (food (fset:set-difference (fset:@ allergen-possibs all)
                                                             (fset:domain food-assignments)))
                       (assign-allergen rest-alls
                                        (fset:with food-assignments food all)))))))
      (assign-allergen (fset:convert 'list (fset:domain allergen-possibs))
                       (fset:empty-map)))))

(defun day-21a ()
  (let ((food-to-allergen (get-food-to-allergens)))
    (loop for (foods _) in (get-inputs)
          summing (loop for food in foods
                        counting (not (fset:contains? (fset:domain food-to-allergen) food))))))

(defun day-21b ()
  (let ((food-to-allergen (get-food-to-allergens)))
    (str:join "," (mapcar #'car
                          (sort (fset:convert 'list food-to-allergen)
                                (lambda (x y) (string<= (cdr x) (cdr y))))))))
