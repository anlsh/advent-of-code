(uiop:define-package :advent/2021/src/day-08
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-8a
   #:solve-8b))

(in-package :advent/2021/src/day-08)

(defun string-to-set (string)
  (fset:convert 'fset:set string))

(defparameter *input*
  (-<> (uiop:read-file-lines "../inputs/08.example")
    (mapcar (lambda (line)
              (let ((splits (str:split " | " line)))
                (list (mapcar #'string-to-set (str:split " " (car splits)))
                      (mapcar #'string-to-set (str:split " " (cadr splits))))))
            <>)))


(defparameter *num-to-codes*
  (fset:map
   (0 (string-to-set "abcefg"))
   (1 (string-to-set "cf"))
   (2 (string-to-set "acdeg"))
   (3 (string-to-set "acdfg"))
   (4 (string-to-set "bcdf"))
   (5 (string-to-set "abdfg"))
   (6 (string-to-set "abdefg"))
   (7 (string-to-set "acf"))
   (8 (string-to-set "abcdefg"))
   (9 (string-to-set "abcdfg"))))

(defparameter *strlen-to-nums*
  (let ((map (fset:empty-map (fset:empty-set))))
    (fset:image (lambda (key val)
                  (fset:adjoinf (fset:@ map (fset:size val)) key))
                *num-to-codes*)
    map))

(defun get-representatives (fakechars-to-candidates)
  (if (zerop (fset:size fakechars-to-candidates))
      (return-from get-representatives (values (fset:empty-map) t)))
  (let ((min-key (iter:iter
                   (iter:for k in (fset:convert 'list (fset:domain fakechars-to-candidates)))
                   (iter:finding k iter:minimizing (fset:size (fset:@ fakechars-to-candidates k))))))
    (if (zerop (fset:size (fset:@ fakechars-to-candidates min-key)))
        (return-from get-representatives (values nil nil))
        (loop for candidate in (fset:convert 'list (fset:@ fakechars-to-candidates min-key))
              do (multiple-value-bind
                       (mapping valid?)
                     (get-representatives (fset:less (fset:image (lambda (k s) (values k (fset:less s candidate)))
                                                                 fakechars-to-candidates)
                                                     min-key))
                   (if valid?
                       (return (values (fset:with mapping min-key candidate) t))
                       (values nil nil)))
              finally
                 (return (values nil nil))))))

(defun get-fake-to-real-mapping (scrambled-lines)
  (let ((mapping (fset:empty-map (string-to-set "abcdefg"))))
    (fset:image
     (lambda (scrambled-set)
       (fset:image (lambda (scrambled-letter)
                     (fset:intersectf (fset:@ mapping scrambled-letter)
                                      (fset:reduce #'fset:union
                                                   (fset:image (lambda (key)
                                                                 (fset:@ *num-to-codes* key))
                                                               (fset:@ *strlen-to-nums*
                                                                       (fset:size scrambled-set)))
                                                   :initial-value (fset:empty-set))))
                   scrambled-set))
     scrambled-lines)
    mapping))

(defun solve-8b ()
  (loop for scrambled-line in *input*
        collect (get-fake-to-real-mapping (car scrambled-line))))
