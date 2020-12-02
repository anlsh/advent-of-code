(uiop:define-package :advent/src/day-2
  (:use #:cl #:iterate)
  (:export
   #:solve-2a
   #:solve-2b))

(in-package :advent/src/day-2)

;; The (length (filter ...)) pattern involves an extra pass over the list. Iterate probably has some
;; clause or the other which will take care of that in a jiffy though

(defclass pwd-entry ()
  ((min :initarg :min)
   (max :initarg :max)
   (letter :initarg :letter)
   (pwd :initarg :pwd)))

(defun get-password-entries ()
  (iter (for line in-file "inputs/2.txt" using #'read-line)
    (until (zerop (length line)))
    (collect (ppcre:register-groups-bind (min-str max-str letter-str pwd)
                 ("(\\d+)-(\\d+) (.): (.*)" line)
               (make-instance 'pwd-entry
                              :min (parse-integer min-str)
                              :max (parse-integer max-str)
                              :letter (elt letter-str 0)
                              :pwd pwd)))))

(defun solve-2a ()
  (length (remove-if-not
           (lambda (pwd-entry)
             (with-slots (min max letter pwd) pwd-entry
               (<= min (count letter pwd) max)))
           (get-password-entries))))

(defun xor (a b)
  (and (or a b)
       (not (and a b))))

(defun solve-2b ()
  (length (remove-if-not
           (lambda (pwd-entry)
             (with-slots (min max letter pwd) pwd-entry
               (xor (equalp letter (elt pwd (1- min)))
                    (equalp letter (elt pwd (1- max))))))
           (get-password-entries))))
