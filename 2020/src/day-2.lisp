(uiop:define-package :advent/2020/src/day-2
  (:use #:cl #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-2a
   #:solve-2b))

(in-package :advent/2020/src/day-2)

(defclass pwd-entry ()
  ((min :initarg :min)
   (max :initarg :max)
   (letter :initarg :letter)
   (pwd :initarg :pwd)))

(defun get-password-entries ()
  (iter (for line in-file "inputs/2.txt" using #'read-line)
    (until (zerop (length line)))
    (collect
        (ppcre:register-groups-bind
            ((#'parse-integer min) (#'parse-integer max) letter-str pwd)
                 ("(\\d+)-(\\d+) (.): (.*)" line)
               (make-instance 'pwd-entry :min min :max max
                              :letter (elt letter-str 0) :pwd pwd)))))

(defun solve-2a ()
  (count-if (lambda (pwd-entry)
              (with-slots (min max letter pwd) pwd-entry
                (<= min (count letter pwd) max)))
            (get-password-entries)))

(defun solve-2b ()
  (count-if (lambda (pwd-entry)
              (with-slots (min max letter pwd) pwd-entry
                (alx:xor (equalp letter (elt pwd (1- min)))
                         (equalp letter (elt pwd (1- max))))))
            (get-password-entries)))
