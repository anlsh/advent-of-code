(uiop:define-package :advent/src/day-4
  (:use #:cl #:iterate)
  (:local-nicknames (#:alx #:alexandria) (#:split #:split-sequence))
  (:export
   #:solve-4a
   #:solve-4b))

(in-package :advent/src/day-4)

(defun get-entries ()
  (let* ((entry-strings
           (split-sequence:split-sequence "" (uiop:read-file-lines "../inputs/4.txt")
                                          :test #'equalp))
         (entry-strings (mapcar (lambda (strs) (str:join " " strs)) entry-strings))
         (entries (mapcar (lambda (str)
                            (mapcar (lambda (fieldval)
                                      (destructuring-bind (field val) (str:split ":" fieldval)
                                        (cons field val)))
                                    (str:split " " str)))
                          entry-strings)))
    entries))

(defun entry-alist-valid-ish? (entry-alist)
  (or (= 8 (length entry-alist))
      (and (= 7 (length entry-alist))
           (not (assoc "cid" entry-alist :test #'equalp)))))


    ;; byr (Birth Year) - four digits; at least 1920 and at most 2002.
    ;; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    ;; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    ;; hgt (Height) - a number followed by either cm or in:
    ;;     If cm, the number must be at least 150 and at most 193.
    ;;     If in, the number must be at least 59 and at most 76.
    ;; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ;; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    ;; pid (Passport ID) - a nine-digit number, including leading zeroes.
    ;; cid (Country ID) - ignored, missing or not.


(defun entry-alist-valid? (entry-alist)
  (and (entry-alist-valid-ish? entry-alist)
       (loop for (field . valstr) in entry-alist
             ;; do (format t "Entry ~a~%" entry-alist)
             always
             (macrolet ((test-fields (&body clauses)
                          (append
                           '(alx:eswitch (field :test #'equalp))
                           (loop for clause in clauses
                                 for (field . body) = clause
                                 for sym = (gensym) collect
                                 `(,field (let ((,sym (progn ,@body)))
                                            (unless ,sym
                                              (or t (format t "field ~a: Invalid string ~a~%" ,field valstr)))
                                            ,sym))))))
               (test-fields
                ("byr" (<= 1920 (parse-integer valstr) 2002))
                ("iyr" (<= 2010 (parse-integer valstr) 2020))
                ("eyr" (<= 2020 (parse-integer valstr) 2030))
                ("hgt" (ppcre:register-groups-bind
                           ((#'parse-integer num) unit)
                           ("([0-9]*)([a-z]{2})" valstr)
                         (alx:eswitch (unit :test #'equalp)
                           ("cm" (<= 150 num 193))
                           ("in" (<= 59 num 76)))))
                ("hcl" (ppcre:scan "#[0-9a-f]{6}" valstr))
                ("ecl" (member valstr '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'equalp))
                ("pid" (ppcre:scan "[0-9]{9}" valstr))
                ("cid" t))))))

(defun solve-4a ()
  (count-if #'entry-alist-valid-ish? (get-entries)))

(defun solve-4b ()
  (count-if (lambda (x) (and (entry-alist-valid? x) (print x))) (get-entries)))
