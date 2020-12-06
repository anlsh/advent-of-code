(uiop:define-package :advent/src/day-4
  (:use #:cl #:iterate #:arrow-macros)
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
  (if (assoc "cid" entry-alist :test #'equalp)
      (= (length entry-alist) 8)
      (= (length entry-alist) 7)))

(defun entry-alist-valid? (entry-alist)
  (and (entry-alist-valid-ish? entry-alist)
       (loop for (field . valstr) in entry-alist
             always
             (alx:eswitch (field :test #'equalp)
                ("byr" (<= 1920 (parse-integer valstr) 2002))
                ("iyr" (<= 2010 (parse-integer valstr) 2020))
                ("eyr" (<= 2020 (parse-integer valstr) 2030))
                ("hgt" (ppcre:register-groups-bind
                           ((#'parse-integer num) unit)
                           ("^([0-9]*)(cm|in)?" valstr)
                         (alx:eswitch (unit :test #'equalp)
                           ("cm" (<= 150 num 193))
                           ("in" (<= 59 num 76)))))
                ("hcl" (ppcre:scan "^#([0-9a-f]{6})$" valstr))
                ("ecl" (ppcre:scan "^amb|blu|brn|gry|grn|hzl|oth$" valstr))
                ("pid" (ppcre:scan "^[0-9]{9}$" valstr))
                ("cid" t)))))

(defun solve-4a ()
  (count-if #'entry-alist-valid-ish? (get-entries)))

(defun solve-4b ()
  ;; For some reason this returns the right answer + 1 ???????
  (count-if #'entry-alist-valid? (get-entries)))
