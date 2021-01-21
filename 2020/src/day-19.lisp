
(uiop:define-package :advent/2020/src/day-19
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-19a
   #:day-19b))

(in-package :advent/2020/src/day-19)

(defun parse-input ()
  (let* ((lines (uiop:read-file-lines "../inputs/19.txt"))
         (rule-set (fset:empty-map)))
    (destructuring-bind (rule-lines str-lines) (split-sequence:split-sequence "" lines
                                                                              :test #'equalp)
      (loop for rule-line in rule-lines
            do (ppcre:register-groups-bind
                   ((#'parse-integer var) prod-str)
                   ("(\\d*): (.*)" rule-line)
                 (setf (fset:@ rule-set var)
                       (mapcar (lambda (var-str)
                                 (if (str:contains? "\"" var-str)
                                     (list (str:replace-all "\"" "" var-str))
                                     (mapcar #'parse-integer (str:split " " var-str))))
                               (str:split " | " prod-str)))))
      (values rule-set str-lines))))

(defun varls-matches (rule-map var-ls str)
  (let ((var-memo (fset:empty-map))
        (varls-memo (fset:empty-map)))
    (labels ((var-matches-range (var start end &aux key)
               ;; Dynammic programming preamble
               (setf key (list var start end))
               (when (= start end)
                 (return-from var-matches-range nil))
               (multiple-value-bind (val done) (fset:@ var-memo key)
                 (when done
                   (return-from var-matches-range val)))
               ;; Actual logic
               (setf (fset:@ var-memo key)
                     (if (stringp var)
                         (equalp var (str:substring start end str))
                         (loop for prod in (fset:@ rule-map var)
                                 thereis (varls-matches-range prod start end)))))
             (varls-matches-range (var-ls start end &aux key)
               ;; DP preamble
               (setf key (list var-ls start end))
               (when (or (null var-ls) (= start end))
                 (return-from varls-matches-range (and (null var-ls) (= start end))))
               (multiple-value-bind (val done) (fset:@ varls-memo key)
                 (when done
                   (return-from varls-matches-range val)))
               ;; Actual logic
               (setf (fset:@ varls-memo key)
                     (destructuring-bind (hd-var . tl-vars) var-ls
                       (loop for sep from 1 upto end
                               thereis (and (var-matches-range hd-var start sep)
                                            (varls-matches-range tl-vars sep end)))))))
      (varls-matches-range var-ls 0 (length str)))))

(defun day-19a ()
  ;; takes 30 seconds or so
  (multiple-value-bind (rules lines) (parse-input)
    (loop for line in lines
          counting (varls-matches rules (list 0) line))))

(defun day-19b ()
  ;; Probably took about five minutes
  (multiple-value-bind (rules lines) (parse-input)
    (setf (fset:@ rules 8) '((42) (42 8)))
    (setf (fset:@ rules 11) '((42 31) (42 11 31)))
    (loop for line in lines
          counting (varls-matches rules (list 0) line))))
