
(uiop:define-package :advent/2020/src/day-16
  (:use #:cl #:arrow-macros)
  (:local-nicknames)
  (:export
   #:day-16a
   #:day-16b))

(in-package :advent/2020/src/day-16)

(defun num-valid-for-rule? (num rule)
  (destructuring-bind (_ i0 i1 t0 t1) rule
    (declare (ignore _))
    (or (<= i0 num i1)
        (<= t0 num t1))))

(defun rule-valid-for-tickets? (rule pos tickets)
  (loop for ticket across tickets
        always (num-valid-for-rule? (aref ticket pos) rule)))

(defun ticket-valid-for-rules? (ticket rules)
  (loop with ticket-valid = t
        for num across ticket
        for num-valid = (loop for rule across rules
                                thereis (num-valid-for-rule? num rule))
        summing (if num-valid 0 num) into err-rate
        do (setf ticket-valid (and ticket-valid num-valid))
        finally (return (values ticket-valid err-rate))))

(defun get-inputs ()
  (destructuring-bind
      (field-file my-ticket-file nearby-file)
      (split-sequence:split-sequence "" (uiop:read-file-lines "../inputs/16.txt")
                                     :test #'equalp)
    (labels ((tline-to-ticket (tline)
               (apply #'vector (mapcar #'parse-integer (str:split "," tline)))))
      (list (apply #'vector
                   (loop for line in field-file
                         collect (ppcre:register-groups-bind (fname (#'parse-integer i0 i1 t0 t1))
                                     ("(.*): (\\d+)-(\\d+) or (\\d+)-(\\d+)" line)
                                   (list fname i0 i1 t0 t1))))
            (tline-to-ticket (cadr my-ticket-file))
            (apply #'vector (mapcar #'tline-to-ticket (cdr nearby-file)))))))

(defun day-16a ()
  (destructuring-bind (rules my-ticket nearby-tickets) (get-inputs)
    (declare (ignore my-ticket))
    (loop for ticket across nearby-tickets
          for (valid errs) = (multiple-value-list (ticket-valid-for-rules? ticket rules))
          summing errs)))

(defun get-valid-tickets (tickets rules)
  (remove-if-not (lambda (ticket) (ticket-valid-for-rules? ticket rules))
                 tickets))

(defun get-rule-positions (tickets rules)
  (let* ((n (length (elt tickets 0)))
         (pos-possibs (make-array n))
         (rule-map (fset:empty-map)))
    (loop for i below n
          do (-<> i
               (loop for rule across rules
                     when (rule-valid-for-tickets? rule <> tickets)
                       collect rule)
               (fset:convert 'fset:set <>)
               (setf (aref pos-possibs i) <>)))
    (loop while (< (fset:size rule-map) n)
          do (loop for i below n
                   for possibs = (fset:set-difference (aref pos-possibs i)
                                                      (fset:domain rule-map))
                   until (= 1 (fset:size possibs))
                   finally
                      (setf (fset:@ rule-map (car (fset:convert 'list possibs)))
                            i)))
    rule-map))

(defun day-16b ()
  (destructuring-bind (rules my-ticket nearby-tickets) (get-inputs)
    (-<> nearby-tickets
      (get-valid-tickets <> rules)
      (get-rule-positions <> rules)
      (fset:convert 'list <>)
      (mapcar (lambda (rulespec)
                (if (str:starts-with? "departure" (caar rulespec))
                    (aref my-ticket (cdr rulespec))
                    1))
              <>)
      (reduce #'* <>))))
