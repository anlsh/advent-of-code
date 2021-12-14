(uiop:define-package :advent/2021/src/day-14
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-14)

(defparameter *input* (uiop:read-file-lines "../inputs/14.txt"))

(defparameter *initstr* (car *input*))
(defparameter *rules*
  (let ((rules (fset:empty-map "")))
    (mapcar (lambda (line) (ppcre:register-groups-bind (e1 e2 p) ("(.)(.) -> (.)" line)
                             (fset:adjoinf rules (concatenate 'string e1 e2) p)))
            (cddr *input*))
    rules))

(defun step-string (str)
  (loop with new-str = ""
        for (a b) on (loop for c across str collecting (string c))
        for key = (concatenate 'string a b)
        while b
        do (setf new-str
                 (concatenate 'string new-str (if (fset:contains? (fset:domain *rules*) key)
                                                  (concatenate 'string (string a)
                                                               (fset:@ *rules* key))
                                                  (string a))))
        finally (return (concatenate 'string new-str (string a)))))

(defun step-string-for-steps (string num-steps)
  (let ((memo-map (fset:empty-map)))
    (labels ((between-chars (s1 s2 num-steps)))
      (let* ((str-key (concatenate 'string s1 s2))
             (key (list str-key num-steps)))
        (unless (fset:contains? memo-map key)
          (setf (fset:@ memo-map key)
                (cond ((= num-steps 0) "")
                      (t (loop with new-str = ""
                               for (a b) on (loop for c across (concatenate 'string a (between-chars a b (1- num-steps)) b) collecting (string c))
                               for key = (concatenate 'string a b)
                               while b
                               do (setf new-str
                                        (concatenate 'string new-str (concatenate'string a (fset:@ *rules* key))))
                               finally (return (subseq 1 new-str))))
                      )))
        (fset:@ memo-map key))
      )))

(defun string-diffcounts (str)
  (let* ((letter-count-map (fset:convert 'fset:map (fset:convert 'fset:bag str))))
    (- (fset:reduce #'max (fset:range letter-count-map))
       (fset:reduce #'min (fset:range letter-count-map)))))

(defun solve-14a ()
  (string-diffcounts (loop with curr-str = *initstr*
                           for k below 10
                           do (setf curr-str (step-string curr-str))
                           finally (return curr-str))))
