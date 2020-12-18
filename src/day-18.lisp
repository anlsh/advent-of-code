
(uiop:define-package :advent/src/day-18
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-18a
   #:day-18b))

(in-package :advent/src/day-18)

(defun tokenize-line (line)
  (-<> line
    (str:replace-all "(" " ( " <>)
    (str:replace-all ")" " ) " <>)
    (str:split " " <>)
    (remove-if (lambda (x) (equalp x ""))
               <>)))

(defun eval-tree (tree)
  (when (numberp tree)
    (return-from eval-tree tree))
  (when (= (length tree) 1)
    (return-from eval-tree (eval-tree (car tree))))
  (destructuring-bind (fn-sym . args) tree
    (apply (symbol-function fn-sym) (mapcar #'eval-tree args))))

(labels ((tokens-to-tree (stream-fn)
           (loop for token = (funcall stream-fn)
                 with acc = nil
                 when (or (null token)
                          (equalp token "("))
                   do (return acc)
                 when (equalp token ")")
                   do (setf acc (tokens-to-tree stream-fn))
                 when (or (equalp token "+") (equalp token "*"))
                   do (return (list (intern token) acc (tokens-to-tree stream-fn)))
                 when (parse-integer token :junk-allowed t)
                   do (setf acc (parse-integer token))))
         (tree-from-str (str)
           (let ((tokens (reverse (tokenize-line str))))
             (tokens-to-tree (lambda () (pop tokens))))))

  (defun day-18a ()
    (loop for line in (uiop:read-file-lines "../inputs/18.txt")
          summing (eval-tree (tree-from-str line)))))

(labels
    ((tokens-to-tree (stream-fn)
       (loop for token = (funcall stream-fn)
             when (or (null token)
                      (equalp token ")"))
               do (return acc)
             when (equalp token "(")
               collect (tokens-to-tree stream-fn) into acc
             when (or (equalp token "+") (equalp token "*"))
               collect (intern token) into acc
             when (parse-integer token :junk-allowed t)
               collect (parse-integer token) into acc))
     (tokens-to-multree (tree)
       (cond ((numberp tree) tree)
             ((= (length tree) 1) (tokens-to-multree (car tree)))
             ((find '* tree)
              (cons '* (mapcar #'tokens-to-multree (split-sequence:split-sequence '* tree))))
             ((find '+ tree)
              (cons '+ (mapcar #'tokens-to-multree (split-sequence:split-sequence '+ tree))))
             (t tree)))
     (tree-from-str (str)
       (let ((tokens (tokenize-line str)))
         (tokens-to-multree (tokens-to-tree (lambda () (pop tokens)))))))
  (defun day-18b ()
    (loop for line in (uiop:read-file-lines "../inputs/18.txt")
          summing (eval-tree (tree-from-str line)))))
