(uiop:define-package :advent/src/day-1
  (:use #:cl #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-1a
   #:solve-1b))

(in-package :advent/src/day-1)

(defun get-numlist ()
  (let* ((input-file-contents (alx:read-file-into-string "../inputs/1.txt"))
         (numstr-list (split-sequence:split-sequence #\newline input-file-contents)))
    (iter (for numstr in numstr-list)
      (until (zerop (length numstr)))
      (collect (parse-integer numstr)))))

(defun nums-in-list-summing-to (num-list sum)
  ;; This is just an inefficient implementation of the classic "two sum" problem: inefficient
  ;; because you don't need the hash table at all.
  ;; See https://web.stanford.edu/class/cs9/sample_probs/TwoSum.pdf for the "proper" solution...
  ;;     The reason why it works wasn't immediately obvious to me, but you can prove that it works
  ;;     by showing that if there are n1, n2 such that A[n1] + A[n2] = desired sum, then it is
  ;;     impoosible for i or j to move inside the [n1, n2] range while the other is outside
  ;;
  ;; As an addendum, two-sum with duplicates is easy: just map numbers to the list of indices where
  ;; they appear, two-sum a deduplicated array to get a list of summands, and just go through
  ;; that list and calculate the cartesian product of the relevant indices at each step
  (let ((num-set (make-hash-table :test #'equalp)))
    (iter (for num in num-list)
          (for complement = (- sum num))
          (if (gethash complement num-set)
              (return-from nums-in-list-summing-to (cons num complement))
              (setf (gethash num num-set) t)))))

(defun solve-1a ()
  (destructuring-bind (n1 . n2) (nums-in-list-summing-to (get-numlist) 2020)
    (* n1 n2)))

(defun solve-1b ()
  (iter (for list on (get-numlist))
    (for (head t1 t2) = list)
    (while t2)
    (for result = (nums-in-list-summing-to (cdr list) (- 2020 head)))
    (if result
        (destructuring-bind (n1 . n2) result
          (return-from solve-1b (* head n1 n2))))))
