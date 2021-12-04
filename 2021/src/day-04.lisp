(uiop:define-package :advent/2021/src/day-04
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-4a
   #:solve-4b))

(in-package :advent/2021/src/day-04)

(defparameter *input* (uiop:read-file-lines "../inputs/04.txt"))

(defparameter *nums* (mapcar #'parse-integer (str:split "," (car *input*))))

(defparameter *boards*
  (-<> (cddr *input*)
    (split-sequence:split-sequence (cadr *input*) <> :test #'equalp)
    (mapcar (lambda (lines)
              (mapcar (lambda (line)
                        (-<> line
                          (str:trim <>)
                          (str:split " " <>)
                          (remove-if (lambda (s) (equalp s ""))
                                     <>)
                          (mapcar #'parse-integer <>))
                        )
                      lines))
            <>)))

(defparameter *board-size* (length (caar *boards*)))

(defun score (bnum called-nums just-called-num)
  (* just-called-num
     (-<> bnum
       (elt *boards* <>)
       (apply #'append <>)
       (remove-if (lambda (n) (fset:contains? called-nums n)) <>)
       (reduce #'+ <>))))

(defun solve-4 ()
  (let ((num-to-pos-maps (fset:empty-map (fset:empty-set)))
        (board-to-rowcounts (fset:empty-map))
        (board-to-colcounts (fset:empty-map)))
    (loop for board in *boards*
          for bnum from 0
          do (setf (fset:@ board-to-rowcounts bnum) (make-array *board-size* :initial-element 0))
             (setf (fset:@ board-to-colcounts bnum) (make-array *board-size* :initial-element 0))
             (loop for row in board
                   for rownum from 0
                   do (loop for num in row
                            for colnum from 0
                            do (fset:unionf (fset:@ num-to-pos-maps num)
                                            (fset:set (list bnum rownum colnum))))))
    (loop with called-nums = (fset:empty-set)
          with winning-boards = (fset:empty-set)
          with first-winning-score = nil
          with last-winning-score = nil
          for num in *nums*
          do (fset:adjoinf called-nums num)
             (loop for (bnum rnum cnum) in (fset:convert 'list (fset:@ num-to-pos-maps num))
                   do (when (or (equalp *board-size*
                                        (incf (aref (fset:@ board-to-rowcounts bnum) rnum)))
                                (equalp *board-size*
                                        (incf (aref (fset:@ board-to-colcounts bnum) cnum))))
                        (let ((new-score (score bnum called-nums num)))
                          (unless (fset:contains? winning-boards bnum)
                            (unless first-winning-score (setf first-winning-score new-score))
                            (setf last-winning-score new-score))
                          (fset:adjoinf winning-boards bnum))))
          finally
             (return (list first-winning-score last-winning-score)))))
