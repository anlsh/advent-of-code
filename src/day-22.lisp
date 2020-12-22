
(uiop:define-package :advent/src/day-22
  (:use #:cl)
  (:export
   #:day-22a
   #:day-22b))

(in-package :advent/src/day-22)

(defun get-decks (file)
  (loop for pspec in (split-sequence:split-sequence "" (uiop:read-file-lines file)
                                                    :test #'equalp)
        for cardls = (cdr pspec)
        collect (mapcar #'parse-integer cardls)))

(defun score (d)
  (loop for factor from 1
        for card in (reverse d)
        summing (* factor card)))

(defun play-it-game (d1 d2)
  (loop while (and d1 d2)
        for c1 = (pop d1)
        for c2 = (pop d2)
        for winner = (> c1 c2)
        do (if winner
               (fset:appendf d1 (list c1 c2))
               (fset:appendf d2 (list c2 c1))))
  (or d1 d2))

(defun play-rec-game (d1 d2)
  (loop with seen-rounds = (fset:empty-set)
        while (and d1 d2)
        for key = (cons (copy-list d1) (copy-list d2))
        do (if (fset:contains? seen-rounds key)
               (return-from play-rec-game (values t d1))
               (fset:adjoinf seen-rounds key))
           (let ((c1 (pop d1))
                 (c2 (pop d2)))
             (if (if (or (> c1 (length d1))
                         (> c2 (length d2)))
                     (> c1 c2)
                     (play-rec-game (subseq d1 0 c1)
                                    (subseq d2 0 c2)))
                 (fset:appendf d1 (list c1 c2))
                 (fset:appendf d2 (list c2 c1)))))
  (values d1 (or d1 d2)))

(defun day-22a (&optional (file "../inputs/22.txt"))
  (destructuring-bind (d1 d2) (get-decks file)
    (score (play-it-game d1 d2))))

(defun day-22b (&optional (file "../inputs/22.txt"))
  (destructuring-bind (d1 d2) (get-decks file)
    (score (second (multiple-value-list (play-rec-game d1 d2))))))
