(uiop:define-package :advent/src/day-8
  (:use #:cl)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-8a
   #:day-8b))

(in-package :advent/src/day-8)

(defun get-ops ()
  (apply #'vector (uiop:read-file-lines "../inputs/8.txt")))

(defun simulate-until-termination (ops flip-fun)
  (loop with acc = 0
        with iptr = 0
        with visited = (fset:empty-set)
        until (or (>= iptr (length ops))
                  (fset:contains? visited iptr))
        for op = (aref ops iptr)
        do
           (fset:adjoinf visited iptr)
           (ppcre:register-groups-bind (opcode (#'parse-integer oparg)) ("(.{3}) (.*)" op)
             (alx:switch ((funcall flip-fun opcode iptr) :test #'equalp)
               ("nop" (incf iptr))
               ("jmp" (incf iptr oparg))
               ("acc" (incf acc oparg) (incf iptr))))
        finally
           (return (values acc (= iptr (length ops))))))

(defun day-8a ()
  (simulate-until-termination (get-ops) (lambda (op iptr) (declare (ignore iptr)) op)))

;; Just a brute force lol
;; I did just realize how to do the second part though

;;     1. Create a graph `G` where the nodes correspond to lines of code and the edges take each
;;     node from the node to the next node which would be executed.

;;     2. Create a copy `G'` of `G`, and for each `jmp`/`nop` instruction in `G` insert an edge to
;;     the node in `G'` which you *would* have gotten to had the opcode been flipped

;;     3. Pathfind from node 0 in `G` to node (number-of-instructions) in `G'`, then back up the
;;     path (in the bfs or whatever, keep track of every node's parent) and simulate the
;;     accumulation stuff

(defun day-8b ()
  (loop with ops = (get-instructions)
        for linum below (length ops)
        for flip-fun = (lambda (opcode iptr)
                         (if (= iptr linum)
                             (alx:switch (opcode :test #'equalp) ("nop" "jmp") ("jmp" "nop"))
                             opcode))
        do (multiple-value-bind
                 (acc normal-term) (simulate-until-termination ops flip-fun)
             (when normal-term (return acc)))))
