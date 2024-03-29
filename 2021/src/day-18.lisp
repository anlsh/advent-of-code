(uiop:define-package :advent/2021/src/day-18
  (:use #:cl #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria) (#:pq #:priority-queue))
  (:shadowing-import-from fset
   :@))

(in-package :advent/2021/src/day-18)

(defclass snail-num ()
  ((depth :accessor snum-depth :initarg :depth)
   (val :accessor snum-val :initarg :val :initform nil)
   (left :accessor snum-left :initarg :left :initform nil)
   (right :accessor snum-right :initarg :right :initform nil)
   (parent :accessor snum-parent :initarg :parent :initform nil)))

(defun build-tree-with-parents (snum-listy parent depth)
  (let ((snum (make-instance 'snail-num :depth depth)))
    (setf (snum-parent snum) parent)
    (if (numberp snum-listy)
        (setf (snum-val snum) snum-listy)
        (progn (setf (snum-left snum) (build-tree-with-parents (car snum-listy) snum (1+ depth)))
               (setf (snum-right snum) (build-tree-with-parents (cadr snum-listy) snum (1+ depth)))))
    snum))

(defun snum-from-str (str)
  (-<> str
    (str:replace-all "[" "(" <>)
    (str:replace-all "]" ")" <>)
    (str:replace-all "," " " <>)
    (read-from-string <>)
    (build-tree-with-parents <> nil 1)))

(defmethod snum-number? ((snum snail-num))
  (snum-val snum))

(defun snum-to-tree (snum)
  (if (snum-number? snum)
      (snum-val snum)
      (list (snum-to-tree (snum-left snum))
            (snum-to-tree (snum-right snum)))))

(defun get-input (fname)
  (-<> (uiop:read-file-lines fname)
    (mapcar #'snum-from-str <>)))

(defun copy-snum (snum parent)
  (if snum
      (let ((snum-copy
              (make-instance 'snail-num
                             :depth (snum-depth snum)
                             :val (snum-val snum)
                             :parent parent)))
        (setf (snum-left snum-copy) (copy-snum (snum-left snum) snum-copy))
        (setf (snum-right snum-copy) (copy-snum (snum-right snum) snum-copy))
        snum-copy)
      nil))

(defun mutating-snum-traverse (snum down-going-left? incf-val)
  (let ((down-fn (if down-going-left?
                     #'snum-left #'snum-right))
        (pchild (if down-going-left?
                    #'snum-right #'snum-left)))
    (loop with snum = snum
          for snum-parent = (snum-parent snum)
          when (not snum-parent)
            do (return nil)
          when (eq snum (funcall down-fn snum-parent))
            do (return (loop with snum = (funcall pchild snum-parent)
                             for next-snum = (funcall down-fn snum)
                             while next-snum
                             do (setf snum next-snum)
                             finally
                                (incf (snum-val snum) incf-val)))
          do
             (setf snum snum-parent))))

(defmethod snum-add ((a snail-num) (b snail-num))
  (labels ((incf-snum-depths (snum)
             (when snum
               (incf (snum-depth snum))
               (incf-snum-depths (snum-left snum))
               (incf-snum-depths (snum-right snum))))
           (get-explodable (snum)
             (unless (snum-number? snum)
               (if (and (> (snum-depth snum) 4)
                        (snum-number? (snum-left snum))
                        (snum-number? (snum-right snum)))
                   (return-from get-explodable snum))
               (or (get-explodable (snum-left snum))
                   (get-explodable (snum-right snum)))))
           (get-splittable (snum)
             (if (snum-number? snum)
                 (if (>= (snum-val snum) 10)
                     snum)
                 (or (get-splittable (snum-left snum))
                     (get-splittable (snum-right snum)))))
           (reduce-snail-num (snum)
             (tagbody
              explode-tag
                (loop for explode-node = (get-explodable snum)
                      while explode-node
                      do (mutating-snum-traverse explode-node
                                                 nil
                                                 (snum-val (snum-left explode-node)))
                         (mutating-snum-traverse explode-node
                                                 t
                                                 (snum-val (snum-right explode-node)))
                         (setf (snum-left explode-node) nil
                               (snum-right explode-node) nil
                               (snum-val explode-node) 0))
              split-tag
                (let ((split-node (get-splittable snum)))
                  (when split-node
                    (let ((depth (snum-depth split-node))
                          (val (snum-val split-node)))
                      (setf (snum-left split-node) (make-instance 'snail-num :val (floor (/ val 2))
                                                                             :depth (1+ depth)
                                                                             :parent split-node)
                            (snum-right split-node) (make-instance 'snail-num :val (ceiling (/ val 2))
                                                                              :depth (1+ depth)
                                                                              :parent split-node)
                            (snum-val split-node) nil))
                    (go explode-tag))))))
    (let ((new-root (make-instance 'snail-num
                                   :left (copy-snum a nil)
                                   :right (copy-snum b nil)
                                   :depth 0)))
      (setf (snum-parent (snum-left new-root)) new-root
            (snum-parent (snum-right new-root)) new-root)
      (incf-snum-depths new-root)
      (reduce-snail-num new-root)
      new-root)))

(defun magnitude (snum)
  (if (snum-number? snum)
      (snum-val snum)
      (+ (* 3 (magnitude (snum-left snum)))
         (* 2 (magnitude (snum-right snum))))))

(defun solve-17a (fname)
  (let ((final-snum (reduce #'snum-add (get-input fname))))
    (magnitude final-snum)))

(defun solve-17b (fname)
  (let ((snums (get-input fname)))
    (loop for (snum1 snum2) in (loop for s1 in snums appending (loop for s2 in snums
                                                                     when (not (eq s1 s2))
                                                                       collecting (list s1 s2)))
          maximizing (magnitude (snum-add snum1 snum2)))))
