(uiop:define-package :advent/src/day-20
  (:use #:cl #:arrow-macros #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-20a
   #:day-20b))

(in-package :advent/src/day-20)

(defparameter *perms* (list #(0 1 2 3) #(3 0 1 2) #(2 3 0 1) #(1 2 3 0)))
(defparameter *counter-perms* (list #(0 3 2 1) #(1 0 3 2) #(2 1 0 3) #(3 2 1 0)))
(defparameter *popts* (append (mapcar (lambda (p) (list 0 p))
                                      *perms*)
                              (mapcar (lambda (p) (list 1 p))
                                       *counter-perms*)))
(defparameter *bigsize* 12)

(defun get-edges (arr)
  (labels ((getv (row col)
             (ecase (aref arr row col)
               (#\. 0)
               (#\# 1)))
           (nums-from-edge (ls)
             (let ((edge (make-array 10 :element-type 'bit :initial-contents ls)))
               (vector (bit-smasher:bits->int edge)
                       (bit-smasher:bits->int (reverse edge))))))
    (vector (nums-from-edge (loop for i below 10 collect (getv 0 i)))
            (nums-from-edge (loop for i below 10 collect (getv i 9)))
            (nums-from-edge (loop for i below 10 collect (getv 9 (- 9 i))))
            (nums-from-edge (loop for i below 10 collect (getv (- 9 i) 0))))))

(defun get-inputs (block-proc-fn)
  (let ((map (fset:empty-map)))
    (-<> (uiop:read-file-lines "../inputs/20.txt")
      (split-sequence:split-sequence "" <> :test #'equalp)
      (mapcar (lambda (spec)
                (destructuring-bind (id-str . rest) spec
                  (ppcre:register-groups-bind
                      ((#'parse-integer id)) ("Tile (.*):" id-str)
                    (setf (fset:@ map id)
                          (funcall block-proc-fn rest)))))
              <>))
    map))

(defun find-valid-config (edge-info-map)
  (labels ((o+ (n) (mod (1+ n) 2))
           (place-tile (row col remaining-tile-ids pos-tile-map)
             (when (zerop (fset:size remaining-tile-ids))
               (return-from find-valid-config pos-tile-map))
             (when (= col *bigsize*)
               (return-from place-tile (place-tile (1+ row) 0 remaining-tile-ids pos-tile-map)))
             (fset:do-set (id remaining-tile-ids)
               (loop
                 for popt in *popts*
                 for (flip perm) = popt
                 when (and (or (zerop col)
                               (= (-<> 3 (aref perm <>) (aref (fset:@ edge-info-map id) <>)
                                    (aref <> flip))
                                  (destructuring-bind
                                      (id (flip perm)) (fset:@ pos-tile-map (cons row (1- col)))
                                    (-<> 1 (aref perm <>) (aref (fset:@ edge-info-map id) <>)
                                      (aref <> (o+ flip))))))
                           (or (zerop row)
                               (= (-<> 0 (aref perm <>) (aref (fset:@ edge-info-map id) <>)
                                    (aref <> flip))
                                  (destructuring-bind
                                      (id (flip perm)) (fset:@ pos-tile-map (cons (1- row) col))
                                    (-<> 2 (aref perm <>) (aref (fset:@ edge-info-map id) <>)
                                      (aref <> (o+ flip)))))))
                   do (place-tile row (1+ col)
                                  (fset:less remaining-tile-ids id)
                                  (fset:with pos-tile-map (cons row col) (list id popt)))))))
    (place-tile 0 0 (fset:domain edge-info-map) (fset:empty-map))))

(defun day-20a ()
  (let* ((map-edges (get-inputs (lambda (rest) (get-edges (make-array '(10 10)
                                                                      :initial-contents rest)))))
         (solution (find-valid-config map-edges)))
    (values solution
            (-<> (list (cons 0 0) (cons 0 (1- *bigsize*)) (cons (1- *bigsize*) 0)
                       (cons (1- *bigsize*) (1- *bigsize*)))
              (mapcar (lambda (x) (fset:@ solution x)) <>)
              (mapcar #'car <>)
              (reduce #'* <>)))))

;; It's WRONG!!!
;; (funcall (get-coord-tform (list 1 #(0 3 2 1)) '(10 10)) '(2 0))
;; => '(2 0) but it SHOULD be  (2 9)
(defun get-coord-tform (popt edgelen)
  (labels ((rot-cwise (n-times pos)
              (loop for _ below n-times
                    do (setf pos (list (cadr pos)
                                       (- (1- edgelen) (car pos))))
                    finally (return pos))))
     (destructuring-bind (flip perm) popt
       (if (zerop flip)
           (let ((zpos (loop for z from 0 when (zerop (aref perm z)) do (return z))))
             (lambda (pos) (rot-cwise zpos pos)))
           (let ((tpos (loop for tp from 0 when (= 3 (aref perm tp)) do (return tp))))
             (lambda (pos) (rot-cwise tpos (reverse pos))))))))

(defun splice-map ()
  (iterate
    (with solution = (day-20a))
    (with block-map
          = (get-inputs (lambda (x)
                          (make-array '(8 8)
                                      :initial-contents (mapcar (lambda (x) (subseq x 1 (1- (length x))))
                                                                (subseq x 1 (1- (length x))))))))
    (with big-map-size = (* *bigsize* 8))
    (with big-map = (make-array (list big-map-size big-map-size)))
    (for root in-it (picl:nfold-product 2 (picl:range *bigsize*)))
    (for rootx = (aref root 0))
    (for rooty = (aref root 1))
    (for (id popt) = (fset:@ solution (cons rootx rooty)))
    (iterate
      (with tform = (get-coord-tform popt 8))
      (with blk = (fset:@ block-map id))
      (for coord in-it (picl:nfold-product 2 (picl:range 8)))
      (let ((tcoord (funcall tform (coerce coord 'list))))
        (setf (aref big-map
                    (+ (* 8 rootx) (elt tcoord 0))
                    (+ (* 8 rooty) (elt tcoord 1)))
              (aref blk (elt coord 0) (elt coord 1)))))
    (finally (return big-map))))

(defvar *snake-lines*
  (list "                  # "
        "#    ##    ##    ###"
        " #  #  #  #  #  #   "))
(defvar *snake-nrows* (length *snake-lines*))
(defvar *snake-ncols* (length (elt *snake-lines* 0)))
(defparameter *snake-parts*
  (iterate (for loc in-it (picl:product (picl:range *snake-nrows*)
                                        (picl:range *snake-ncols*)))
    (for locx = (aref loc 0))
    (for locy = (aref loc 1))
    (when (equalp #\# (elt (elt *snake-lines* locx) locy))
      (collect (list locx locy)))))

(defun day-20b ()
  (let ((map (splice-map)))
    (destructuring-bind (nrows ncols) (array-dimensions map)
      (iterate (for pos in-it (picl:product (picl:range nrows)
                                            (picl:range ncols)))
        (loop for popt in *popts*
              for tform = (get-coord-tform popt 1)
              for spos-ls = (-<> *snake-parts*
                              (mapcar (lambda (x) (funcall tform x)) <>)
                              (mapcar
                               (lambda (x) (list (+ (elt pos 0) (elt x 0))
                                                 (+ (elt pos 1) (elt x 1))))
                               <>))
              for spos-bounded
                = (every (lambda (p)
                           (destructuring-bind (row col) p
                             (and (< -1 row nrows)
                                  (< -1 col ncols))))
                         spos-ls)
              when (and spos-bounded
                        (every (lambda (p)
                                 (not (equalp #\. (apply #'aref map p))))
                               spos-ls))
                do (progn (print "found a monster!")
                          (mapcar (lambda (p)
                                    (destructuring-bind (row col) p
                                      (setf (aref map row col) #\O)))
                                  spos-ls))))
      (iterate (for pos in-it (picl:product (picl:range nrows)
                                            (picl:range ncols)))
        (counting (equalp #\# (picl:apply #'aref map pos)))))))
