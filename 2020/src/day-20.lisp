(uiop:define-package :advent/2020/src/day-20
  (:use #:cl #:arrow-macros #:iterate)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:day-20a
   #:day-20b))

(in-package :advent/2020/src/day-20)

(defparameter *perms* (list #(0 1 2 3) #(3 0 1 2) #(2 3 0 1) #(1 2 3 0)))
(defparameter *counter-perms* (list #(0 3 2 1) #(1 0 3 2) #(2 1 0 3) #(3 2 1 0)))
(defparameter *popts* (append (mapcar (lambda (p) (list 0 p))
                                      *perms*)
                              (mapcar (lambda (p) (list 1 p))
                                       *counter-perms*)))
(defparameter *bigsize* nil)

(defun to-list (x)
  (coerce x 'list))

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
                (destructuring-bind (id-str . tile) spec
                  (ppcre:register-groups-bind
                      ((#'parse-integer id)) ("Tile (.*):" id-str)
                    (setf (fset:@ map id)
                          (funcall block-proc-fn tile)))))
              <>))
    (setf *bigsize* (round (sqrt (fset:size map))))
    map))

(defun find-valid-config (edge-info-map)
  ;; Classic backtracking approach to find a valid placement of the tiles
  (labels ((o+ (n) (mod (1+ n) 2))
           (align? (edge-code new-idpopt other-idpopt)
             (destructuring-bind (nid (nflip nperm)) new-idpopt
               (destructuring-bind (oid(oflip operm)) other-idpopt
                 (= (-<> edge-code
                      (aref nperm <>) (aref (fset:@ edge-info-map nid) <>) (aref <> nflip))
                    (-<> (mod (+ 2 edge-code) 4)
                      (aref operm <>) (aref (fset:@ edge-info-map oid) <>) (aref <> (o+ oflip)))))))
           (place-tile (row col remaining-tile-ids pos-tile-map)
             (when (zerop (fset:size remaining-tile-ids))
               (return-from find-valid-config pos-tile-map))
             (when (= col *bigsize*)
               (return-from place-tile (place-tile (1+ row) 0 remaining-tile-ids pos-tile-map)))
             (fset:do-set (id remaining-tile-ids)
               (loop
                 for popt in *popts*
                 when (and (or (zerop col)
                               (align? 3 (list id popt) (fset:@ pos-tile-map (list row (1- col)))))
                           (or (zerop row)
                               (align? 0 (list id popt) (fset:@ pos-tile-map (list (1- row) col)))))
                   do (place-tile row (1+ col)
                                  (fset:less remaining-tile-ids id)
                                  (fset:with pos-tile-map (list row col) (list id popt)))))))
    (place-tile 0 0 (fset:domain edge-info-map) (fset:empty-map))))

(defun day-20a ()
  (let* ((map-edges (get-inputs (lambda (rest)
                                  (get-edges (make-array '(10 10) :initial-contents rest)))))
         (solution (find-valid-config map-edges)))
    (values solution
            (-<> (list (list 0 0) (list 0 (1- *bigsize*)) (list (1- *bigsize*) 0)
                       (list (1- *bigsize*) (1- *bigsize*)))
              (mapcar (lambda (x) (fset:@ solution x)) <>)
              (mapcar #'car <>)
              (reduce #'* <>)))))

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
  (labels ((trim (x) "Shave off the ends of a sequence" (subseq x 1 (1- (length x)))))
    (iterate
      (with solution = (day-20a))
      (with block-map
            = (get-inputs (lambda (x)
                            (make-array '(8 8)
                                        :initial-contents (mapcar #'trim (trim x))))))
      (with big-map-size = (* *bigsize* 8))
      (with big-map = (make-array (list big-map-size big-map-size)))
      (for (rootx rooty) in-it (picl:map #'to-list (picl:nfold-product 2 (picl:range *bigsize*))))
      (for (id popt) = (fset:@ solution (list rootx rooty)))
      (iterate
        (with tform = (get-coord-tform popt 8))
        (with blk = (fset:@ block-map id))
        (for coord in-it (picl:map #'to-list (picl:nfold-product 2 (picl:range 8))))
        (let ((tcoord (funcall tform coord)))
          (setf (aref big-map
                      (+ (* 8 rootx) (elt tcoord 0))
                      (+ (* 8 rooty) (elt tcoord 1)))
                (aref blk (elt coord 0) (elt coord 1)))))
      (finally (return big-map)))))

(defvar *snake*
  #2A("                  # "
      "#    ##    ##    ###"
      " #  #  #  #  #  #   "))
(destructuring-bind (rows cols) (array-dimensions *snake*)
  (defvar *snake-nrows* rows)
  (defvar *snake-ncols* cols))
(defparameter *snake-parts*
  (iterate (for (locx locy)
                in-it (picl:map #'to-list (picl:product (picl:range (array-dimension *snake* 0))
                                                        (picl:range (array-dimension *snake* 1)))))
    (when (equalp #\# (aref *snake* locx locy))
      (collect (list locx locy)))))

(defun day-20b ()
  (let ((map (splice-map)))
    (destructuring-bind (nrows ncols) (array-dimensions map)
      (iterate (for pos in-it (picl:map #'to-list
                                        (picl:product (picl:range nrows) (picl:range ncols))))
        (loop for popt in *popts*
              for tform = (get-coord-tform popt 1)
              for spos-ls = (-<> *snake-parts*
                              (mapcar (lambda (x) (funcall tform x)) <>)
                              (mapcar (lambda (x) (mapcar #'+ pos x)) <>))
              for spos-bounded
                = (every (lambda (p) (every #'< '(-1 -1) p (list nrows ncols)))
                         spos-ls)
              when (and spos-bounded
                        (notany (lambda (p) (equalp #\. (apply #'aref map p)))
                               spos-ls))
                do (mapcar (lambda (p) (setf (apply #'aref map p) #\O))
                            spos-ls)))
      (iterate (for pos below (expt (* 8 *bigsize*) 2))
        (counting (equalp #\# (row-major-aref map pos)))))))
