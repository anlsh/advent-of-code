(uiop:define-package :advent/2021/src/day-02
  (:use #:cl #:iterate #:arrow-macros)
  (:local-nicknames (#:alx #:alexandria))
  (:export
   #:solve-2a
   #:solve-2b))

(in-package :advent/2021/src/day-02)

(defun solve-2a ()
  (loop for line in (uiop:read-file-lines "../inputs/02.txt")
        with depth = 0
        with hpos = 0
        do (ppcre:register-groups-bind
               (dir (#'parse-integer dist))
               ("(.*) (.*)" line)
             (alx:eswitch (dir :test #'equalp)
               ("forward" (incf hpos dist))
               ("down" (incf depth dist))
               ("up" (decf depth dist))))
        finally
           (return (* depth hpos))))

(defun solve-2b ()
  (loop for line in (uiop:read-file-lines "../inputs/02.txt")
        with depth = 0
        with hpos = 0
        with aim = 0
        do (ppcre:register-groups-bind
               (dir (#'parse-integer dist))
               ("(.*) (.*)" line)
             (alx:eswitch (dir :test #'equalp)
               ("forward" (incf hpos dist) (incf depth (* aim dist)))
               ("down" (incf aim dist))
               ("up" (decf aim dist))))
        finally
           (return (* depth hpos))))
