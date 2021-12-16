;;;;; advent.asd

(asdf:defsystem :advent
  :description "Common Lisp Advent of Code 2020"
  :author "Anish Moorthy (anlsh@protonmail.com)"
  :license  "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:alexandria
               :arrow-macros
               :bit-smasher
               :cl-ppcre
               :fset
               :iterate
               :picl
               :picl/iterate
               :priority-queue
               :split-sequence
               :str
               :uiop

               :advent/2018/package
               :advent/2020/package
               :advent/2021/package))
