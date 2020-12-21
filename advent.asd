;;;;; advent.asd

(asdf:defsystem :advent
  :description "Common Lisp Advent of Code 2020"
  :author "Anish Moorthy (anlsh@protonmail.com)"
  :license  "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:advent/package
               :alexandria
               :arrow-macros
               :bit-smasher
               :cl-ppcre
               :fset
               :iterate
               :picl
               :queues
               :split-sequence
               :str
               :uiop))
