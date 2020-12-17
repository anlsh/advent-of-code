;;;;; advent.asd

(asdf:defsystem :advent
  :description "Common Lisp Advent of Code 2020"
  :author "Anish Moorthy (anlsh@protonmail.com)"
  :license  "MIT"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:uiop :alexandria :split-sequence
                     :picl
                     :iterate :cl-ppcre :str :fset
               :arrow-macros :advent/package))
