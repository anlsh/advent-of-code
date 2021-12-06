;; Documentation

;; Fset maps
(defparameter *map* (fset:map ("foo" 2))) ;; => {"foo": 3}
;; Can init empty maps with default value, very useful w/ fset's immutable stuff
(defparameter *default-map* (fset:empty-map 'empty-val))

;; Map addition, out-of-place
(fset:with *map* "bar" 4)
;; And "in place"
(fset:adjoinf *map* "baz" 3)

;; Fset Bags
;; Initialization
(fset:empty-bag)
(defparameter *bag* (fset:bag :a :b :x :y :x))
(fset:multiplicity *bag* :x)
