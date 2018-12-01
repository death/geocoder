;;;; +----------------------------------------------------------------+
;;;; | Geocoder                                                       |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:geocoder
  :description "Convert place names to geodesic coordinates and vice versa"
  :author "death <github.com/death>"
  :license "MIT"
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("geocoder/all"))
