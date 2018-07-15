;;;; +----------------------------------------------------------------+
;;;; | Geocoder                                                       |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:geocoder
  :class :package-inferred-system
  :defsystem-depends-on ("asdf-package-system")
  :depends-on ("geocoder/all"))
