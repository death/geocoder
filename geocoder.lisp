;;;; +----------------------------------------------------------------+
;;;; | Geocoder                                                       |
;;;; +----------------------------------------------------------------+

(defpackage #:geocoder/geocoder
  (:use #:cl)
  (:import-from
   #:dexador)
  (:import-from
   #:quri
   #:url-encode)
  (:import-from
   #:com.gigamonkeys.json
   #:parse-json)
  (:import-from
   #:parse-number
   #:parse-number)
  (:export
   #:place
   #:name
   #:lat
   #:lon
   #:geocoder
   #:geocode*
   #:reverse-geocode*
   #:osm-geocoder
   #:*geocoder*
   #:geocode
   #:reverse-geocode))

(in-package #:geocoder/geocoder)

(defclass place ()
  ((name :initform nil :reader name)
   (lat :initform nil :reader lat)
   (lon :initform nil :reader lon)
   (raw :initarg :raw :reader raw)))

(defmethod print-object ((place place) stream)
  (print-unreadable-object (place stream :type t)
    (with-slots (name lat lon) place
      (format stream "~S~@[ (~F ~F)~]" name lat lon)))
  place)

(defclass geocoder ()
  ())

(defgeneric geocode* (geocoder value))

(defgeneric reverse-geocode* (geocoder query))

;; OpenStreetMap geocoder

(defclass osm-geocoder (geocoder)
  ((endpoint :initarg :endpoint :reader endpoint))
  (:default-initargs :endpoint "https://nominatim.openstreetmap.org"))

(defmethod search-endpoint ((geocoder osm-geocoder))
  (concatenate 'string (endpoint geocoder) "/search"))

(defmethod reverse-endpoint ((geocoder osm-geocoder))
  (concatenate 'string (endpoint geocoder) "/reverse"))

(defmethod geocode* ((geocoder osm-geocoder) value)
  (parse-response geocoder
                  (http-request (search-endpoint geocoder)
                                (geocode-params geocoder value))))

(defmethod geocode-params ((geocoder osm-geocoder) value)
  (append (force-params geocoder)
          (value-params geocoder value)
          (common-params geocoder)))

(defmethod force-params ((geocoder osm-geocoder))
  (list :format "json"
        :addressdetails 1))

(defmethod value-params ((geocoder osm-geocoder) (value string))
  (list :q value))

(defmethod value-params ((geocoder osm-geocoder) (value list))
  value)

(defmethod common-params ((geocoder osm-geocoder))
  (list))

(defun http-request (endpoint params)
  (dex:get
   (with-output-to-string (out)
     (write-string endpoint out)
     (loop for prefix = "?" then "&"
           for (key value) on params by #'cddr
           do (write-string prefix out)
              (write-string (string-downcase key) out)
              (write-string "=" out)
              (write-string (url-encode (typecase value
                                          (string value)
                                          (double-float (format nil "~F" value))
                                          (t (princ-to-string value)))
                                        :space-to-plus t)
                            out)))))

(defmethod parse-response ((geocoder osm-geocoder) response)
  (let ((json (parse-json response)))
    (if (vectorp json)
        (map 'list #'parse-place json)
        (list (parse-place json)))))

(defun parse-place (raw-place)
  (let ((place (make-instance 'place :raw raw-place)))
    (with-slots (lon lat name) place
      (loop for (key value) on raw-place by #'cddr
            do (cond ((equal key "display_name")
                      (setf name value))
                     ((equal key "lat")
                      (setf lat (parse-number value :float-format 'double-float)))
                     ((equal key "lon")
                      (setf lon (parse-number value :float-format 'double-float))))))
    place))

(defmethod reverse-geocode* ((geocoder osm-geocoder) query)
  (parse-response geocoder
                  (http-request (reverse-endpoint geocoder)
                                (geocode-params geocoder query))))

;; Convenience

(defvar *geocoder*
  (make-instance 'osm-geocoder))

(defun geocode (value &key (geocoder *geocoder*))
  (geocode* geocoder value))

(defun reverse-geocode (query &key (geocoder *geocoder*))
  (reverse-geocode* geocoder query))
