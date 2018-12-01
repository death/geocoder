# Geocoder

Convert place names to geodesic coordinates and vice versa.

# Example

```lisp
CL-USER> (geocoder:geocode (make-instance 'geocoder:osm-geocoder)
                           "Sognsvann, Oslo")
(#<GEOCODER/GEOCODER:PLACE "Sognsvann, Sognsveien, Kringsjå, Nordre Aker, Oslo, 0863, Norge" (59.96713 10.734004)>
 #<GEOCODER/GEOCODER:PLACE "Sognsvann, Ankerveien, Marka, Oslo, 0863, Norge" (59.975132 10.729231)>
 #<GEOCODER/GEOCODER:PLACE "Sognsvann, Sognsveien, Kringsjå, Nordre Aker, Oslo, 0863, Norge" (59.969307 10.734298)>)
CL-USER> (geocoder:reverse-geocode (make-instance 'geocoder:osm-geocoder)
                                   '(:lat 59.96713 :lon 10.734004))
(#<GEOCODER/GEOCODER:PLACE "Sognsvann st, Sognsveien, Kringsjå, Nordre Aker, Oslo, 0863, Norge" (59.96748 10.733345)>)
```

# License

MIT
