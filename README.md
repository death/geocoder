# Geocoder

Convert place names to geodesic coordinates and vice versa.

# Example

```lisp
CL-USER> (geocoder:geocode (make-instance 'geocoder:osm-geocoder)
                           "Sognsvann, Oslo")
(#<GEOCODER/GEOCODER:PLACE "Sognsvann, Marka, Oslo, Norge" (59.9751304 10.729231051836638)>
 #<GEOCODER/GEOCODER:PLACE "Sognsvann, Sognsveien, Holtet, Nordre Aker, Oslo, 0863, Norge" (59.9671305 10.7340042)>
 #<GEOCODER/GEOCODER:PLACE "Sognsvann, Sognsveien, Holtet, Nordre Aker, Oslo, 0863, Norge" (59.9671262 10.7339377)>)
CL-USER> (geocoder:reverse-geocode (make-instance 'geocoder:osm-geocoder)
                                   '(:lat 59.9671304d0 :lon 10.7340042d0))
(#<GEOCODER/GEOCODER:PLACE "Sognsvann, Sognsveien, Holtet, Nordre Aker, Oslo, 0863, Norge" (59.9671305 10.7340042)>)
```

# License

MIT
