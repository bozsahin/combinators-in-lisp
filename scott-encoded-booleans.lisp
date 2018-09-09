;; this is an example to implement Scott encoding of booleans 
;; load the file and do (or true false) to get K
;; -cem bozsahin

(setf or '#$(#&s #&s (#&k (#&k #&k))))
(setf true '#&k)
(setf false '#$(#&k (#&s #&k #&k)))
