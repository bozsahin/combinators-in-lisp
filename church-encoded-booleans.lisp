;; this is an example to implement Church booleans 
;; load the file and do (comb-eval myor true false) to get (K K) viz \y\x\y2.x
;; the example is borrowed from Steve Piantadosi

(setf myor '(#&s #&s (#&k (#&k #&k))))
(setf true '(#&k #&k))
(setf false '#&k)
