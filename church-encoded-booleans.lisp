;; this is an example to implement Church booleans 
;; load the file and do (comb-eval myor true false) to get (K K) viz \y\x\y2.x
;; also try (comb-eval myor false false) which should give you K (false)
;; the example is borrowed from Steve Piantadosi

(setf myor `(,ss ,ss (,k (,k ,k))))
(setf true `(,k ,k))
(setf false `,k)
