;; this is an example to show Y combinator in action
;; load the file and do (comb-eval '#&y fac 4) to get factorial of 4
;; -cem bozsahin

(setf true '(lam a (lam b a)))  ; Church-encodings directly in lam notation for simplicity
(setf false '(lam a (lam b b)))
(setf myif '(lam p (lam a (lam b ((p a) b)))))
(setf fac '(lam f (lam n ())))
