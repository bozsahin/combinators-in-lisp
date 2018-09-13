;; this is an example to show Y combinator in action
;; load the file and do (comb-eval '#&y ch-h ch-3) to get factorial of 3
;;
;; Factorial is functionally FAC n= H FAC n, where H=\f\n.(if (= n 0) 1 (* n f (- n 1)))
;;   therefore FAC n = Y H n
;; All of these are encoded as Church functions
;; -cem bozsahin

;; backquoted ones do the equivalent of text substitution

(defconstant  ch-0 '(lam f (lam x x)))
(defconstant  ch-1 '(lam f (lam x (f x))))
(defconstant  ch-2 '(lam f (lam x (f (f x)))))
(defconstant  ch-3 '(lam f (lam x (f (f (f x))))))
(defconstant  ch-4 '(lam f (lam x (f (f (f (f x)))))))
(defconstant  ch-5 '(lam f (lam x (f (f (f (f (f x))))))))
(defconstant  ch-6 '(lam f (lam x (f (f (f (f (f (f x)))))))))
(defconstant  ch-mult '(lam m (lam n (lam f #$(m (n f))))))
(defconstant  ch-pred '(lam n (lam f (lam x #$(n (lam g (lam h (h (g f)))) (lam u x) (lam u u))))))
(defconstant  ch-minus `(lam m (lam n #$(n ,ch-pred m))))
(defconstant  ch-if '(lam p (lam a (lam b #$(p a b)))))
(defconstant  ch-false '(lam a (lam b b)))
(defconstant  ch-true '(lam a (lam b a)))
(defconstant  ch-is0 `(lam n #$(n (lam x ,ch-false) ,ch-true)))
(defconstant  ch-and '(lam p (lam q #$(p q p))))
(defconstant  ch-leq `(lam m (lam n #$(,ch-is0 (,ch-minus m n)))))
(defconstant  ch-eq `(lam m (lam n #$(,ch-and (,ch-leq m n) (,ch-leq n m)))))
(defconstant  ch-h `(lam f (lam n #$(,ch-if (,ch-eq n ,ch-0) ,ch-1 (,ch-mult n f (,ch-minus n ,ch-1))))))
