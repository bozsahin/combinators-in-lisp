# combinators-in-lisp
Combinators as Lisp macros on top of lambda calculus.

Lambda calculus is not native language of Common Lisp. (it was for Lisp 1s):

<code>(((lambda (x) (lambda (y) (+ x y))) 1) 2)</code> is not well-formed in CL.

That's because of two namespaces in CL. Now try

<code>(((lam x (lam y ((+ x) y))) 1) 2)</code>

to see the difference.

Once we have that, combinators are just macros sitting on top of LAMs (not lambdas)

Lambda calculus ADT is by Alessandro Cimatti.

For combinators, just prefix the Curry & Feys name with '&'.

-cem bozsahin
