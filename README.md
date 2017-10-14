# combinators-in-lisp
Combinators as Lisp macros on top of lambda calculus.

Lambda calculus is not native language of Common Lisp. (it was for Lisp 1s):

<code>(((lambda (x) (lambda (y) (+ x y))) 1) 2)</code> is not well-formed in CL.

This repo builds on lambda calculus as an ADT (by Alessandro Cimatti) to treat things like above as lambda terms.

More accurately, they will be <b>Curried/Schonfinkeled</b> lambda terms, and with keyword LAM rather than LAMBDA:

<code>(((lambda x (lambda x ((+ x) y))) 1) 2)</code>

Once we have that, combinators are just macros sitting on top of LAMs (not lambdas)

For combinator names, just prefix the Curry & Feys name with '&'.

-cem bozsahin
