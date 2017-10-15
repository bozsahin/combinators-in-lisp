# combinators-in-lisp
Combinators as Lisp macros on top of lambda calculus.

Lambda calculus is not native language of Common Lisp. (it was for Lisp 1s):

<code>(((lambda (x) (lambda (y) (+ x y))) 1) 2)</code> is not well-formed in CL.

This repo builds on lambda calculus as an ADT (by Alessandro Cimatti) to treat things like above as lambda terms.
That's step #1 for combinators.

More accurately, they will be <b>Curried/Schonfinkeled</b> lambda terms (hence no parenthesis for arguments), and with keyword LAM rather than LAMBDA:

<code>(((lam x (lam y ((+ x) y))) 1) 2)</code>

Once we have that, combinators are just CL macros sitting on top of LAMs (not lambdas)

For combinator names, just prefix the Curry & Feys name with '&'.

Keep in mind that <b>all combinators are Curried</b> in the current implementation.

soon.

-cem bozsahin
