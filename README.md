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

Applicative lambda terms must be binary in the ADT. If you're tired of writing <code>(a b c)</code> as <code>((a b) c)</code>,
you can use the reader macro #$(..) which binarizes them recursively for you. Instead of 

<code>(noe (&s '(lam x (lam y ((p x) y))) '(lam y (q y)))) ==>
(LAM X ((P X) (Q X)))
</code>

You can write 

<code>(noe (&s '(lam x (lam y #$(p x y))) '(lam y (q y)))) ==>
(LAM X (((&I P) X) (Q X)))
</code>

It is particularly useful when you combine functions with many arguments:

<code>(noe (&s '(lam x (lam y (lam z #$(p x y z)))) '(lam y (q y)))) ==>
(LAM X (LAM Z ((((&I P) X) (Q X)) Z)))
</code>

Notice the extra <code>&i</code> (identity) combinator in the innermost term. 

Keep in mind that <b>all combinators are Curried</b> in the current implementation.

soon.

-cem bozsahin
