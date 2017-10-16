# combinators-in-lisp
Combinators as Lisp macros on top of lambda calculus.

Lambda calculus is not native language of Common Lisp. (it was for Lisp 1s):

<code>(((lambda (x) (lambda (y) (+ x y))) 1) 2)</code> is not well-formed in CL.

This repo builds on lambda calculus as an ADT (by Alessandro Cimatti) to treat things like above as lambda terms.
That's step #1 for combinators.

More accurately, they will be <b>Curried/Schonfinkeled</b> lambda terms (hence no parenthesis for arguments), and with keyword LAM rather than LAMBDA:

<code>(((lam x (lam y ((+ x) y))) 1) 2)</code>

Once we have that, combinators are just CL macros sitting on top of LAMs (not lambdas)

<b>For combinator names, just prefix the Curry & Feys name with '&'.</b>

Applicative lambda terms must be binary in the ADT. If you're tired of writing <code>(a b c)</code> as <code>((a b) c)</code>, you can use the reader macro #$(..) which binarizes them recursively for you. Instead of 

<code>(noe (&s '(lam x (lam y ((p x) y))) '(lam y (q y)))) ==>
(LAM X ((P X) (Q X)))</code>

You can write 

<code>(noe (&s '(lam x (lam y #$(p x y))) '(lam y (q y)))) ==>
(LAM X (((&I P) X) (Q X)))</code>

It is particularly useful when you combine functions with many arguments:

<code>(noe (&s '(lam x (lam y (lam z #$(p x y z)))) '(lam y (q y)))) ==>
(LAM X (LAM Z ((((&I P) X) (Q X)) Z)))</code>

Here is an example with B-cube using bunch of #$(..) in it; as long as their result is not eval'd they are fine:

<code>(aoe (&b3 '(lam x (lam y #$(p x y))) '(lam y (lam z (lam z2 #$(q y z z2)))))) ==>

(LAM X (LAM Y (LAM Z (LAM #:G471 (((&I P) ((((&I Q) X) Y) Z)) #:G471)))))
</code>

Here, <code>noe</code> and <code>aoe</code> are normal order and applicative order evaluators.

Notice the extra <code>&i</code> (identity) combinator in the innermost term. It is introduced by
the <code>uncurry</code> function as a reminder that the innermost object is necessarily a function.

Keep in mind that <b>all combinators are Curried</b> in the current implementation.

-cem bozsahin
