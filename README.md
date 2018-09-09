# combinators-in-lisp
Combinators as Lisp macros on top of lambda calculus.

Lambda calculus is not native language of Common Lisp. (it was for Lisp 1s):

<code>(((lambda (x) (lambda (y) (+ x y))) 1) 2)</code> is not well-formed in CL.

This repo builds on lambda calculus as an ADT (by Alessandro Cimatti) to treat things like above as lambda terms.
That's step #1 for combinators.

More accurately, they will be <b>Curried/Schonfinkeled</b> lambda terms (hence no parenthesis for arguments), and with keyword LAM rather than LAMBDA:

<code>(((lam x (lam y ((+ x) y))) 1) 2)</code>

Once we have that, combinators are just CL macros sitting on top of LAMs (not lambdas)

<b>For combinator names, just prefix the Curry & Feys name with '#&'.</b>

<b>Applicative lambda terms must be binary in the ADT. If you're tired of writing <code>(a b c)</code> as <code>((a b) c)</code>, you can use the reader macro #$(..) which binarizes them recursively for you. </b> Instead of 

<code>(noe '((#&s (lam x (lam y ((p x) y)))) (lam y (q y)))) ==>
(LAM X ((P X) (Q X)))</code>

You can write 

<code>(noe '((#&s (lam x (lam y #$(p x y)))) (lam y (q y)))) ==>
(LAM X ((P X) (Q X)))</code>

<code>noe</code> is the normal order evaluator. 

It is particularly useful when you combine functions with many arguments:

<code>(noe '((#&s (lam x (lam y (lam z #$(p x y z))))) (lam y (q y)))) ==>
(LAM X (LAM Z (((P X) (Q X)) Z)))</code>

There are examples of Church and Scott encodings in the repo to show more complex cases.

For example, Scott encoding of <code>or, true, false</code> are:

<code>or = ((S S) (K (K K)))</code>
  
<code>true = K</code>
  
<code>false = K(S K K)</code>
 
so that <code>(or true false) ==> (lam x (lam y x))</code>, which is <code>K</code>=true.

and <code>(or false false) ==> (lam y (lam x x))</code>, which is <code>K(S K K)</code>=false.

-cem bozsahin
