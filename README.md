# combinators-in-lisp
Combinators as Lisp macros on top of lambda calculus.

To use, just do: <code>(load "combinators")</code>

Lambda calculus is not native language of Common Lisp. (it was for Lisp 1s):

<code>(((lambda (x) (lambda (y) (+ x y))) 1) 2)</code> is not well-formed in CL.

This repo builds on lambda calculus as an ADT (by Alessandro Cimatti) to treat things like above as lambda terms.
That's step #1 for combinators.

More accurately, they will be <b>Curried/Schonfinkeled</b> lambda terms (hence no parenthesis for arguments), and with keyword LAM rather than LAMBDA:

<code>(((lam x (lam y ((+ x) y))) 1) 2)</code>

Once we have that, combinators are just CL macros sitting on top of LAMs (not lambdas)

<b>For combinator names, just prefix the Curry & Feys name with comma in a backquoted list (S is ss and T is tt because of
  name clash with Lisp.</b>

<b>Applicative lambda terms must be binary in the ADT. If you're tired of writing <code>(a b c)</code> as <code>((a b) c)</code>, you can use the reader macro #$(..) which binarizes them recursively for you. </b> Instead of 

<code>(noe `((,ss (lam x (lam y ((p x) y)))) (lam y (q y)))) ==>
(LAM X ((P X) (Q X)))</code>

You can write 

<code>(noe `((,ss (lam x (lam y #$(p x y)))) (lam y (q y)))) ==>
(LAM X ((P X) (Q X)))</code>

<code>noe</code> is the normal order evaluator. 

It is particularly useful when you combine functions with many arguments:

<code>(noe `((,ss (lam x (lam y (lam z #$(p x y z))))) (lam y (q y)))) ==>
(LAM X (LAM Z (((P X) (Q X)) Z)))</code>

For example, mortals wouldn't want to binarize the following manually:

<code>(noe `#$((,ss ,ss (,k (,k ,k))) (,k ,k) ,k))</code>

There are such examples of Church encodings in the repo.

For example, Church encoding of <code>or, true, false</code> are:

<code>or = ((S S) (K (K K)))</code>
  
<code>true = K K</code>
  
<code>false = K</code>
 
so that <code>(or true false) ==> (lam y (lam x (lam y1 x)))</code>, which is <code>K K</code>=true.

and <code>(or false false) ==> (lam x (lam y x))</code>, which is <code>K</code>=false.

Have a look at the examples in the repo for this. Last use of <code>noe</code> above just did the equivalent of
<code>(or true false)</code>.

You can check for equivalences too. For example <code>O=C B^2 B</code>, and you can check this by comparing:

<code>(noe `#$(,o 1 2 3))</code>

and 

<code>(noe `#$(,c ,b2 ,b 1 2 3))</code>.

Left-associativity is taken care of on the fly. All of the examples below are same lambda terms:

<code> ((lam x x) a)</code>

<code> (((lam x x)) a)</code>

<code> ((((lam x x))) a)</code>

-cem bozsahin
