;;;; ====================================================
;;;; Combinators on top of true and curried lambda terms.
;;;; -cem bozsahin
;;;; ====================================================

;; some top level interface to lambda ADT

(defmacro noe (term)
  "normal order evaluation"
  `(beta-normalize-outer ,term))


(defmacro aoe (term)
  "applicative order evaluation"
  `(beta-normalize-inner ,term))

(defmacro church-rosser (term)
  "check the church rosser property"
  `(beta-normalize ,term))

(defun uncurry (l &optional (res '&i))
  "makes l left-branching  and binary"
  (if (listp l) 
    (case (length l)
      (0 nil)
      (1 (append (list res) (list (uncurry (first l)))))
      (otherwise (uncurry (rest l)
			  (append (list res) (list (uncurry (first l)))))))
    l))

;; macros for combinators

(defmacro &i (x)
  "identity"
  `(mk-v ,x))

(defmacro  &a (f a)
  "application, which is not a combinator, contrary to common belief, but the primitive of combinators."
  `(mk-a ,f ,a))

(defmacro &b (f g)
  "B combinator"
  `(mk-l (mk-v 'x)(mk-a ,f (mk-a ,g 'x))))

(defmacro &b2 (f g)
  "B^2 combinator"
  `(mk-l (mk-v 'x)(mk-l (mk-v 'y)(mk-a ,f (mk-a (mk-a ,g 'x)'y)))))

(defmacro &b3 (f g)
  "B^3 combinator"
  `(mk-l (mk-v 'x)(mk-l (mk-v 'y)(mk-l (mk-v 'z)(mk-a ,f (mk-a (mk-a (mk-a ,g 'x)'y)'z))))))

(defmacro &s (f g)
  "S combinator"
  `(mk-l (mk-v 'x)(mk-a (mk-a ,f 'x) (mk-a ,g 'x))))

(defmacro &s2 (f g)
  "S^2 combinator. This is actually S'' not Curry's S^2. See Bozsahin 2012"
  `(mk-l (mk-v 'x)(mk-l (mk-v 'y)(mk-a (mk-a ,f 'x) (mk-a (mk-a ,g 'x)'y)))))

(defmacro &o (f g)
  "O combinator, also called D by Hoyt & Baldridge 2008. See Bozsahin 2015 book for discussion."
  `(mk-l (mk-v 'h)(mk-a ,f (mk-l (mk-v 'x)(mk-a ,g (mk-a 'h 'x))))))

(defmacro &k (x y)
  "K combinator; cf &i"
  (declare (ignore y))
  `(mk-v ,x))

(defmacro &c (f g)
  "C combinator"
  `(mk-l (mk-v 'x)(mk-a (mk-a ,f 'x) ,g)))

(defmacro &w (f x)
  "W combinator"
  `(mk-a (mk-a ,f ,x) ,x))

(defmacro &phi (f g)
  "Phi combinator"
  `(mk-l (mk-v 'h)(mk-l (mk-v 'x)(mk-a (mk-a ,f (mk-a ,g 'x)) (mk-a 'h 'x)))))

(defmacro &psi (f g)
  "Psi combinator"
  `(mk-l (mk-v 'z)(mk-l (mk-v 'w)(mk-a (mk-a ,f (mk-a ,g 'z))(mk-a ,g 'w)))))

(defmacro &t (f a)
  "T combinator"
  `(mk-a ,a ,f))

(defmacro &j (x y)
  "Rosser's J combinator"
  `(mk-l (mk-v 'z)(mk-l (mk-v 'w)(mk-a (mk-a ,x ,y)(mk-a (mk-a ,x 'w) 'z)))))

;;;; ==============================================
;;;; The lambda layer
;;;; ==============================================
;;;;
;;;; this is a direct import of Alessandro Cimatti's ADT for Lambda-calculus. 
;;;; Thanks for putting it on the web.
;;;; (minor addition for our purposes: singleton e can be symbol OR constant)

;;;; The ADT for expressions
;;;; e ::= v | l | a
;;;; v ::= symbolp | constantp
;;;; a ::= ( e e )
;;;; l :: = ( lam v e )

(defun mk-v (sym) sym)
(defun is-v (e) (cond ((consp e) nil)
		      ((symbolp e) t) 
		      ((constantp e) t)
		      ((special-operator-p e) t)
		      (t nil)))
(defun mk-l (v b) (list 'lam v b))
(defun is-l (e) (and (consp e) (= (length e) 3) (equal 'lam (first e)) (is-v (second e))))
(defun l-get-v (l) (second l))
(defun l-get-b (l) (third l))
(defun mk-a (f a) (list f a))
(defun is-a (e) (and (consp e) (= (length e) 2)))
(defun a-get-f (a) (first a))
(defun a-get-a (a) (second a))
(defun freshvar ()(gensym))

;; Recognizer. takes arbitrary s-exp in input

(defun is-e (e)
  (cond ((is-v e) t)
	((is-a e) (and
		    (is-e (a-get-f e))
		    (is-e (a-get-a e))))
	((is-l e) (and
		    (is-e (l-get-v e))
		    (is-e (l-get-b e))))
	(t nil)))

;; Return the free variables of an expression

(defun fv (e)
  (cond
    ((is-v e) (list e))
    ((is-a e) (append
		(fv (a-get-f e))
		(fv (a-get-a e))))
    ((is-l e) (remove
		(l-get-v e)
		(fv (l-get-b e))))
    (t (error "Unknown lambda term type"))))

(defun free-in (v e) (member v (fv e)))

;;; equivalence up to alpha conversion

(defun alpha-equivalent1 (e1 e2 rpl1 rpl2)
  (cond 
    ((is-v e1)
     (and (is-v e2)
	  (let ((new1 (cdr (assoc e1 rpl1)))
		(new2 (cdr (assoc e2 rpl2))))
	    (if (and (null new1) (null new2))
	      (equal e1 e2)
	      (equal new1 new2)))))
    ((is-a e1)
     (and (is-a e2)
	  (alpha-equivalent1 (a-get-f e1) (a-get-f e2) rpl1 rpl2) 
	  (alpha-equivalent1 (a-get-a e1) (a-get-a e2) rpl1 rpl2)))
    ((is-l e1)
     (and (is-l e2)
	  (let* ((new (freshvar))
		 (old1 (l-get-v e1))
		 (old2 (l-get-v e2))
		 (newrpl1 (cons (cons old1 new) rpl1))
		 (newrpl2 (cons (cons old2 new) rpl2)))
	    (alpha-equivalent1 (l-get-b e1) (l-get-b e2) newrpl1 newrpl2))))))

(defun alpha-equivalent (e1 e2)  (alpha-equivalent1 e1 e2 nil nil))

;;; substitution

(defun subst-with-in (x e1 exp)
  (cond 
    ((is-v exp)
     (if (equal x exp) e1 exp))
    ((is-a exp)
     (mk-a
       (subst-with-in x e1 (a-get-f exp))
       (subst-with-in x e1 (a-get-a exp))))
    ((is-l exp) ; say exp is (lam y e)
     (let ((y (l-get-v exp)) (e (l-get-b exp)))
       (cond
	 ((equal x y) exp)
	 ((not (free-in x e)) exp)
	 ((and (free-in x e) (not (free-in y e1)))
	  (mk-l y (subst-with-in x e1 e)))
	 ((and (free-in x e) (free-in y e1))
	  (let ((z (freshvar)))
	    (mk-l z (subst-with-in x e1 (subst-with-in y z e))))))))))

;;; beta reduction

(defun is-rdx (e) (and (is-a e) (is-l (a-get-f e))))
(defun rdx-get-v (rdx) (l-get-v (a-get-f rdx)))
(defun rdx-get-b (rdx) (l-get-b (a-get-f rdx)))
(defun rdx-get-a (rdx) (a-get-a rdx))

;;; Beta reduce: (a (l v e) e1) ==> [e1 / v] e

(defun beta-reduce (rdx)
  (subst-with-in 
    (rdx-get-v rdx)
    (rdx-get-a rdx)
    (rdx-get-b rdx)))

;;; Beta reduce if possible

(defun beta-reduce-if-redex (e)
  (if (is-rdx e) (beta-reduce e) e))

;;; Iterate beta reduction on outermost redex

(defun beta-reduce-outer (e &optional (lim 100))
  (cond
    ((< lim 0) e)
    ((is-rdx e)
     (beta-reduce-outer (beta-reduce e) (- lim 1)))
    ((is-v e) e)
    ((is-a e)
     (mk-a
       (beta-reduce-outer (a-get-f e))
       (beta-reduce-outer (a-get-a e))))
    ((is-l e)
     (mk-l
       (l-get-v e)
       (beta-reduce-outer (l-get-b e))))))

;;; Iterate beta reduction on innermost redex

(defun beta-reduce-inner (e &optional (lim 100))
  (cond
    ((< lim 0) e)
    ((is-v e) e)
    ((is-a e)
     (beta-reduce-if-redex
       (mk-a (beta-reduce-inner (a-get-f e) lim)
	     (beta-reduce-inner (a-get-a e) lim))))
    ((is-l e)
     (mk-l
       (l-get-v e)
       (beta-reduce-inner (l-get-b e) lim)))))

;;; Beta normalization

(defun beta-normalize-param (e fn &optional (lim 100))
  (let* ((res (apply fn (list e lim)))
	 (use-alpha-equivalent t)
	 (stop (if use-alpha-equivalent
		 (alpha-equivalent res e)
		 (equal res e))))
    (if stop
      res ; fix point reached
      (beta-normalize-param res fn))))

(defun beta-normalize-outer (e &optional (lim 100))
  (beta-normalize-param e 'beta-reduce-outer lim))

(defun beta-normalize-inner (e &optional (lim 100))
  (beta-normalize-param e 'beta-reduce-inner lim))

;;; try with the two different strategies and compare results

(defun beta-normalize (e)
  (let ((res-inner (beta-normalize-inner e 100))
	(res-outer (beta-normalize-outer e 100)))
    (if (alpha-equivalent res-outer res-inner)
      (progn 
	(format t "Results are alpha equivalent~%")
	(format t "Inner: ~A~%" res-inner)
	(format t "Outer: ~A~2%" res-outer))
      (progn 
	(format t "Results are NOT alpha-equivalent!")
	(format t "Inner: ~A~%" res-inner)
	(format t "Outer: ~A~2%" res-outer)))))

;; a shorthand for uncurrying a list recursively; use as #$(a b c) to get
;;  (((identity a) b) c) 
;; #$(a (b c) d) should give ((identity a ((identity b)c)) d)

(defun |#$-reader| (s c1 c2)
  "NB. results of readers are program expressions, i.e. they are eval'd"
  (declare (ignore c1 c2))
  (let ((l (read s t nil t)))
    (if (listp l) (uncurry l) l)))
(set-dispatch-macro-character #\# #\$ #'|#$-reader|)

