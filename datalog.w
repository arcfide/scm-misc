#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Datalog for Chez Scheme}
\\bigskip
\\centerline{Aaron W. Hsu {\\tt <arcfide@sacrideo.us>}}
\\medskip
\\centerline{\\today}\\par
\\bigskip\\rendertoc\\par
\\vfill
\\noindent
This software is a port of Jay McCarthy's Datalog library for Racket
with modifcations to make it suit the Chez Scheme programming
environment better.  I am currently unable to find the copyrights for
this library online, so I am assuming that I can use the code.
Additions and modifications made by myself fall under the following
license.
\\medskip\\noindent
Copyright $\\copyright$ 2010 Aaron W. Hsu {\\tt <arcfide@sacrideo.us>}
\\medskip\\noindent
Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.
\\medskip\\noindent
THE SOFTWARE IS PROVIDED ``AS IS'' AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\\par\\break
"

"\\centerline{\\bf Library Organization}\\bigskip

\\noindent
The following libraries are defined by this system.
{\\parindent = 2in
\\item{|(arcfide datalog)|}
This is the primary library for normal use which exports all the important functionality. 
\\item{|(arcfide datalog ast)|}
The abstract syntax tree structures and supporting code.
\\item{|(arcfide datalog sexp)|}
The s-expression based notation for writing Datalog.
\\item{|(arcfide datalog pretty-printing)|}
The pretty printing code for datalog output.
\\item{|(arcfide datalog runtime)|}
The runtime system for datalog.
\\item{|(arcfide datalog eval)|}
The evaluation code for datalog.
\\vfill\\par}"

(@l "This library provides the structures that represent Datalog syntax."

(arcfide datalog ast)
(export 
	srcloc? datum? datum-equal? 
	variable variable? make-variable variable-srcloc variable-sym
	variable-equal?
	constant constant? make-constant constant-srcloc constant-datum
	constant-equal?
	term?
	term-equal?
	literal literal? make-literal literal-srcloc literal-predicate literal-terms
	literal-equal? 
	clause clause? make-clause clause-srcloc clause-head clause-body
	clause-equal?
	assertion assertion? make-assertion assertion-srcloc assertion-clause
	retraction retraction? make-retraction retraction-srcloc retraction-clause
	query query? make-query query-srcloc query-literal
	statement?
	program?
)

(import (chezscheme))

(@* "Structures/Records"
"The following form the abstract syntax tree elements."
 
(@c
(define-record-type variable (fields srcloc sym))  
(define-record-type constant (fields srcloc datum))
(define-record-type literal (fields srcloc predicate terms))
(define-record-type clause (fields srcloc head body))
(define-record-type assertion (fields srcloc clause))
(define-record-type retraction (fields srcloc clause))
(define-record-type query (fields srcloc literal))
))

(@* "Predicates"
"The following procedures define a series of the predicates for use in
this library and elsewhere, they identify the various datatypes used
by the Datalog abstract syntax tree."

(@c
(define (srcloc? x)
	(define (test pred? x)
		(or (pred? x) (not x)))
	(define (exact-positive-integer? x)
		(and (integer? x) (exact? x) (positive? x)))
	(define (exact-nonnegative-integer? x)
		(and (integer? x) (exact? x) (nonnegative? x)))
	(define (syntax? x) #t)
	(or 
		(syntax? x)
		(not x)
		(and (list? x) (= 5 (length x))
			(test exact-positive-integer? (list-ref x 1))
			(test exact-nonnegative-integer? (list-ref x 2))
			(test exact-nonnegative-integer? (list-ref x 3))
			(test exact-positive-integer? (list-ref x 4)))))
(define (datum? x)
	(or (string? x) (symbol? x)))
(define (term? x)
	(or (variable? x) (constant? x)))
(define (statement? x) 
	(or (assertion? x) (retraction? x) (query? x)))
(define (program? x) 
	(for-all statement? x))
))

(@* "Equivalence Predicates"
"The following predicates determine equivalence between various AST elements."

(@c
(define datum-equal? equal?)
(define (variable-equal? v1 v2)
	(eq? (variable-sym v1) (variable-sym v2)))
(define (constant-equal? v1 v2)
	(datum-equal? (constant-datum v1) (constant-datum v2)))
(define (term-equal? t1 t2)
	(cond
		[(and (variable? t1) (variable? t2))
			(variable-equal? t1 t2)]
		[(and (constant? t1) (constant? t2))
			(constant-equal? t1 t2)]
		[else #f]))
(define (literal-equal? l1 l2)
	(and 
		(datum-equal? 
			(literal-predicate l1)
			(literal-predicate l2))
		(= 
			(length (literal-terms l1))
			(length (literal-terms l2)))
		(for-all term-equal?
			(literal-terms l1)
			(literal-terms l2))))
(define (clause-equal? c1 c2)
	(and 
		(literal-equal? 
			(clause-head c1)
			(clause-head c2))
		(= 
			(length (clause-body c1))
			(length (clause-body c2)))
		(for-all literal-equal?
			(clause-body c1)
			(clause-body c2))))
))
)

(@l "This package recognizes an alternative, Scheme-like front-end
syntax for Datalog."

(arcfide datalog sexp)
(export
	stx->program
	stx->statement 
	stx->clause
	stx->literal
	stx->term
	sexp->program
	sexp->statement
	sexp->clause
	sexp->literal
	sexp->term
	begin ! ~ ? :- unquote)
(import 
	(chezscheme) 
	(arcfide datalog ast))

(@* "Wrapping S-expressions" ""

(@c
(define (sexpr? x) #t)

(define (sexp-wrap s->)
	(lambda (sexp)
		(s-> (datum->syntax #'dummy sexp))))
))

(@* "Syntax Conversions" ""

(@c
(define (stx->program stx)
	(syntax-case stx (begin)
		[(begin s ...)
			(map stx->statement#'(s ...))]))
(define sexp->program (sexp-wrap stx->program))
))

(@ ""

(@c
(define (stx->statement stx)
	(syntax-case stx (! ~ ?)
		[(! c)
			(make-assertion stx (stx->clause #'c))]
		[(~ c)
			(make-retraction stx (stx->clause #'c))]
		[(? l)
			(make-query stx (stx->literal #'l))]))
(define sexp->statement (sexp-wrap stx->statement))

(define-syntax (! x)
	(errorf #f "misplaced aux keyword ~a"
		(syntax->datum x)))
(define-syntax (~ x)
	(errorf #f "misplaced aux keyword ~a"
		(syntax->datum x)))
(define-syntax (? x)
	(errorf #f "misplaced aux keyword ~a"
		(syntax->datum x)))
))

(@ ""

(@c
(define (stx->clause stx)
	(syntax-case stx (:-)
		[(:- l body ...)
			(make-clause stx (stx->literal #'l)
				(map stx->literal (syntax->list #'(body ...))))]
		[l
			(make-clause stx (stx->literal #'l) '())]))
(define sexp->clause (sexp-wrap stx->clause))

(define-syntax (:- x)
	(errorf #f "misplaced aux keyword ~a"
		(syntax->datum x)))
))

(@ ""

(@c
(define (stx->literal stx)
	(syntax-case stx ()
		[(p t ...)
			(make-literal
				stx (stx->datum #'p)
				(map stx->term (syntax->list #'(t ...))))]))
(define sexp->literal (sexp-wrap stx->literal))
))

(@ ""

(@c
(define (stx->term stx)
	(syntax-case stx (unquote)
		[(unquote d)
			(identifier? #'d)
			(make-variable stx (syntax->datum #'d))]
		[d
			(datum-syntax? #'d)
			(make-constant stx (stx->datum #'d))]))
(define sexp->term (sexp-wrap stx->term))
))

(@ ""

(@c
(define (datum-syntax? stx)
	(define d (syntax->datum stx))
	(or (symbol? d) (string? d)))
(define (stx->datum stx)
	(syntax-case stx ()
		[d
			(datum-syntax? #'d)
			(syntax->datum #'d)]))
))
)

(@l "This library provides facilities for pretty-printing Datalog source."

(arcfide datalog pretty-printing)
(export
	format-datum
	format-variable
	format-constant
	format-term
	format-literal
	format-literals
	format-clause
	format-assertion
	format-retraction
	format-query
	format-statement
	format-program)
(import
	(chezscheme)
	(rename (only (chezscheme) lambda) (lambda λ))
	(arcfide datalog ast))

(@ ""

(@c
(define (format-datum s)
	(cond
		[(string? s)
			(format "~S" s)]
		[(symbol? s)
			(symbol->string s)]))
))

(@ ""

(@c
(define (format-variable v)
	(format-datum (variable-sym v)))
(define (format-constant c)
	(format-datum (constant-datum c)))
))

(@ ""

(@c
(define (format-term t)
	(cond
		[(variable? t)
			(format-variable t)]
		[(constant? t)
			(format-constant t)]))
))

(@ ""

(@c
(define (format-literal l)
	(assert (literal? l))
	(let ([pred (literal-predicate l)] [terms (literal-terms l)])
		(cond
			[(null? terms) (format-datum pred)]
			[(and (eq? '= pred) (list? terms) (= 2 (length terms)))
				(format "~a = ~a" 
					(format-term (car terms))
					(format-term (cadr terms)))]
			[else
				(format "~a(~{~a~^, ~})"
					(format-datum pred)
					(map format-term terms))])))
))

(@ ""

(@c
(define (format-literals ls)
	(format "~{~a\n~}\n"
		(map
			(λ (l)
				(format-assertion
					(make-assertion #f (make-clause #f l '()))))
			ls)))
))

(@ ""

(@c
(define (format-clause c)
	(if (null? (clause-body c))
		(format "~a" (format-literal (clause-head c)))
		(format "~a :- ~{\n~4,0t~a~^, ~}"
			(format-literal (clause-head c))
			(map format-literal (clause-body c)))))
))

(@ ""

(@c
(define (format-assertion a)
	(format "~a." (format-clause (assertion-clause a))))
(define (format-retraction r)
	(format "~a~~" (format-clause (retraction-clause r))))
(define (format-query q)
	(format "~a?" (format-literal (query-literal q))))
))

(@ ""

(@c
(define (format-statement s)
	(cond
		[(assertion? s) (format-assertion s)]
		[(retraction? s) (format-retraction s)]
		[(query? s) (format-query s)]))
(define (format-program p)
	(format "~{~a~^\n~}" (map format-statement p)))
))
)

(@l ""

(arcfide datalog runtime)
(export
	safe-clause? 
	theory? immutable-theory? mutable-theory?
	make-mutable-theory make-immutable-theory
	assume retract assume! retract! prove)
(import
	(chezscheme)
	(rename (only (chezscheme) lambda) (lambda λ))
	(riastradh hash-tries)
	(arcfide datalog ast))

(@* "Environment module"
""

(@c
(define hash-trie-type:equal (make-hash-trie-type equal? equal-hash))
(define (env? x)
	(hash-trie? x))
(define (empty-env)
	(make-hash-trie hash-trie-type:equal))
(define (lookup env var . maybe-def)
	(hash-trie/lookup env var 
		(if (null? maybe-def) #f (car maybe-def))))
(define (extend env var val)
	(hash-trie/insert env var val))
))

(@* "Substitution"
""

(@> |Substitution procedures| 
(export subst-term subst-clause rename-clause rename-literal)
(define (subst-term env t)
	(if (variable? t)
		(lookup env (variable-sym t) t)
		t))

(define (subst-literal env lit)
	(make-literal 
		(literal-srcloc lit)
		(literal-predicate lit)
		(map (λ (t) (subst-term env t))
			(literal-terms lit))))

(define (subst-clause env c)
	(make-clause 
		(clause-srcloc c)
		(subst-literal env (clause-head c))
		(map (λ (l) (subst-literal env l))
			(clause-body c))))

(define (shuffle env lit)
	(unless (literal? lit)
		(errorf 'shuffle "~s is not a literal" lit))
	(let loop ([env env] [terms (literal-terms lit)])
		(cond
			[(null? terms) env]
			[(constant? (car terms))
				(loop env (cdr terms))]
			[(variable? (car terms))
				(let ([var (variable-sym (car terms))])
					(if (lookup env var)
						(loop env (cdr terms))
						(loop 
							(extend env var 
								(make-variable 
									(variable-srcloc (car terms)) 
									(gensym 
										(if (string? var) 
											var 
											(symbol->string var)))))
							(cdr terms))))]
			[else (errorf 'shuffle "unknown term ~s" (car terms))])))

(define (rename-clause c)
	(subst-clause
		(fold-left
			(λ (e a) (shuffle e a))
			(shuffle (empty-env) (clause-head c))
			(clause-body c))
		c))

(define (rename-literal lit)
	(subst-literal (shuffle (empty-env) lit) lit))
))

(@ ""

(@c
(@< |Substitution procedures|)
))

(@* "Unification module"
""

(@> |Unification procedures|
(export unify unify-term)
(define (chase env t)
	(if (variable? t)
		(cond
			[(lookup env (variable-sym t)) =>
				(λ (term) (chase env term))]
			[else t])
		t))

(define (unify-term env t1 t2)
	(define t1-p (chase env t1))
	(define t2-p (chase env t2))
	(cond
		[(term-equal? t1-p t2-p) env]
		[(variable? t1-p) 
			(extend env (variable-sym t1-p) t2-p)]
		[(variable? t2-p)
			(extend env (variable-sym t2-p) t1-p)]
		[else #f]))

(define (unify-terms env ts1 ts2)
	(cond
		[(null? ts1) (if (null? ts2) env #f)]
		[(null? ts2) #f]
		[(unify-term env (car ts1) (car ts2)) =>
			(λ (env) (unify-terms env (cdr ts1) (cdr ts2)))]
		[else #f]))

(define (unify l1 l2)
	(and 
		(datum-equal? 
			(literal-predicate l1)
			(literal-predicate l2))
		(unify-terms 
			(empty-env)
			(literal-terms l1)
			(literal-terms l2))))
))

(@ ""

(@c
(@< |Unification procedures|)
))

(@* "Variant term module"
""

(@> |Variant term procedures|
(export literal-tbl? make-literal-tbl literal-tbl-find literal-tbl-replace! mem-literal)
(define (variant-terms env1 env2 ts1 ts2)
	(cond
		[(null? ts1) (null? ts2)]
		[(not (null? ts2))
			(variant-term
				env1 env2
				(car ts1) (car ts2)
				(cdr ts1) (cdr ts2))]
		[else #f]))

(define (variant-term env1 env2 t1 t2 ts1 ts2)
	(or
		(and
			(variable? t1)
			(variable? t2)
			(variant-var
				env1 env2
				(variable-sym t1) (variable-sym t2)
				ts1 ts2))
		(and
			(term-equal? t1 t2)
			(variant-terms env1 env2 ts1 ts2))))

(define (variant-var env1 env2 v1 v2 ts1 ts2)
	(let ([r1 (lookup env1 v1)] [r2 (lookup env2 v2)])
		(cond
			[(not (or r1 r2))
				(variant-terms
					(extend env1 v1 (make-variable #f v2))
					(extend env2 v2 (make-variable #f v1))
					ts1 ts2)]
			[(and (variable? r1) (list? r2) (variable? (car r2))
				(datum-equal? (variable-sym r1) v2)
				(datum-equal? (variable-sym (car r2)) v1)
				(variant-terms env1 env2 ts1 ts2))]
			[else #f])))

(define (variant? l1 l2)
	(and 
		(datum-equal? 
			(literal-predicate l1)
			(literal-predicate l2))
		(variant-terms 
			(empty-env) (empty-env)
			(literal-terms l1)
			(literal-terms l2))))

(define (mem-literal lit ls)
	(exists (λ (l) (variant? lit l)) ls))

; Literal Tables modulo variant?
(define (term-hash t recur-hash)
	(cond
		[(variable? t) 101]
		[(constant? t) (recur-hash (constant-datum t))]))

(define (mk-literal-hash recur-hash)
	(λ (l)
		(let loop (
				[code (recur-hash (literal-predicate l))]
				[i 0]
				[terms (literal-terms l)])
			(if (null? terms)
				code
				(loop 
					(+ code (term-hash (car terms) recur-hash) (* i -7))
					(add1 i)
					(cdr terms))))))

(define (literal-tbl? x)
	(hashtable? x))
(define (make-literal-tbl)
	(make-hashtable (mk-literal-hash equal-hash) variant?))
(define (literal-tbl-find ltbl s)
	(hashtable-ref ltbl s #f))
(define (literal-tbl-replace! ltbl s x)
	(hashtable-set! ltbl s x))

))

(@ ""

(@c
(@< |Variant term procedures|)
))

(@* "Clause safety"
""

(@c
; A clause is safe if every variable in its head occurs in some literal in its body.
(define (safe-clause? c)
	(for-all 
		(λ (v)
			(exists 
				(λ (l)
					(exists 
						(λ (t) (term-equal? t v))
						(literal-terms l)))
				(clause-body c)))
		(filter variable? (literal-terms (clause-head c)))))
))

(@* "Predicates"
""

(@c
(define (theory? x)
	(or (hash-trie? x) (hashtable? x)))
(define (immutable-theory? x)
	(hash-trie? x))
(define (mutable-theory? x)
	(hashtable? x))
))

(@* "Keys"
""

(@c
(define (literal-key l)
	(format "~a/~a" (literal-predicate l) (length (literal-terms l))))
(define (clause-key c)
	(literal-key (clause-head c)))
))

(@* "Theory Constructors"
""

(@c
(define (make-immutable-theory)
	(make-hash-trie hash-trie-type:equal))
(define (make-mutable-theory)
	(make-hashtable equal-hash equal?))
))

(@* "Theorem Functions"
""

(@c
(define (mk-assume hash-update) 
	(λ (thy c)
		(hash-update thy (clause-key c) '()
	 		(λ (clauses) (cons c clauses)))))
(define (mk-retract hash-update) 
	(λ (thy rc)	
		(hash-update thy (clause-key rc) '()
			(λ (clauses)
				(filter 
					(λ (c) (not (clause-equal? c rc)))
					clauses)))))

(define assume (mk-assume hash-trie/modify))
(define retract (mk-retract hash-trie/modify))
(define assume! 
	(mk-assume 
		(λ (t k d p) (hashtable-update! t k p d))))
(define retract! 
	(mk-retract 
		(λ (t k d p) (hashtable-update! t k p d))))

(define (get thy lit)
	(cond
		[(immutable-theory? thy)
			(hash-trie/lookup thy (literal-key lit) '())]
		[(mutable-theory? thy)
			(hashtable-ref thy (literal-key lit) '())]
		[else (errorf 'get "type of theory ~s unknown" thy)]))
))

(@* "Subgoals"
""

(@c
(define-record-type subgoal
	(fields
		literal
		(mutable facts)
		(mutable waiters)))
))

(@* "Proving Theories"
""

(@c
(define (resolve c lit)
	(define body (clause-body c))
	(and (not (null? body))
		(cond
			[(unify (car body) (rename-literal lit)) => 
				(λ (env)
					(subst-clause env 
						(make-clause 
							(clause-srcloc c) 
							(clause-head c) 
							(cdr body))))]
			[else #f])))

(define (prove thy lit)
	(define subgoals (make-literal-tbl))
	(define (fact! sg lit)
		(unless (mem-literal lit (subgoal-facts sg))
			(subgoal-facts-set! sg (list* lit (subgoal-facts sg)))
			(for-each 
				(λ (w)
					(cond
						[(resolve (cdr w) lit)
							=> (λ (cs-p) (add-clause! (car w) cs-p))]))
				(subgoal-waiters sg))))
	(define (rule! sg1 c s)
		(define sg2 (literal-tbl-find subgoals s))
		(if sg2
			(begin
				(subgoal-waiters-set! sg2 (list* (cons sg1 c) (subgoal-waiters sg2)))
				(for-each 
					(λ (fact)
						(cond
							[(resolve c fact)
								=> (λ (cs) (add-clause! sg1 cs))]))
					(subgoal-facts sg2)))
			(let ([sg2 (make-subgoal s '() (list (cons sg1 c)))])
				(literal-tbl-replace! subgoals s sg2)
				(search! sg2))))
	(define (add-clause! sg c)
		(unless (clause? c)
			(errorf 'prove "unknown clause ~s" c))
		(cond
			[(null? (clause-body c)) (fact! sg (clause-head c))]
			[(pair? (clause-body c)) (rule! sg c (car (clause-body c)))]
			[else (errorf 'prove "unknown clause ~s" c)]))
	(define (search-theory! sg)
		(for-each 
			(λ (clause)
				(define renamed (rename-clause clause))
				(define selected (clause-head renamed))
				(cond
					[(unify (subgoal-literal sg) selected) => 
						(λ (env) (add-clause! sg (subst-clause env renamed)))]))
			(get thy (subgoal-literal sg))))
	(define (search! sg)
		(let ([sglit (subgoal-literal sg)])
			(cond
				[(and 
					(literal? sglit) 
					(eq? '= (literal-predicate sglit)) 
					(= 2 (length (literal-terms sglit))))
					(let ([srcloc (literal-srcloc sglit)] [terms (literal-terms sglit)])
						(define (equal-test a b)
							(when (term-equal? a b)
					 			(fact! sg (make-literal srcloc '= (list a b)))))
						(cond
							[(unify-term (empty-env) (car terms) (cadr terms)) =>
								(λ (env)
									(equal-test 
										(subst-term env (car terms))
										(subst-term env (cadr terms))))]
							[else (equal-test (car terms) (cadr terms))]))]
				[else (search-theory! sg)])))
	(define sg (make-subgoal lit '() '()))
	(literal-tbl-replace! subgoals lit sg)
	(search! sg)
	(subgoal-facts sg))
))
)

(@l ""

(arcfide datalog eval)
(export
	current-theory
	eval-program
	eval-statement
	eval-program/fresh
	datalog-repl)
(import
	(chezscheme)
	(rename (only (chezscheme) lambda) (lambda λ))
	(arcfide datalog ast)
	(arcfide datalog sexp)
	(arcfide datalog pretty-printing)
	(arcfide datalog runtime))

(@* "Theory Parameter"
""

(@c
(define current-theory (make-parameter (make-mutable-theory)))
))

(@* "Safe Assumptions"
""

(@c
(define (assume-if-safe assume thy s)
	(let ([c (assertion-clause s)])
		(if (safe-clause? c)
			(assume thy c)
			(syntax-violation 'datalog
				"unsafe clause in assertion"
				(format-statement s)))))
))

(@* "Printing"
""

(@c
(define (print-literals ls)
	(printf "~a\n" (format-literals ls)))
))

(@* "Evaluation"
""

(@c
(define (eval-program p)
	(for-each 
		(λ (s)
			(define v (eval-statement s))
			(unless (eq? (void) v)
				(print-literals v)))
		p))
(define (eval-statement s)
	(cond
		[(assertion? s)
			(assume-if-safe assume! (current-theory) s)]
		[(retraction? s)
			(retract! (current-theory) (retraction-clause s))]
		[(query? s)
			(prove (current-theory) (query-literal s))]))

(define (eval-program/fresh p)
	(let loop (
			[thy (make-immutable-theory)]
			[p p])
		(if (null? p)
			thy
			(let ([s (car p)])
				(loop
					(cond
						[(assertion? s)
							(assume-if-safe assume thy s)]
						 [(retraction? s)
							(retract thy (retraction-clause s))]
						 [(query? s)
							(print-literals (prove thy (query-literal s)))
							thy])
					 (cdr p))))))
))

(@* "Datalog REPLs"
"The following convenience form makes it easy to spawn a new interactive Datalog REPL for playing around with a specific datalog theorem.

\\medskip\\verbatim
(datalog-repl)
(datalog-repl theory)
|endverbatim
\\medskip

\\noindent
The nullary version of this procedure creates a new REPL using |new-cafe| that has an empty, new, mutable theory. You can run SEXP datalog commands in it and it will print out the results of queries. The unary version uses the given theory as its starting point. We can create a new cafe like so:"

(@> |New datalog cafe| (capture theory)
(new-cafe
	(λ (exp)
		(let ([res (eval-statement (sexp->statement exp))])
			(when (pair? res)
				(display (format-literals res))))))
))

(@ "We can then use |case-lambda| for a nice little datalog repl creating procedure."

(@c
(define datalog-repl
	(case-lambda
		[() (let ([theory (make-mutable-theory)]) (@< |New datalog cafe| theory))]
		[(theory) (@< |New datalog cafe| theory)]))
))
)

(@l "This package contains a lightweight deductive database system.
Queries and database updates are expressed using Datalog --- a
declarative logic language in which each formula is a function-free
Horn clause, and every variable in the head of a clause must appear in
the body of the clause.	The use of Datalog syntax and an
implementation based on tabling intermediate results ensures that all
queries terminate."

(arcfide datalog)
(export
	srcloc? datum? datum-equal? 
	variable variable? make-variable variable-srcloc variable-sym
	variable-equal?
	constant constant? make-constant constant-srcloc constant-datum
	constant-equal?
	term?
	term-equal?
	literal literal? make-literal literal-srcloc literal-predicate literal-terms
	literal-equal? 
	clause clause? make-clause clause-srcloc clause-head clause-body
	clause-equal?
	assertion assertion? make-assertion assertion-srcloc assertion-clause
	retraction retraction? make-retraction retraction-srcloc retraction-clause
	query query? make-query query-srcloc query-literal
	statement?
	program?
	stx->program
	stx->statement 
	stx->clause
	stx->literal
	stx->term
	sexp->program
	sexp->statement
	sexp->clause
	sexp->literal
	sexp->term
	begin ! ~ ? :- unquote
	format-datum
	format-variable
	format-constant
	format-term
	format-literal
	format-literals
	format-clause
	format-assertion
	format-retraction
	format-query
	format-statement
	format-program
	safe-clause? 
	theory? immutable-theory? mutable-theory?
	make-mutable-theory make-immutable-theory
	assume retract assume! retract! prove
	current-theory
	eval-program
	eval-statement
	eval-program/fresh
	datalog-repl
)
(import
	(arcfide datalog ast)
	(arcfide datalog sexp)
	(arcfide datalog pretty-printing)
	(arcfide datalog runtime)
	(arcfide datalog eval))

(@ "This package has no definitions of its own.")

)
