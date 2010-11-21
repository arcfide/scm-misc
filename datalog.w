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

\\centerline{\\bf TODO List}\\bigskip

\\orderedlist
\\li Add more primitive predicates to the system.
\\endorderedlist

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
  term-equal? term->? term->=? term-<? term-<=? term-<>?
  literal literal? make-literal literal-srcloc literal-predicate literal-terms
  literal-equal? 
  clause clause? make-clause clause-srcloc clause-head clause-body
  clause-equal?
  assertion assertion? make-assertion assertion-srcloc assertion-clause
  retraction retraction? make-retraction retraction-srcloc retraction-clause
  query query? make-query query-srcloc query-literal
  statement?
  program?
  run-datalog/ast-tests
)
(import 
  (chezscheme)
  (rename (only (chezscheme) lambda) (lambda λ))
  (srfi :64))

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

(@* "Primitive Predicates"
"In addition to the equivalence predicates above, we have a number of
term level predicates that are used as primitives."

(@c
(define (make-term-comparator num-pred? string-pred?)
  (λ (t1 t2)
    (and (constant? t1) (constant? t2)
      (let (
          [d1 (constant-datum t1)]
          [d2 (constant-datum t2)])
        (or
          (and (string? d1) (string? d2)
            (string-pred? d1 d2))
          (and (number? d1) (number? d2)
            (num-pred? d1 d2)))))))
(define term-<? (make-term-comparator < string<?))
(define term-<=? (make-term-comparator <= string<=?))
(define term->? (make-term-comparator > string>?))
(define term->=? (make-term-comparator >= string>=?))
(define term-<>? 
  (make-term-comparator 
    (λ (x y) (not (= x y)))
    (λ (x y) (not (string=? x y)))))
))

(@* "Testing Suite"
"The following groups the various tests together into a single testing suite for export."

(@c
(define (run-datalog/ast-tests)
  (test-begin "AST")
    (test-begin "Datum Equality")
      (test-assert "str/str 1" (datum-equal? "str" "str"))
      (test-assert "str/str 2" (not (datum-equal? "str1" "str2")))
      (test-assert "sym/sym 1" (datum-equal? 'sym1 'sym1))
      (test-assert "sym/sym" (not (datum-equal? 'sym1 'sym2)))
      (test-assert "str/sym" (not (datum-equal? "str" 'sym)))
      (test-assert "sym/str" (not (datum-equal? 'sym "str")))
    (test-end "Datum Equality")
    (test-begin "Variable Equality")
      (test-assert "var/var" 
        (variable-equal? 
          (make-variable #f 'sym1) 
          (make-variable #f 'sym1)))
      (test-assert "var/var" 
        (variable-equal? 
          (make-variable #f 'sym1) 
          (make-variable #'sym1 'sym1)))
      (test-assert "var/var" 
        (not 
          (variable-equal? 
            (make-variable #f 'sym1) 
            (make-variable #f 'sym2))))
      (test-assert "var/var" 
        (not 
          (variable-equal? 
            (make-variable #f 'sym1) 
            (make-variable #'sym2 'sym2))))
    (test-end "Variable Equality")
    (test-begin "Constant Equality")
      (test-assert "sym/sym" 
        (constant-equal? 
          (make-constant #f 'sym1) 
          (make-constant #f 'sym1)))
      (test-assert "sym/sym" 
        (constant-equal? 
          (make-constant #f 'sym1) 
          (make-constant #'sym1 'sym1)))
      (test-assert "sym/sym" 
        (not 
          (constant-equal? 
            (make-constant #f 'sym1) 
            (make-constant #'sym1 'sym2))))
      (test-assert "str/str" 
        (constant-equal? 
          (make-constant #f "sym1") 
          (make-constant #f "sym1")))
      (test-assert "str/str" 
        (constant-equal? 
          (make-constant #f "sym1") 
          (make-constant #'sym1 "sym1")))
      (test-assert "str/str" 
        (not 
          (constant-equal? 
            (make-constant #f "sym1") 
            (make-constant #'sym1 "sym2"))))
      (test-assert "sym/str" 
        (not 
          (constant-equal? 
            (make-constant #f 'sym1) 
            (make-constant #'sym1 "sym2"))))
      (test-assert "str/sym" 
        (not 
          (constant-equal? 
            (make-constant #'sym1 "sym2") 
            (make-constant #f 'sym1))))
    (test-end "Constant Equality")
    (test-begin "Term Equality")
      (test-assert "var/var" 
        (term-equal? 
          (make-variable #f 'sym1) 
          (make-variable #f 'sym1)))
      (test-assert "var/var" 
        (term-equal? 
          (make-variable #f 'sym1) 
          (make-variable #'sym1 'sym1)))
      (test-assert "var/var" 
        (not 
          (term-equal? 
            (make-variable #f 'sym1) 
            (make-variable #f 'sym2))))
      (test-assert "var/var" 
        (not 
          (term-equal? 
            (make-variable #f 'sym1) 
            (make-variable #'sym2 'sym2))))
      (test-assert "sym/sym" 
        (term-equal? 
          (make-constant #f 'sym1) 
          (make-constant #f 'sym1)))
      (test-assert "sym/sym" 
        (term-equal? 
          (make-constant #f 'sym1) 
          (make-constant #'sym1 'sym1)))
      (test-assert "sym/sym" 
        (not 
          (term-equal? 
            (make-constant #f 'sym1) 
            (make-constant #'sym1 'sym2))))
      (test-assert "str/str" 
        (term-equal? 
          (make-constant #f "sym1") 
          (make-constant #f "sym1")))
      (test-assert "str/str" 
        (term-equal? 
          (make-constant #f "sym1") 
          (make-constant #'sym1 "sym1")))
      (test-assert "str/str" 
        (not 
          (term-equal? 
            (make-constant #f "sym1") 
            (make-constant #'sym1 "sym2"))))
      (test-assert "sym/str" 
        (not 
          (term-equal? 
            (make-constant #f 'sym1) 
            (make-constant #'sym1 "sym2"))))
      (test-assert "str/sym" 
        (not 
          (term-equal? 
            (make-constant #'sym1 "sym2") 
            (make-constant #f 'sym1)))) 
      (test-assert "con/var" 
        (not 
          (term-equal? 
            (make-constant #'sym1 "sym2") 
            (make-variable #f 'sym1))))
      (test-assert "var/con" 
        (not 
          (term-equal? 
            (make-variable #f 'sym1) 
            (make-constant #'sym1 "sym2"))))
    (test-end "Term Equality")
    (test-begin "Literal Equality")
      (test-assert "lit" 
        (literal-equal? 
          (make-literal #f 'lit1 '()) 
          (make-literal #'lit1 'lit1 '())))
      (test-assert "lit" 
        (literal-equal? 
          (make-literal #f 'lit1 (list (make-variable #f 'sym1)))
          (make-literal #'lit1 'lit1 (list (make-variable #f 'sym1)))))
      (test-assert "lit" 
        (literal-equal? 
          (make-literal #f 'lit1 (list (make-variable #f 'sym1)))
          (make-literal #'lit1 'lit1 (list (make-variable #'sym1 'sym1)))))
      (test-assert "lit" 
        (not 
          (literal-equal? 
            (make-literal #f 'lit1 '()) (make-literal #'lit1 'lit2 '()))))
      (test-assert "lit" 
        (not 
          (literal-equal? 
            (make-literal #f 'lit1 (list (make-variable #f 'sym1))) 
            (make-literal #'lit1 'lit2 '()))))
      (test-assert "lit" 
        (not 
          (literal-equal? 
            (make-literal #f 'lit1 (list (make-variable #f 'sym1)))
            (make-literal #'lit1 'lit2 (list (make-variable #'sym1 'sym2))))))
    (test-end "Literal Equality")
    (test-begin "Clause Equality")
      (test-assert "lit" 
        (clause-equal? 
          (make-clause #f (make-literal #f 'lit1 '()) '())
          (make-clause #f (make-literal #f 'lit1 '()) '())))
      (test-assert "lit" 
        (clause-equal?
          (make-clause #f (make-literal #f 'lit1 '()) (list (make-literal #f 'lit1 '())))
          (make-clause #f (make-literal #f 'lit1 '()) (list (make-literal #f 'lit1 '())))))
      (test-assert "lit" 
        (clause-equal? 
          (make-clause #f (make-literal #f 'lit1 '()) '())
          (make-clause #'cl1 (make-literal #f 'lit1 '()) '())))
      (test-assert "lit" 
        (not 
          (clause-equal? 
            (make-clause #f (make-literal #f 'lit1 '()) '())
            (make-clause #f (make-literal #f 'lit2 '()) '()))))
      (test-assert "lit" 
        (not 
          (clause-equal? 
            (make-clause #f (make-literal #f 'lit1 '()) (list (make-literal #f 'lit1 '())))
            (make-clause #f (make-literal #f 'lit1 '()) '()))))
      (test-assert "lit" 
        (not 
          (clause-equal? 
            (make-clause #f (make-literal #f 'lit1 '()) (list (make-literal #f 'lit1 '())))
            (make-clause #f (make-literal #f 'lit1 '()) (list (make-literal #f 'lit2 '()))))))
    (test-end "Clause Equality")
  (test-end "AST"))
))
)

(@l "This package recognizes an alternative, Scheme-like front-end
syntax for Datalog.

\\medskip\\verbatim
program := (begin <statement> ...)       
statement := <assertion> || <retraction> || <query>
assertion  :=  (! <clause>)
retraction  :=  (~ <clause>)
query  :=  (? <literal>)
clause  :=  (:- <literal> <literal> ...) ||  <literal>
literal  :=  (<datum> <term> ...)
term  :=  <variable> ||  <constant>
variable  :=  (unquote :symbol)
constant  :=  <datum>
datum  :=  :symbol ||  :string
|endverbatim
\\medskip"

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
  begin ! ~ ? :- unquote
  run-datalog/sexp-tests)
(import 
  (chezscheme) 
  (arcfide datalog ast)
  (srfi :64))

(@* "Wrapping S-expressions" 
"Generally speaking, our S-expression based form can come in either a
syntax object form or a raw s-expression form.  We turn the raw datum
form into syntax so that we can use |syntax-case| to parse it."

(@c
(define (sexpr? x) #t)

(define (sexp-wrap s->)
  (lambda (sexp)
    (s-> (datum->syntax #'dummy sexp))))
))

(@* "Syntax Conversions" 
"The syntax conversions allow us to convert a syntax object
representing the datalog program, assertion, and so forth, and convert
it into the abstract syntax tree."

(@c
(define (stx->program stx)
  (syntax-case stx (begin)
    [(begin s ...)
      (map stx->statement#'(s ...))]))
(define sexp->program (sexp-wrap stx->program))
))

(@ "Statements are either assertions, queries, or retractions.  We use
explicit auxiliary keywords for the literals."

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

(@ "A clause is either a literal or it is an implication or horn clause."

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

(@ "A literal is the combination of a datum at the front (the
predicate) and the terms, which are either variables or data."

(@c
(define (stx->literal stx)
  (syntax-case stx ()
    [(p t ...)
      (make-literal
        stx (stx->datum #'p)
        (map stx->term (syntax->list #'(t ...))))]))
(define sexp->literal (sexp-wrap stx->literal))
))

(@ "A term is either a variable or a constant."

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

(@ "This are just some ways of handling data."

(@c
(define (datum-syntax? stx)
  (define d (syntax->datum stx))
  (or (symbol? d) (string? d) (number? d)))
(define (stx->datum stx)
  (syntax-case stx ()
    [d
      (datum-syntax? #'d)
      (syntax->datum #'d)]))
))

(@* "Testing Sexpressions"
"These tests should test the various sexpression features."

(@c
(define test
  #'(begin
      (! (parent john douglas))
      (? (parent john douglas))
      (? (parent john ebbon))
      (! (parent bob john))
      (! (parent ebbon bob))
      (? (parent ,A ,B))
      (? (parent john ,B))
      (? (parent ,A ,A))
      (! (:- (ancestor ,A ,B)
             (parent ,A ,B)))
      (! (:- (ancestor ,A ,B)
             (parent ,A ,C)
             (ancestor ,C ,B)))
      (? (ancestor ,A ,B))
      (? (ancestor ,X john))
      (~ (parent bob john))
      (? (parent ,A ,B))
      (? (ancestor ,A ,B))))

(define (run-datalog/sexp-tests) #f)
#;(define (run-datalog/sexp-tests)
  (test-begin "sexp")
    (test-assert "program" (program? (stx->program test)))
    (test-assert "stmt" (assertion? (stx->statement #'(! (parent john douglas)))))
    (test-assert "stmt" (retraction? (stx->statement #'(~ (parent john douglas)))))
    (test-assert "stmt" (query? (stx->statement #'(? (parent john douglas)))))
    (test-assert "clause" 
      (clause-equal? 
        (stx->clause #'(parent john douglas))
        (make-clause #f (make-literal #f 'parent (list (make-constant #f 'john) (make-constant #f 'douglas))) '())))
    (test-assert "clause" 
      (clause-equal? 
        (sexp->clause '(:- (ancestor ,A ,B) (parent ,A ,B)))
        (make-clause #f 
          (make-literal #f 'ancestor (list (make-variable #f 'A) (make-variable #f 'B)))
          (list (make-literal #f 'parent (list (make-variable #f 'A) (make-variable #f 'B)))))))
    (test-assert "literal" 
      (literal-equal? 
        (stx->literal #'(parent john douglas))
        (make-literal #f 'parent (list (make-constant #f 'john) (make-constant #f 'douglas)))))
  (test-end "sexp"))
))
)

(@l "This library provides facilities for pretty-printing Datalog
source.  This code is based on the Common Lisp Formatted output
program |format|, which is opposition to the Racket version of this
code, which uses pprint and some other things.  Actually, there are
now at least two different versions in Racket, one which uses the
pprint libraries, and one that does not."

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

(@ "When we format a datum, it should come out in a reasonable form.
Here are some examples:

\\medskip\\verbatim
> (printf \"~a~n\" (format-datum 'sym))
sym
> (printf \"~a~n\" (format-datum \"str\"))
\"str\"
|endverbatim \\medskip"

(@c
(define (format-datum s)
  (cond
    [(string? s)
      (format "~S" s)]
    [(symbol? s)
      (symbol->string s)]
    [(number? s) 
      (format "~d" s)]))
))

(@ "Variables and constants get formatted just the same as any other
datum.  For example:

\\medskip\\verbatim
> (printf \"~a~n\" (format-variable (make-variable #f 'Ancestor)))
Ancestor
> (printf \"~a~n\" (format-constant (make-constant #f 'joseph)))
joseph
> (printf \"~a~n\" (format-constant (make-constant #f \"whom\")))
\"whom\"
|endverbatim \\medskip"

(@c
(define (format-variable v)
  (format-datum (variable-sym v)))
(define (format-constant c)
  (format-datum (constant-datum c)))
))

(@ "Printing a term is just the same as printing any data,"

(@c
(define (format-term t)
  (cond
    [(variable? t)
      (format-variable t)]
    [(constant? t)
      (format-constant t)]))
))

(@ "A literal can be either a single datum, a predicate, or an infix
predicate.  We special case any of the primitives to be infix binary. For example:

\\medskip\\verbatim
> (printf \"~a~n\" (format-literal (make-literal #f 'true (list))))
true
> (printf \"~a~n\" (format-literal 
  (make-literal (make-literal #f '=
    (list (make-constant #f 'joseph) (make-constant #f 'jay))))))
joseph = jay
> (printf \"~a~n\" 
  (format-literal 
    (make-literal #f 'ancestor
      (list (make-variable #f 'A) (make-constant #f 'jay)))))
ancestor(A, jay).
|endverbatim \\medskip"

(@c
(define primitive-predicates
  '(= >= > < <= <>))
(define (format-literal l)
  (assert (literal? l))
  (let ([pred (literal-predicate l)] [terms (literal-terms l)])
    (cond
      [(null? terms) (format-datum pred)]
      [(and 
        (memq pred primitive-predicates) 
        (list? terms) 
        (= 2 (length terms)))
        (format "~a ~a ~a" 
          (format-term (car terms))
          pred
          (format-term (cadr terms)))]
      [else
        (format "~a(~{~a~^, ~})"
          (format-datum pred)
          (map format-term terms))])))
))

(@ "We may also want to format a list of literals, one per line."

(@c
(define (format-literals ls)
  (format "~{~a\n~}\n"
    (map
      (λ (l)
        (format-assertion
          (make-assertion #f (make-clause #f l '()))))
      ls)))
))

(@ "Formatting a clause is a single literal unless we have a horn
clause, in which case we need to print the head of the clause to the
left of the horn."

(@c
(define (format-clause c)
  (if (null? (clause-body c))
    (format "~a" (format-literal (clause-head c)))
    (format "~a :- ~{\n~4,0t~a~^, ~}"
      (format-literal (clause-head c))
      (map format-literal (clause-body c)))))
))

(@ "Assertions, retractions, and queries are the same as formatting a
clause, with the additional character at the end that determines what
sort of clause it is.  This could be a period, tilde, or question
mark, respectively."

(@c
(define (format-assertion a)
  (format "~a." (format-clause (assertion-clause a))))
(define (format-retraction r)
  (format "~a~~" (format-clause (retraction-clause r))))
(define (format-query q)
  (format "~a?" (format-literal (query-literal q))))
))

(@ "A statement is basically one of either an assertion, query, or
retraction.  "

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

(@l "The Datalog runtime system provides the means for interacting
with theories and make retractions, assertions, and queries."

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
"Environments are modelled using immutable hash tries.  We use the
implementation from Taylor Campbell's library for this, but this
should work with any decent constant time lookup structure.  It would
be interesting to see how the performance is affected by using
association lists instead of hash tries."

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
"Substitutions of the runtime are handled here.  I don't really know
how it all works at the moment, so I am leaving this fairly blank.
Basically, there are substitutions, where you lookup the values in the
environment,and there are renames.  There is also a strange procedure
known as |shuffle|.  I am not sure what shuffling does, and I don't
want to study it too hard at the moment."

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

(@ "Let's make sure that the useful ones are available for the rest of
the code."

(@c
(@< |Substitution procedures|)
))

(@* "Unification module"
"Unification is done in a fairly naive way, so I won't belabor the
point here.  It's standard unification."

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

(@ "Let's make these available at the top-level."

(@c
(@< |Unification procedures|)
))

(@* "Variant term module"
"I don't really know what any of this stuff does.  I really should
document it more clearly at some point.  "

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

(@ "Make them all visible."

(@c
(@< |Variant term procedures|)
))

(@* "Clause safety"
"A clause is safe if every variable in its head occurs in some literal in its body."

(@c
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
"We need to define some simple predicates to distinguish and recognize
the various theories.  We aren't using disjoint types here, but that
isn't too important at this point.  It would be better to provide a
bit more abstraction here, because these are visible to the outside
world, though."

(@c
(define (theory? x)
  (or (hash-trie? x) (hashtable? x)))
(define (immutable-theory? x)
  (hash-trie? x))
(define (mutable-theory? x)
  (hashtable? x))
))

(@* "Keys"
"I don't know."

(@c
(define (literal-key l)
  (format "~a/~a" (literal-predicate l) (length (literal-terms l))))
(define (clause-key c)
  (literal-key (clause-head c)))
))

(@* "Theory Constructors"
"The basic constructors for mutable and immutable theories.  I use a
hash-trie for the immutable theories, and a mutable hashtable for the
mutable theories.  Both of these work on |equal?| equivalence."

(@c
(define (make-immutable-theory)
  (make-hash-trie hash-trie-type:equal))
(define (make-mutable-theory)
  (make-hashtable equal-hash equal?))
))

(@* "Theorem Functions"
"Finally we can define the basic runtime functions. 

\\medskip\\verbatim
(assume theory clause) 
(assume! theory clause)
(retract theory clause)
(retract! theory clause)
(get theory literatl)
|endverbatim
\\medskip

\\noindent
The mutating versions are marked by the exclamation point and work on
mutable theories, while the non-mutable ones return newly extended or
retracted theories from immutable theories.  "

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
"A subgoal is used internally when dealing with searching for problem
solutions."

(@c
(define-record-type subgoal
  (fields
    literal
    (mutable facts)
    (mutable waiters)))
))

(@* "Proving Theories"
"The actual proving techniques are a little troublesome to understand,
but they basically use a tabling technique for the intermediate
results, which is supposedly a well known technique for doing fast
proofs.")

(@ "We have the following primitive predicates, which are used further
down in the following code."

(@c
(define primitive-predicates
  `((= . ,term-equal?)
    (>= . ,term->=?)
    (> . ,term->?)
    (< . ,term-<?)
    (<= . ,term-<=?)
    (<> . ,term-<>?)))
))

(@ "When attempting to deal with subgoals that are literals, we need to
handle the case when these literals are primitive predicates.  We do
this by unifying the terms if necessary and then checking whether the
predicate holds.  If it does, we insert it into our system as a fact."

(@> |Deal with primitves| (capture sg sglit fact!)
(λ (pred-pair)
  (let (
      [srcloc (literal-srcloc sglit)] 
      [terms (literal-terms sglit)]
      [pred-sym (car pred-pair)]
      [term-test? (cdr pred-pair)])
    (define (test a b)
      (when (term-test? a b)
        (fact! sg (make-literal srcloc pred-sym (list a b)))))
    (cond
      [(unify-term (empty-env) (car terms) (cadr terms)) =>
        (λ (env)
          (test 
            (subst-term env (car terms))
            (subst-term env (cadr terms))))]
      [else (test (car terms) (cadr terms))])))  
))

(@ "The rest of the prover follows, and I don't really understand how
it works."

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
          (= 2 (length (literal-terms sglit))) 
          (assq (literal-predicate sglit) primitive-predicates)) 
          =>
          (@< |Deal with primitves| sg sglit fact!)]
        [else (search-theory! sg)])))
  (define sg (make-subgoal lit '() '()))
  (literal-tbl-replace! subgoals lit sg)
  (search! sg)
  (subgoal-facts sg))
))
)

(@l "This library provides facilities for evaluating Datalog programs,
statements, and so forth."

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
"We define a parameter for use as an implicit parameter for some
procedures, namely, |eval-program| and |eval-statement|.  "

(@c
(define current-theory (make-parameter (make-mutable-theory)))
))

(@* "Safe Assumptions"
"We need to have some checking that we can perform any time we want to
have some sort of assumption.  We use the |safe-clause?| predicate to
ensure that we have a safe assumption clause before we actually do the
assumption."

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
"When we get back results, it is good to print the results to the
standard output.  We use |print-literals| defined below for this
purpose."

(@c
(define (print-literals ls)
  (printf "~a\n" (format-literals ls)))
))

(@* "Evaluation"
"Programs can be evaluated using |eval-program| as follows:

\\medskip\\verbatim
(eval-program datalog-program)
|endverbatim
\\medskip

\\noindent 
If any results are returned, then they will be printed out.  This uses
the |current-theory| parameter as the theory on which to operate."

(@c
(define (eval-program p)
  (for-each 
    (λ (s)
      (define v (eval-statement s))
      (unless (eq? (void) v)
        (print-literals v)))
    p))
))

(@ "Statements are evaluated with the appropriate runtime procedures."

(@c
(define (eval-statement s)
  (cond
    [(assertion? s)
      (assume-if-safe assume! (current-theory) s)]
    [(retraction? s)
      (retract! (current-theory) (retraction-clause s))]
    [(query? s)
      (prove (current-theory) (query-literal s))]))
))

(@ "|eval-program/fresh| let's you evaluate a program in a fresh
theory, and have that theory returned after completing the program.
Any query responses are printed out as they are encountered."

(@c
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
"The following convenience form makes it easy to spawn a new
interactive Datalog REPL for playing around with a specific datalog
theorem.

\\medskip\\verbatim
(datalog-repl)
(datalog-repl theory)
|endverbatim
\\medskip

\\noindent
The nullary version of this procedure creates a new REPL using
|new-cafe| that has an empty, new, mutable theory.  You can run SEXP
datalog commands in it and it will print out the results of queries.
The unary version uses the given theory as its starting point.  We can
create a new cafe like so:"

(@> |New datalog cafe| (capture theory)
(new-cafe
  (λ (exp)
    (let ([res (eval-statement (sexp->statement exp))])
      (when (pair? res)
        (display (format-literals res))))))
))

(@ "We can then use |case-lambda| for a nice little datalog repl
creating procedure."

(@c
(define datalog-repl
  (case-lambda
    [() 
      (let ([theory (make-mutable-theory)]) 
        (@< |New datalog cafe| theory))]
    [(theory) (@< |New datalog cafe| theory)]))
))
)

(@l "This package contains a lightweight deductive database system.
Queries and database updates are expressed using Datalog --- a
declarative logic language in which each formula is a function-free
Horn clause, and every variable in the head of a clause must appear in
the body of the clause.  The use of Datalog syntax and an
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
  term-equal? term-<? term-<=? term->? term->=? term-<>?
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
  run-datalog-tests
)
(import
  (arcfide datalog ast)
  (arcfide datalog sexp)
  (arcfide datalog pretty-printing)
  (arcfide datalog runtime)
  (arcfide datalog eval)
  (chezscheme)
  (srfi :64))

(@ "This package only re-exports various procedures.  It also provides
the following grouping for the testing suites."

(@c
(define (run-datalog-tests)
  (parameterize ([test-runner-current (test-runner-simple)])
    (test-begin "Datalog")
    (run-datalog/ast-tests)
    (test-end "Datalog")))
))

)
