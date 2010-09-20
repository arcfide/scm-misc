#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Variant Types in Scheme}
\\bigskip
\\centerline{Aaron W. Hsu {\\tt <arcfide@sacrideo.us>}}
\\medskip
\\centerline{\\today}\\par
\\bigskip\\rendertoc\\par
\\vfill
\\noindent
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
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.\\par
"

(@l "This library implements Taylor Campbell's interface for variant
types.  In general, a variant type can be thought of as a type that is
disjoint from other types, but which has a number of constructors for
creating variant or different forms of that type.  It is much the same
as creating a parent record and child records, with convenient
features for dispatching on those types."

(arcfide variant-type)
(export define-variant-type variant)
(import 
	(rnrs base) 
	(rnrs syntax-case) 
	(rnrs records syntactic) 
	(rnrs enums)
	(only (chezscheme) meta void))

(@* "Usage"
"\\medskip\\verbatim
(define-variant-type <type-id> <descriptor-id> <predicate> <case-name>
  (variant <clause-name> (<constructor> <arguments> ...)) ...)
|endverbatim
\\medskip

\\noindent The above creates a new type with the id |<type-id>| and a
record desciptor |<descriptor-id>| that can be identified and
distinguished from other types using the type predicate
|<predicate>|.

A variant type is created using one of the |<constructor>|
procedures, which take the associated |<arguments> ...|.

The |<case-name>| form is bound to a dispatching form that has the
following usage:

\\medskip\\verbatim
(<case-name> <test-datum-expression>
  ((<clause-name> <name> ...) ---)
   ...)
|endverbatim

\\noindent The number of |<name> ...| identifiers must be equal to
the number of |<arguments> ...| associate with the
|<clause-name>|.  The |<test-datum-expression>| should evaluate to
a value that is a |<type-id>| (i.e.  --- returns |#t| when passed
to |<predicate>|).

If the given value was created using the |<constructor>| associated
with |<clause-name>| then the corresponding body of that expression
is evaluated and becomes the value of the entire case expression.
This works just like |case|.  Additionally, if one of the
|<clause-name>| elements is actually an |else| auxilary keyword,
occurs at the end, and none of the above forms have been satisfied,
the value of the whole case is the value of the body expression of the
|else| clause.")

(@* "Implementation"
"The general form of |define-variant-type| will expand into a mostly 
syntactic system, in hopes of avoiding the inefficiencies of the procedural 
record interface.

I start with a basic template for handling |define-variant-type|."

(@> |Define define-variant-type syntax| (export define-variant-type)
(define-syntax (define-variant-type x)
	(syntax-case x (variant)
		[(_ type-id descriptor-id variant-type? variant-case (variant type (maker args (... ...))) (... ...))
			(with-syntax (
					[((getter (... ...)) (... ...)) (map generate-temporaries #'((args (... ...)) (... ...)))]
					[(pred (... ...)) (generate-temporaries #'(maker (... ...)))]
					[(name (... ...)) (generate-temporaries #'(type (... ...)))])
				#'(begin 
					(define-record-type (type-id make-variant-type variant-type?))
					(define-record-type (name maker pred) (parent type-id)
						(fields (immutable args getter) (... ...)))
					(... ...)
					(define descriptor-id (record-type-descriptor type-id))
					(@< |Variant Case| 
						variant-case (pred (... ...)) ((getter (... ...)) (... ...)) (type (... ...)))))]))
))

(@ "The variant case form is the hard one.

Roughly speaking, the variant case should expand into a case over the variant 
type field stored in the passed record. When comparing each type, if the right 
type is found, then we need to evaluate the body in a context where the 
values given in the original form are bound to the field contents associated with 
those field positions in the record."

(@> |Variant Case| (export (variant-case %variant-case %%variant-case))
(capture variant-case (pred ...) ((getter ...) ...) (type ...))
(meta define (subtype-predicate t)
	(case (syntax->datum t)
		[(type) #'pred] ...
		[else (syntax-violation #f "invalid variant type" t)]))
(meta define (subtype-fields t)
	(case (syntax->datum t)
		[(type) #'(getter ...)] ...
		[else (syntax-violation #f "invalid variant type" t)]))
(define-syntax (variant-case x)
	(syntax-case x (else)
		[(_ expr (else body (... ...))) #'(begin body (... ...))]
		[(_ expr (else body (... ...)) other stuff (... ...))
			(syntax-violation 'variant-case "else clause not final clause" x)]
		[(_ expr f1 f2 (... ...)) #'(%variant-case expr (f1) f2 (... ...))]))
(define-syntax (%variant-case x)
	(syntax-case x (else)
		[(_ expr (forms (... ...))) #'(%%variant-case expr () forms (... ...))]
		[(_ expr (forms (... ...)) (else body (... ...)))
			#'(%%variant-case expr (body (... ...)) forms (... ...))]
		[(_ expr (forms (... ...)) (else body (... ...)) other stuff (... ...))
			(syntax-violation 'variant-case "else clause not final clause" x)]
		[(_ expr (forms (... ...)) f1 f2 (... ...))
			#'(%variant-case expr (forms (... ...) f1) f2 (... ...))]))
(define-syntax (%%variant-case x)
	(syntax-case x ()
		[(_ expr [else-body (... ...)] [(subtype vals (... ...)) body (... ...)] (... ...))
			(with-syntax (
					[(test? (... ...)) (map subtype-predicate #'(subtype (... ...)))]
					[((fields (... ...)) (... ...)) (map subtype-fields #'(subtype (... ...)))])
				#`(let ([rec expr])
					(cond 
						[(test? rec)
							(let ([vals (fields rec)] (... ...)) body (... ...))]
						(... ...)
						[else (void) else-body (... ...)])))]))
))

(@ "Also export the explicit auxilary keyword."

(@c
(define-syntax variant 
	(lambda (x)
		(syntax-violation #f "misplaced aux keyword" x)))
(@< |Define define-variant-type syntax|)
))
)