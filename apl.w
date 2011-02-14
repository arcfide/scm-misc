#!chezscheme
(@chezweb)

;;; This file assumes 8-space TABs

"\\centerline{
  \\titlef Sapling: APL for Scheme}
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

(@l "Anyone who uses Scheme will no doubt question whether or not APL and
Scheme go together at all. Indeed, I wondered this myself. However,
I have always had a strange, almost uncontrollabe fascination with APL,
and while this has, at times, almost compelled me to actually use it, the
overall, longstanding attachment and familiarity with Scheme has prevented
me from making such leaps. Indeed, APL as a language lacks a number of
features that I have come to rely on, including syntactic abstraction.
However, despite my best efforts, I have not been able to escape APL.
Thus, it occured to me that rather than fighting it, I should just give in.
Scheme lacks good multipurpose,
portable array libraries that can form an useful base for other work.
Why not start with APL?
Sure, it's a little strange,
but it's no stranger than the mathematical notation
that we spend every day working with.

And so, I present to the reader a library for Scheme
whose purpose is to bring APL to Schemers.
I intend to take liberties where liberties make sense,
and to try to provide as much as I can of APL to the Scheme audience.
Good luck."

(arcfide apl)
(export
  make-scalar ;; Temporary remove later
  apl-value-case scalar array
  make-apl-array
  ->apl-value
  scalar? scalar-value
  apl-array? apl-array-accessor
  apl-value?)
(import
  (rename (chezscheme)
    (assert scheme-assert)))

(@* "Assertions"
"I want to enable a lot of good type checking in this library,
but I also want to make it possible for this thing to go really fast.
To this end, I'm doing a slight tweak on the standard |assert| syntax.
It will work just like a regular |assert| whenever the optimization level
is less than 3. But whenever it is of level three or higher,
then the |assert| will be a no-op."

(@c
(define-syntax (assert x)
  (syntax-case x ()
    [(_ expr)
      (if (< 2 (optimize-level))
	  #'(begin)
	  #'(scheme-assert expr))]))))

(@* "Miscellaneous Utilities"
"I use the following miscellaneous functions throughout the code, so I define them here for easy access."

(@c
(define (id x) x)
(define (vector-for-all pred? v)
  (let ([len (vector-length v)])
    (let loop ([i 0])
      (or (>= i len)
	  (and (pred? (vector-ref v i))
	       (loop (fx1+ i)))))))
(define (vector-fold-left p s v)
  (do ([i 0 (fx1+ i)]
       [res s (p res (vector-ref v i))])
      [(>= i (vector-length v)) res]))
))

(@* "The APL Datatype"
"APL is actually a dynamic language without many datatypes.
The basic number is supported, as well as strings of characters,
which work like vectors of characters,
and then the array type. That's the most interesting.
Basically, You can think of a single number as a zero-dimensional array,
a string as a one dimensional array of characters,
and everything else are just various dimensions of arrays.
The important thing is to design a datatype
that will have good properties for the type of data processing APL does.
We need to have a representation that let's us reshape and work with the
arrays without having to do a lot of manipulation.
I would also like something that is fairly cache friendly.
Right now, my simple and ``good enough'' solution is to have one vector
that talks about the dimensions of the array, and another that stores the elements.

In Chez Scheme, the largest possible vector that you can create is a vector
of size |(most-positive-fixnum)|.
This is way bigger on my 64-bit machine than I could hope to address,
so I am not going to worry about handling anything larger than that.
I assume that my arrays will contain fixnum vectors for their dimensions,
and regular vectors for their contents.
Later on, it may be possible to optimize this, but I think that using this generic
vector datatype for the moment will work fine.
The following table summarizes the APL array structures.

$$\\vbox{
  \\offinterlineskip
  \\halign{
    \\strut # & # & # \\cr
    {\\bf Rank} & {\\bf Name of Array} & {\\bf Description} \\cr
    \\noalign{\\hrule}
    0 & Scalar & One item arranged along no axes. \\cr
    1 & Vector & Zero or more items arranged along one axis. \\cr
    2 & Matix & Zero or more items arranged along two axes. \\cr
    3 & --- & Zero or more items arranged along $n$ axes. \\cr
    \\noalign{\\hrule}
  }
}$$

\\noindent
When we want to deal with these, we want to be able to talk about them
all as being APL values, without having to worry about Scheme
structures that look very similar.
Thus, I have the core |apl-value| type which holds them all.
It has two fields: one to hold the rank, dimensions and the like
in the form of the aforementioned |fxvector| and the other to
hold the data object that contains the values."

(@c
(define-record-type (apl-value make-apl-value apl-value?)
  (fields
    (immutable dimensions get-dims)
    (immutable contents get-contents))
  (protocol
    (lambda (n)
      (lambda (dimensions contents)
	(assert (fxvector? dimensions))
	(n dimensions contents)))))
))

(@
"We also want something analogous to |make-vector| that allows a
programmer to construct new, empty APL arrays."

(@c
(define (make-apl-array initial . dims)
  (unless (or (scalar? initial) (apl-array? initial))
    (errorf 'make-apl-array "invalid initial value ~s" initial))
  (unless (for-all fixnum? dims)
    (errorf 'make-apl-array "non-fixnum dimensions ~s" dims))
  (make-apl-value
    (list->fxvector dims)
    (make-vector
      (apply fx+ dims)
      initial)))
))

(@* "Converting Scheme values to APL values"
"I would like to define some sort of correspondence between APL values
and native Scheme values.
Later, such a correspondence may enable me to treat certain types of
values more efficiently, but my current motivation for doing this is
to help with conversion.
APL functions might be written to handle any arbitrary Scheme values,
but I hesitate to do this because it complicates the functions.
Instead, I propose an explicit converter that an user must use to
cast native Scheme values into ther appropriate APL equivalents.

\\medskip\\verbatim
(->apl-value v)
|endverbatim
\\medskip

\\noindent
The procedure |->apl-value| should take any value and produce some
valid |apl-value|.
It should raise an error if it cannot convert the value.
The following details the conversions.

$$\\vbox{
  \\offinterlineskip
  \\halign{
    \\strut # \\hfil & # \\hfil \\cr
    {\\bf Scheme Value} & {\\bf Equivalent APL Value} \\cr
    \\noalign{\\hrule}
    Number & Number \\cr
    Boolean & 0/1 \\cr
    Character & Character \\cr
    String & APL Vector of Characters \\cr
    Vector of arbitrary values & APL Vector of APL values \\cr
    Vector $\\times$ Vector $\\times\\ldots\\times$ Vector & APL Array \\cr
    \\noalign{\\hrule}
  }
}$$

\\noindent
When we talk about these values, we should remember to deeply convert
the native Scheme values; they are not much use to us otherwise."

(@> |Define APL value converter| (export ->apl-value)
(define (->apl-value x)
  (cond
    [(number? x) x]
    [(char? x) x]
    [(boolean? x) (if x 1 0)]
    [(string? x)
     (make-apl-value
       (fxvector (string-length x))
       (let ([v (make-vector (string-length x))])
	 (do ([i 0 (fx1+ i)])
	     [(>= i (string-length x)) v]
	   (vector-set! v i (string-ref x i)))))]
    [(and (vector? x) (vector-map ->apl-value x))
     =>
     (lambda (x) (@< |Convert vector| x))]
    [else (error '->apl-value "apl conversion failed" x)]))
))

(@
"The most complex transformation in |->apl-value| by far concerns the
handling of vectors.
If we always assumed that a vector was a single dimensional APL
vector, we would be fine.
However, I would rather catch the situation where we have nested
vectors of the same shape that form matrices or the like.
To that end, assuming that we have already verified that we have
received a vector and that this vector has had all of its elements
converted with |->apl-value|, we want to check to see just how close
to an $n$-dimensional APL array we can make the vector.
I only consider makeing an APL array from code vectors whose internal
elements ar all APL values of the same shape.
In all other cases, I want to do the more na\\\" ive conversion."

(@> |Convert vector| (capture x)
(cond
  [(fxzero? (vector-length x))
   (make-apl-value (fxvector 0) x)]
  [(not (vector-for-all apl-value? x))
   (make-apl-value (fxvector (vector-length x)) x)]
  [else
    (let ([shape
	    (let ([s1 (get-dims (vector-ref x 0))])
	      (and
		(vector-for-all
		  (lambda (v) (equal? s1 (get-dims v)))
		  x)
		s1))])
      (if shape
	  (@< |Create an APL array from a vector| x shape)
	  (make-apl-value
	    (fxvector (vector-length x))
	    x)))])
))

(@
"If we are lucky enough to find such an array, we want to add a
dimension to the shape of the array, and then fill in the new array
vector with the values from all of the old vectors.
A little work with modular arithmetic and some conversion should get
us where we want to go."

(@> |Create an APL array from a vector| (capture x shape)
(let* ([shape-dims (fxvector->list shape)]
       [shape-size (apply * shape-dims)]
       [dims (cons (vector-length x)
		   (fxvector->list shape))]
       [len (* (car dims) shape-size)]
       [v (make-vector len)])
  (do ([i 0 (fx1+ i)])
      [(>= i len)
       (make-apl-value (list->fxvector dims) v)]
    (let-values ([(j k) (div-and-mod i shape-size)])
      (vector-set! v i
	(vector-ref
	  (get-contents (vector-ref x j))
	  k)))))
))

(@
"Let us move |->apl-value| into the top-level."

(@c
(@< |Define APL value converter|)
))

(@* "Type Dispatch"
"When dealing with APL values, one should not have to think about the
underlying implementation mechanism of the value.
Namely, the programmer should not be burdened with whether or not the
arrays are accessed as vectors or some other specialized value.
He should not be burdened with the array arrithmetic, nor should we
burden him with the responsibility of determining what sort of value
he has.
Instead, we should build up a |case| like dispatch system for the APL
datatype.

To begin, the more primitive concepts of type predicates and accessors
should be defined.
I will make two distinctions between APL values:
we have either arrays or we have scalars.
Scalars may be numbers or characters, and arrays may have one or more
dimensions (this is known as rank)."

(@c
(define (scalar? x)
  (or (number? x)
      (char? x)
      (and (apl-value? x)
	   (zero? (fxvector-length (get-dims x))))))
(define (scalar-value x)
  (cond
    [(or (number? x) (char? x)) x]
    [(apl-value? x)
     (if (zero? (fxvector-length (get-dims x)))
	 (get-contents x)
	 (error 'scalar-value "not a scalar" x))]
    [else
      (error 'scalar-value "not a scalar" x)]))
))

(@
"When we have an array, we want to also define an accessor for the
content, a setter, and an index function.

\\medskip\\verbatim
(getter i)
(setter i v)
(index i j ...)
|endverbatim
\\medskip

\\noindent
The index function takes as many arguments as the rank of the APL
array that it indexes. It returns the index into the data structure
used to store the APL values. Both the getter and the setter accept
the index as one of the arguments to specify the slot in the array."

(@> |Define APL Array Predicate and Accessor Generator|
(export apl-array? apl-array-accessor)
(define (apl-array? x)
  (and (apl-value? x)
       (not (zero? (fxvector-length (get-dims x))))))
(define (apl-array-accessor x)
  (unless (apl-array? x)
    (error 'apl-array-accessor "invalid APL array" x))
  (values
    (@< |Create APL Array Getter| x)
    (@< |Create APL Array Setter| x)
    (@< |Create APL Array Indexer| x)))
))

(@
"The getter should verify its arguments and the grab the index from
the contents vector of the array."

(@> |Create APL Array Getter| (capture x)
(let* ([contents (get-contents x)]
       [len (vector-length contents)])
  (lambda (i)
    (when (>= i len)
      (errorf #f "range mismatch: max: ~d; index: ~d"
	(fx- len 1) i))
    (vector-ref contents i)))
))

(@
"The setter needs to do the same as the getter, but it should verify
that it receives a valid input value. Of course, it uses |vector-set!|
instead of |vector-ref|."

(@> |Create APL Array Setter| (capture x)
(let* ([contents (get-contents x)]
       [len (vector-length contents)])
  (lambda (i v)
    (when (>= i len)
      (errorf #f "range mismatch: max: ~d; index: ~d"
	(fx- len 1) i))
    (unless (or (apl-value? v) (scalar? v))
      (errorf #f "invalid APL value: ~s" v))
    (vector-set! contents i v)))
))

(@
"The indexer is similarly easy, but we have to make our indexer
generic enougth to handle arrays of any rank.
We also need to do more error checking here to make sure that we catch
dimension and rank mismatches as early as possible.
Otherwise, nothing complex here."

(@> |Create APL Array Indexer| (capture x)
(let ([dims (get-dims x)])
  (lambda coord
    (define (add-coord dim i res)
      (let ([size (fxvector-ref dims i)])
	(when (>= dim size)
	  (errorf #f "dimension mismatch: size: ~d; index: ~d"
	    size dim))
	(if res
	    (+ (* dim size) res)
	    dim)))
    (unless (= (fxvector-length dims) (length coord))
      (errorf #f "rank mismatch, rank: ~d; given: ~d"
	(fxvector-length dims)
	(length coord)))
    (fold-right add-coord #f coord (iota (length coord)))))
))

(@
"Let's put the APL Array procedures at the top-level."

(@c
(@< |Define APL Array Predicate and Accessor Generator|)
))

(@
"Now that we have the basic predicates and a means of creating
special accesssors for APL arrays, it makes sense to create a sort of
general |case| statement for APL values.
This allows us to dispatch on the specific type of APL value that we a
receive.
We might have something that looks like this:

\\medskip\\verbatim
(apl-value-case exp
  [(scalar val) ---]
  [(array (n m) index get set) ---]
  [(array size index get set) ---]
  [else ---])
|endverbatim
\\medskip

\\noindent
In the above example we have a scalar, a matrix, and a general
APL array of any size.
Notice that in one case we match specifically against the size
of the array that we desire, and in the other we do not."

(@c
(define-syntax (scalar x)
  (syntax-violation #f "misplaced aux keyword" x))
(define-syntax (array x)
  (syntax-violation #f "misplaced aux keyword" x))
(define-syntax apl-value-case
  (syntax-rules (%internal scalar array else)
    [(_ %internal x [else e1 e2 ...])
     (begin e1 e2 ...)]
    [(_ %internal x [(scalar v) e1 e2 ...])
     (when (scalar? x) (let ([v (scalar-value x)]) e1 e2 ...))]
    [(_ %internal x [(array (n1 n2 ...) index get set) e1 e2 ...])
     (when (and (apl-array? x)
		(= (length '(n1 n2 ...)) (fxvector-length (get-dims x))))
       (let-values ([(get set index) (apl-array-accessor x)]
		    [(n1 n2 ...)
		     (apply values (fxvector->list (get-dims x)))])
	 e1 e2 ...))]
    [(_ %internal x [(array size index get set) e1 e2 ...])
     (when (apl-array? x)
       (let ([size (fxvector->list (get-dims x))])
	 (let-values ([(get set index) (apl-array-accessor x)])
	   e1 e2 ...)))]
    [(_ %internal x [(scalar v) e1 e2 ...] rest ...)
     (if (scalar? x)
	 (let ([v (scalar-value x)]) e1 e2 ...)
	 (apl-value-case %internal x rest ...))]
    [(_ %internal x [(array (n1 n2 ...) index get set) e1 e2 ...] rest ...)
     (if (and (apl-array? x)
	      (= (length '(n1 n2 ...)) (fxvector-length (get-dims x))))
	 (let-values ([(get set index) (apl-array-accessor x)]
		      [(n1 n2 ...)
		       (apply values (fxvector->list (get-dims x)))])
	   e1 e2 ...)
	 (apl-value-case %internal x rest ...))]
    [(_ %internal x [(array size index get set) e1 e2 ...] rest ...)
     (if (apl-array? x)
	 (let ([size (fxvector->list (get-dims x))])
	   (let-values ([(get set index) (apl-array-accessor x)])
	     e1 e2 ...))
	 (apl-value-case %internal x rest ...))]
    [(_ exp c1 c2 ...)
     (let ([tmp exp]) (apl-value-case %internal tmp c1 c2 ...))]))
))

(@* "Scalar Functions"
"Scalar functions behave the most regularly among APL's set of primitive
operations.
They have defined operations on scalars, and we define their
behavior on arrays in terms of an element-wise application of the
function to each scalar element.
The ouput array has the same shape as the input array or arrays,
and most of the logic can be abstracted away, which is what we do
here.

\\medskip\\verbatim
(make-scalar who f)
|endverbatim
\\medskip

\\noindent
I define a higher-order procedure |make-scalar| that takes the scalar
function definition |f| as a procedure that handles one or two scalars.
It returns the appropriate scalar function defined over all APL
values.
The |who| should be the name symbol of the function; |make-scalar|
will use |who| when reporting errors."

(@c
(define (make-scalar who f)
  (rec loop
    (case-lambda
      [(x)
       (apl-value-case x
	 [(scalar v) (f v)]
	 [(array size index get set)
	  (let ([len (apply + size)])
	    (let ([res (apply make-apl-array 0 size)])
	      (let-values ([(res-get res-set res-idx)
			    (apl-array-accessor res)])
		(do ([i 0 (fx1+ i)]) [(>= i len) res]
		  (res-set i (loop (get i)))))))])]
      [(x y)
       (apl-value-case x
	 [(scalar xv)
	  (apl-value-case y
	   [(scalar yv) (f xv yv)]
	   [(array size index get set)
	    (let ([len (apply + size)])
	      (let ([res (apply make-apl-array 0 size)])
		(let-values ([(res-get res-set res-idx)
			      (apl-array-accessor res)])
		  (do ([i 0 (fx1+ i)]) [(>= i len) res]
		    (res-set i (loop x (get i)))))))])]
	 [(array size index get set)
	  (apl-value-case y
	   [(scalar yv)
	    (let ([len (apply + size)])
	      (let ([res (apply make-apl-array 0 size)])
		(let-values ([(res-get res-set res-idx)
			      (apl-array-accessor res)])
		  (do ([i 0 (fx1+ i)]) [(>= i len) res]
		    (res-set i (loop (get i) y))))))]
	   [(array ysize index yget yset)
	    (unless (equal? size ysize)
	      (errorf who "shape mismatch ~s vs. ~s" size ysize))
	    (let ([len (apply + size)])
	      (let ([res (apply make-apl-array 0 size)])
		(let-values ([(res-get res-set res-idx)
			      (apl-array-accessor res)])
		  (do ([i 0 (fx1+ i)]) [(>= i len) res]
		    (res-set i (loop (get i) (yget i)))))))])])])))
))

)
