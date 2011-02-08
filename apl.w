 #!chezscheme
(@chezweb)
;;; SET YOUR TABS TO 2 IF YOU WANT TO READ THIS DOCUMENT
;;; THIS DOCUMENT HAS BEEN DESIGNED FOR READING BY BOTH 
;;; FIXED WIDTH AND PROPORTIONAL FONT

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
Sure, it's a little strange, but it's no stranger than the mathematical notation 
that we spend every day working with.

And so, I present to the reader a library for Scheme 
whose purpose is to bring APL to Schemers.
I intend to take liberties where liberties make sense,
and to try to provide as much as I can of APL to the Scheme audience.
Good luck."

(arcfide apl)
(export
  ->apl-value)
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
        #'(scheme-assert expr))]))
))

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

(@ ""

(@c
(define (scalar? x)
  (assert (apl-value? x))
  (and 
    (1d-array? x)
    (fx=? 1 (fxvector-ref (get-dims x) 0))))
(define (1d-array? x)
  (assert (apl-value? x))
  (fx= 1 (fxvector-length (get-dims x))))
(define (->apl x)
  (cond
    [(boolean? x) (boolean->apl x)]
    [(list? x) (list->apl x)]
    [else x]))
(define (boolean->apl x)
  (assert (boolean? x))
  (if x 1 0))
(define (list->apl x)
  (assert (list? x))
  (let ([len (length x)])
    (let ([res (make-vector len)])
      (fold-left
        (lambda (s e)
          (vector-set! res s (->apl e))
          (fx1+ s))
        0
        x))))
(define (vector-shape x)
  (assert (vector? x))
  (vector (vector-length x)))
))

(@* "Dealing with Iteration over two arrays"
"Throughout APL there are functions that work as binary operators on
two arrays.  They often have simple degenerate cases where one of the
arrays is a scalar or a single dimensional array.  Scheme has ways of
dealing with the scalar and single dimensional elements of these
iterations.  Things like Scheme vectors provide procedures like
|vector-map| and strings have the non-standard but common
|string-map|.  When we encounter these special cases, it's fairly easy
to iterate over them.  The more complicated cases come when we are
trying to iterate over our apl arrays.  If we need to iterate over
such arrays more work needs to be done.

When iterating over two arrays, we use the following basic algorithm:

\\medskip\\orderedlist
\\li Verify that the shapes of the two arrays are compatible.
\\li Determine the accessors for the elements of the array.
\\li Determine the range of the iteration.
\\li Iterate.
\\endorderedlist
\\medskip

\\noindent
During the verification step, we need to determine whether the two
arrays are of the same general shape.  This basically means comparing
their dimension vectors.  In the simple, degenerate cases like strings
or vectors, we can do something a little easier.

Accessors are a bit of a funny thing here.  This is where the root of
our current techinque rests.  While any array may have any shape, the
ordinary representation that we use intenerally to store the elements
of the array is just a straight up vector.  This let's us avoid some
of the complicated array arithmetic that often comes with doing these
sorts of operations.  APL let's us get away with this because
functions work element-wise through the arrays.  Because of this, so
long as we have an array bounds to work with, we can treat all of our
iterations as a one-dimensional iteration over a vector.  Thus, our
accessors should be binary procedures which work in the same way that
|vector-ref| works.

Since the contents of an apl array (i.e.  --- the internal
representation) may not match the number of elements required exactly,
we should make sure that we know how to loop over these things.  There
are two approaches to take here.  We could require that the accessors
handle that for us, or we could do our own handling of the wrap around
in this library.  I don't think it really matters which way we go.

This leads to the following general implementation.  We capture the
following bindings:

\\itemitem{$f$} $a\\ b\\to c$.  Our operation that we perform on each
element of the arrays.  The iteration will call $f$ on each pair of
elements $a$ and $b$ in the array.

\\itemitem{$a$ \\& $b$} These are our arrays in their encapsulated form.

\\itemitem{$*-shape$} These are our shape procedure which take an
array and give its shape.  The shape should either be a number, when
the array is a vector, or a fixnum vector of the dimensions.

\\itemitem{$*-vec$} These procedures should take the array and give
back the vector suitable for use in the following vector procedures.

\\itemitem{$*-ref$} These procedures work like |vector-ref| and take
the array and an index.  They should return the element in that array
at that index.

\\itemitem{$*-len$} These procedures should return the length of the
vector they are passed.  They operate on the results returned from the
$*-vec$ pair of procedures.

\\noindent
Right now I am using |fxmod| to do the looping around of the vectors.
that way, we loop through the actual vector we have, and this gives us
the right behavior for the APL array looping.  That is, where we may
have a vector which contains fewer elements in the real vector than
our dimensions indicate.  On the other hand, I do not know if any of
these things are optimal, and my guess is that they are not."

(@> |Iterate over two arrays| 
(capture f a a-ref a-shape a-vec a-vlen b b-ref b-shape b-vec b-vlen)
(define (same-shape? a b) (equal? a b))
(define (count) 
  (let ([s (a-shape a)])
    (cond
      [(number? s) s]
      [else
        (let ([len (fxvector-length s)])
          (do ([i 0 (fx1+ i)] [sum 0 (fx+ sum (fxvector-ref s i))])
            [(fx=? i len) sum]))])))
(assert (same-shape? a b))
(let (
    [count (count)]
    [av (a-vec a)]
    [bv (b-vec b)]
    [sa (a-shape a)]
    [sb (b-shape b)])
  (assert (same-shape? sa sb))
  (let (
      [alen (a-vlen av)] 
      [blen (b-vlen bv)]
      [res (make-vector count)])
    (do ([i 0 (fx1+ i)])
      [(fx>=? i count) (make-apl-value sa res)]
      (let ([ai (fxmod i alen)] [bi (fxmod i blen)])
        (vector-set! res i (f (a-ref av ai) (b-ref bv bi)))))))
))

(@* "Simple Arithmetic Functions"
"The simplest thing to start with is the set of arithmetic functions. 
These operate solely on numbers and can take in any sort of valid data 
that contains only numbers.
Fortunately, these functions all follow a common pattern of cases. 
You have either unary calls or binary calls.
You have either both scalars, both arrays, or a combination of them.
This leads to the following pattern for creating functions."

(@> |Arithmetic Function| (capture single-op op :op)
(case-lambda
  [(x) (single-op x)]
  [(x y) (@< |Compute binary arithmetic function| x y op :op)])
))

(@ "To compute the binary function, we just have to figure out what sorts
of data we are dealing with. As mentioned above, there are really three
situations in which we can land ourselves: both scalar, both arrays,
or one of each. Our first step determines whether at least one of them
is scalar, or not."

(@> |Compute binary arithmetic function| (capture x y op :op)
(define (apl-content-ref x i) (vector-ref (get-contents x) i))
(cond
  [(number? x) (@< |Compute with one scalar| x y op :op)]
  [(boolean? x)
    (let ([b (boolean->apl x)]) 
      (@< |Compute with one scalar| b y op :op))]
  [(vector? x) (@< |Compute with one vector| x y op :op)]
  [(apl-value? x)
    (if (scalar? x)
      (let ([v (vector-ref (get-contents x) 0)])
        (@< |Compute with one scalar| v y op :op))
      (@< |Compute with one array| x y op :op))]
  [else
    (error 'op "Unsupported type" x)])
))

(@ "Once we know whether we have a scalar or an array for the first value,
we just do the same for the second value and then we know what to do.
If we have a scalar, the behavior of APL dictates that that scalar
should be used as one of the arguments for all of the other values in
the array or scalar.  That is, something like:

\\medskip
\\verbatim
(+ 5 #(1 2 3))
|endverbatim
\\medskip

\\noindent
Should return |#(6 7 8)|."

(@> |Compute with one scalar| (capture x y op :op)
(cond
  [(number? y) (:op x y)]
  [(boolean? y) (:op x (boolean->apl y))]
  [(vector? y) (vector-map (lambda (e) (op x e)) y)]
  [(apl-value? y)
    (make-apl-value
      (get-dims y)
      (vector-map (lambda (e) (op x e)) (get-contents y)))]
  [else
    (error 'op "Unsupported type" y)])
))

(@ "On the other hand, if we know that we have an array as the first
argument, then either we
are going to be working as we did above, or we'll be doing everything
element-wise."

(@> |Compute with one array| (capture x y op :op)
(define (sum dims)
  (let ([len (vector-length dims)])
    (do (
        [i 0 (fx1+ i)]
        [sum 0 (fx+ sum (vector-ref dims i))])
      [(fx=? i len) sum])))
(define (iterate n)
  (let* ([dims (get-dims x)] [count (sum dims)])
    (make-apl-value dims
      (let ([c (get-contents x)])
        (if (fx<=? (vector-length c) count)
          (vector-map (lambda (e) (op e n)) c)
          (let ([res (make-vector count)])
            (do ([i 0 (fx1+ i)])
              [(fx=? i count) res]
              (vector-set! res i (op n (vector-ref c i))))))))))
(cond
  [(number? y) (iterate y)]
  [(boolean? y) (iterate (boolean->apl y))]
  [(vector? y) 
    (@< |Iterate over two arrays| op
      x vector-ref get-dims get-contents vector-length
      y vector-ref vector-shape id vector-length)]
  [(apl-value? x)
    (@< |Iterate over two arrays| op
      x vector-ref get-dims get-contents vector-length
      y vector-ref get-dims get-contents vector-length)]
  [else
    (error 'op "Unsupported type" y)])
))

(@ "If we have a vector in the first argument things work basically like
they do when we have an array, but we have a slightly different set of
operations to use."

(@> |Compute with one vector| (capture x y op :op)
(define (iterate n) (vector-map (lambda (e) (op e n)) x))
(cond
  [(number? y) (iterate y)]
  [(boolean? y) (iterate (boolean->apl y))]
  [(vector? y) (vector-map (lambda (a b) (op a b)) x y)]
  [(apl-value? x)
    (@< |Iterate over two arrays| op
      x vector-ref vector-shape id vector-length
      y vector-ref get-dims get-contents vector-length)]
  [else
    (error 'op "Unsupported type" y)])
))


#;(@ "Now let's define some arithmetic operations."

(@c
(define apl+ (@< |Arithmetic Function| id apl+ +))
(define (single- x)
  (cond
    [(number? x) (:- x)]
    [(boolean? x) (:- (boolean->apl x))]
    [(vector? x) (vector-map single- x)]
    [(apl-value? x) (apl-array-map single- x)]
    [else (error '+ "Unsupported type" x)]))
(define apl- (@< |Arithmetic Function| single- apl- -))
))

#;(@ "For all the arithmetic functions, things won't work unless everything is
a number. 
The following procedures help to ensure that things are all type checked."

(@c
(define (apl-number? x)
  (or
    (number? x)
    (boolean? x)
    (and (apl-value? x) (scalar? x))))
(define (assert-numbers/vector x)
  (call-with-current-continuation
    (lambda (k)
      (vector-for-each
        (lambda (e)
          (unless (apl-number? e)
            (k #f)))
        x)
      #t)))
))    
        

)
