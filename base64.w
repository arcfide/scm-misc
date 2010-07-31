#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Base64 Encoding/Decoding Library}
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

(@l "The following library is a simple set of procedures to decode and encode 
base64 data to and from bytevectors."

(arcfide base64)
(export 
  base64-encode
  base64-decode)
(import
  (rnrs base)
  (rnrs arithmetic fixnums)
  (rnrs bytevectors)
  (rnrs mutable-strings)
  (riastradh foof-loop)
  (rnrs control))


(@* "Encoding Data"
"Base64 encoding consists of repeated converting 3-byte packets of 
the incoming byte stream (in big endian order) into 4 character 
output strings by indexing to a characteer table by 6-bit indexes 
grabbed in order from the 3-byte packet."

(@> |Define base64-encode| (export base64-encode)
(define (base64-encode bvs)
  (let* ([bvs-len (bytevector-length bvs)]
	 [res-len (fx* 4 (ceiling (/ bvs-len 3)))]
	 [res (make-string res-len)])
    (@< |Define b64-char| bvs-len bvs)
    (iterate! 
      ([for i (up-from 0 (to bvs-len) (by 3))]
       [for j (up-from 0 (by 4))])
	([for si (up-from 1 (to 5))]
	 [for sj (up-from j (to (+ j 4)))])
      (string-set! res sj (b64-char i si)))
    res))
))

(@ "To actually extract the character from the vector, we use the 
following:"

(@> |Define b64-char| (export b64-char) (capture bvs-len bvs)
(define (b64-char i j)
  (assert (fx<=? 1 j 4))
  (if (or (and (fx=? 2 (fx- bvs-len i)) (fx=? j 4))
	  (and (fx=? 1 (fx- bvs-len i)) (fx>? j 2)))
      #\=
      (let ([n (if (> 3 (fx- bvs-len i))
		   (fxarithmetic-shift-left 
		     (bytevector-uint-ref 
		       bvs i (endianness big) (fx- bvs-len i))
		     (fx* 8 (fx- (fx+ i 3) bvs-len)))
		   (bytevector-uint-ref bvs i (endianness big) 3))])
	(vector-ref b64-character-index
	  (fxand 63 (fxarithmetic-shift-right n (fx- 24 (fx* 6 j))))))))
))

(@ "Throw this to the top-level."

(@c
(@< |Define base64-encode|)
))

(@ "The actual bytevector character index is a vector 
of characters A-Z, a-z, 0-9, +/-. The |=| padding 
character is return by |b64-char| if it detects it is necessary."

(@c
(define b64-character-index
  '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L 
     #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
     #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l
     #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
     #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-))
))

(@* "Decoding Data"
"Decoding is a simple reverse."

(@> |Define base64-decode| (export base64-decode)
(define (base64-decode str)
  (@< |Define compute-bytes| str)
  (let ([res (make-bytevector (@< |Get buffer size| str))])
    (loop ([for i (up-from 0 (to (string-length str)) (by 4))]
	   [for j (up-from 0 (by 3))])
      => res
      (let-values ([(b1 b2 b3) (compute-bytes i)])
	(bytevector-u8-set! res j b1)
	(when b2 (bytevector-u8-set! res (fx+ j 1) b2))
	(when b3 (bytevector-u8-set! res (fx+ j 2) b3))))))
))

(@ "Computing the buffer size follows the formula:"

(@> |Get buffer size| (capture str)
(let ([len (string-length str)])
  (fx- (fx* 3 (fxdiv len 4))
       (fx+ (if (char=? #\= (string-ref str (fx- len 1)))
		1 0)
	    (if (char=? #\= (string-ref str (fx- len 2)))
		1 0))))
))

(@ "|compute-bytes| actually does the dirty work. It grabs the 
four characters out of the string and converts them into 
the proper fields."

(@> |Define compute-bytes| (export compute-bytes) (capture str)
(define (compute-bytes i)
  (define (grab j)
    (let ([c (string-ref str (+ i j))])
      (and (not (char=? #\= c))
	   (loop ([for i (up-from 0)]
		  [for vc (in-vector b64-character-index)]
		  [until (char=? c vc)])
	     => i))))
  (let ([c1 (grab 0)]
	[c2 (grab 1)]
	[c3 (grab 2)]
	[c4 (grab 3)])
    (values
      (fx+ (fxarithmetic-shift-left c1 2)
	   (fxand 3 (fxarithmetic-shift-right c2 4)))
      (and c3
	   (fx+ (fxarithmetic-shift-left (fxand 15 c2) 4)
		(fxand 15 (fxarithmetic-shift-right c3 2))))
      (and c4 
	   (fx+ (fxarithmetic-shift-left (fxand 3 c3) 6) c4)))))
))

(@ "Throw the |base64-decode| procedure into the top-level."

(@c
(@< |Define base64-decode|)
))

)
