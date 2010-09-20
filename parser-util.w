#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Parser Utilities}
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

(@l "The following parsers provide additional conveniences beyond what 
Taylor Campell's parsing combinators provide."

(arcfide parser-util)
(export 
	parser:single-bracketed 
	parser:string:append:repeated
	parser:string:append:at-least
	parser:string:append:repeated-until
	parser:string-number:between
	parser:string-number:at-least
	parser:string-number:exactly
	make-generic-parser)
(import
	(rnrs base)
	(srfi :14)
	(riastradh parscheme)
	(rnrs conditions)
	(rnrs exceptions)
	(rnrs io ports)
	(rnrs bytevectors)
	(only (scheme) with-output-to-string printf))

(@* "Simplified Bracketed Parser"
"This parser is for handling a common case where the bracketing 
parsers are the same."

(@c
(define parser:single-bracketed
	(lambda (b p) (parser:bracketed b b p)))
))

(@* "Concatenating Strings"
"Sometimes we may have parsers that return string results that we want 
to parse together."

(@c
(define parser:string:append:repeated
	(lambda (p)
		(parser:repeated (lambda (x y) (string-append y x))
			(parser:return "") 
			p)))

(define parser:string:append:at-least
	(lambda (k p)
		(*parser [sl (parser:list:at-least k p)]
			(parser:return (apply string-append sl)))))

(define (parser:string:append:repeated-until terminal parser)
	(parser:repeated-until terminal 
		(lambda (x y) (string-append y x))
		(parser:return "")
		parser))
))

(@* "Parsing digits"
"The following help to define specific parsers that convert character streams 
into digits."

(@c
(define-parser (parser:string-number:between i j)
	(*parser [digs (parser:string:between i j 
									 (parser:char-in-set char-set:digit))]
		(parser:return (string->number digs))))

(define-parser (parser:string-number:at-least k)
	(*parser [digs (parser:string:at-least k
									 (parser:char-in-set char-set:digit))]
		(parser:return (string->number digs))))

(define-parser (parser:string-number:exactly k)
	(*parser [digs (parser:string:exactly k
									 (parser:char-in-set char-set:digit))]
		(parser:return (string->number digs))))
))

(@* "Generic Parsers"
"Most of the time parsers will operate on strings, ports, or files, so we 
make it easy to make normal parse procedures for this."

(@c
(define (make-generic-parser parser transcoder)
	(lambda (input)
		(parse-input-chars parser
			(cond 
				[(string? input) (open-file-input-port input (file-options) 'block transcoder)]
				[(bytevector? input) (open-bytevector-input-port input transcoder)]
				[(textual-port? input) input]
				[(binary-port? input) (transcoded-port input transcoder)]
				[else (error #f "Unknown input source." input)])
			#f
			(lambda (res con stream) res)
			(lambda (err con stream)
				(raise
					(condition
						(make-message-condition "Parse error")
						(make-irritants-condition (parse-error/position err))
						(make-message-condition
							(with-output-to-string
								(lambda ()
									(for-each 
										(lambda (x) 
											(if (pair? x)
												(printf "~{~a~^ ~}~%" x)
												(printf "~a~%" x)))
										(parse-error/messages err)))))))))))
))


)