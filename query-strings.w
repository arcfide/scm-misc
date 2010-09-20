#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Query Parameters}
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

(@l "Oleg's CGI Processing utilities work well for CGI code, but if we
just want to handle general query strings, then we need to modify his
code slightly.  The following is essentially a modified version of
[[(oleg cgi processing)]] to allow for it to be used more generally,
with non-cgi applications."

(arcfide query-strings)
(export query-parameter-lookup query-string->parameters)
(import
	(except (rnrs base) error)
	(rnrs programs)
	(rnrs io simple)
	(rnrs mutable-pairs)
	(rnrs unicode)
	(rnrs exceptions)
	(rnrs conditions)
	(rnrs lists)
	(rnrs control)
	(rnrs mutable-strings)
	(only (chezscheme)
		with-input-from-string
		with-output-to-string
		display-condition
		trace-define)
	(only (srfi :13)
		string-index-right
		string-contains
		string-null?
		string-concatenate-reverse)
	(oleg sxml-to-html)
	(oleg prelude)
	(srfi :23)
	(srfi :0)
	(srfi private include))

(@* "Implementation"
"Firstly, we need to bring in the dependencies and other Oleg-isms
that need to be around."

(@c
(define parser-error
	(lambda (port msg . args)
		(apply error (cons msg args))))

(define abort
	(lambda (condition)
		(raise condition)))

(define make-property-condition
	(lambda (type . props)
		(condition
			(make-error)
			(make-who-condition type)
			(make-irritants-condition props))))

(define condition-property-accessor
	(lambda args
		(raise (make-implementation-restriction-violation))))

(include/resolve-ci ("oleg" "ssax" "lib") "char-encoding.scm")
(include/resolve-ci ("oleg" "ssax" "lib") "util.scm")
(include/resolve-ci ("oleg" "ssax" "lib") "input-parse.scm")
))

(@ "|query-string->parameters| makes it easy to convert a query string
into a set of query parameters in the form of an association list.

\\medskip\\verbatim
(query-string->parameters query-string)
|endverbatim
\\medskip

\\noindent
This is roughtly the same as |cgi:url-unquote| from |cgi-util.scm| by
Oleg.  A difference is that we automatically consolidate the
parameters."

(@c
(define (query-string->parameters parm-string)
  (define (consolidate-duplicates alist)
    (let loop ((old-l alist) (new-l '()))
      (cond
        ((null? old-l) new-l)
        ((assq (caar old-l) new-l) =>
          (lambda (duplicate-assoc)
            (let last-link-loop ((link duplicate-assoc))
              (if (null? (cdr link))
                (set-cdr! link (cdar old-l))
                (last-link-loop (cdr link))))
            (loop (cdr old-l) new-l)))
        (else
          (loop (cdr old-l) (cons (car old-l) new-l))))))
  (let ((result '())
        (read-primitive-token
          (lambda ()
            (if (eof-object? (peek-char)) ""
              (next-token '() '(#\= #\+ #\& #\% *eof*) "URL-unquoting")))))
            
    (with-input-from-string parm-string
      (lambda ()
        (do ((action-prefix #\& (read-char)) (status 'init)
             (vals '()) (keyword #f))
           ((eq? status 'stop) (consolidate-duplicates result))
           
           (let
             ((token (read-primitive-token)))

               ; If #\% left on stream, read it and the following
               ; two characters (hex digits), unquote the char and
               ; append it to the rest of the token
             (do () ((not (eq? (peek-char) #\%)))
               (read-char)    ; read the percent char
               (let ((quoted-char-str (make-string 2)))
                 (string-set! quoted-char-str 0 (read-char))
                 (string-set! quoted-char-str 1 (read-char))
                 (let ((quoted-char (string->number quoted-char-str 16)))
                   (set! token
                     (string-append token 
                       (if quoted-char (string (integer->char quoted-char))
                         "*INVALID-%-SEQ*")
                       (read-primitive-token))))))
             
             (if (eof-object? action-prefix)
               (set! action-prefix '*eof*))
             (set! status
               (case action-prefix 
                 ((#\& *eof* #\/)    ; new parmset to follow
                   (case status
                     ((init) #t)
                     ((have-read-keyword)  ; parm without any values
                       (set! result (cons (list keyword) result)))
                     ((have-read-value)
                       (set! result (cons (cons keyword (reverse vals)) result)))
                     (else (error "unexpected status " status)))
                   (set! keyword (string->symbol token))
                   (if (eq? action-prefix '*eof*) 'stop 'have-read-keyword))
                 ((#\=)
                   (case status
                     ((have-read-keyword)  ; the first value after the keyword
                       (set! vals (list token))
                       'have-read-value)
                     ((have-read-value)
                       (error "= unexpected after the first value"))
                     (else (error "unexpected status " status))))
                 ((#\+)
                   (case status
                     ((have-read-keyword)
                       (error "+ unexpected after a keyword"))
                     ((have-read-value)    ; other values after the keyword
                       (if (equal? (car vals) "")
                         (set-car! vals token)  ; if the previous token was empty, ditch it
                         (set! vals (cons token vals)))
                         'have-read-value)
                     (else (error "unexpected status " status))))
                 (else (error "unexpected action-prefix " action-prefix))))))))))
))

(@ "|query-parameter-lookup| works the same way that |CGI:lookup| works, 
but it accepts the parameters as an argument, and does not have the predefined 
values.

\\medskip\\verbatim
(query-parameter-lookup parameters name type [default])
|endverbatim"

(@c
(define (query-parameter-lookup query-parms name type . default-value-l)
  (define (coerce type name . vals)
    (case type
      ((token)
         (if (null? vals) #f (car vals)))
      ((tokens) vals)
      ((string)
       (cond
        ((null? vals) "")
        ((null? (cdr vals)) (car vals))
        (else
          (apply string-append (list-intersperse vals " ")))))
      ((io)
        (cond
          ((null? vals) '(""))
          ((null? (cdr vals)) vals)
          (else (list-intersperse vals " "))))
      ((int number)
        (let* (
            (val-raw (and (pair? vals) (null? (cdr vals)) (car vals)))
            (val-c 
              (if (eq? type 'int)
                 (string->integer val-raw 0 (string-length val-raw))
                 (string->number val-raw))))
           (or val-c
             (abort
            (make-property-condition 'cgi-type-error
               'name name
               'type type)))))
      (else
         (error "invalid type: " type))))
  (let ((found-assoc (assq name query-parms)))
    (cond
      ((and found-assoc (not (equal? '("") (cdr found-assoc))))
        (apply coerce (cons type found-assoc)))
      ((null? default-value-l)
        (error "CGI name " name
          " was not found among form parameters: " query-parms))
      (else (car default-value-l)))))
))

)