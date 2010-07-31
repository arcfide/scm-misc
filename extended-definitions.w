#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Extended Definitions}
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

(@l "This library defines a set of definition constructs.
This includes ways to define functions that ought to be integrated, 
as well as constant values.
[[define-auxilary-keywords]] is useful for explicitly exporting 
auxilary syntax."

(arcfide extended-definitions)
(export define-integrable define-constant define-auxilary-keywords)
(import (chezscheme))

(@* "Definitions"
"I really should split these into separate definitions. At the moment,
you'll just have to read the code."

(@c
(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (identifier? #'name)
     (begin
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #'((fluid-let-syntax ([name (identifier-syntax xname)])
                   (lambda formals form1 form2 ...))
                  arg
                  (... ...))])))
       (define xname
         (fluid-let-syntax ([name (identifier-syntax xname)])
           (lambda formals form1 form2 ...))))]))

(define-syntax define-constant
  (syntax-rules ()
    [(_ name expr)
     (define-syntax name
       (let ([x expr])
         (lambda (y)
           (syntax-case y ()
             [id (identifier? #'id) #`'#,(datum->syntax #'id x)]))))]))

(define-syntax define-auxilary-keywords
  (syntax-rules ()
    [(_ aux ...)
     (begin
       (define-syntax aux
         (lambda (x)
           (errorf #f "misplaced aux keyword ~a"
             (syntax->datum x))))
       ...)]))
))

)
