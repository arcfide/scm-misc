#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Error Number FFI Handling}
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

(@l "This library provides a Chez Scheme specific means for getting the
current errno value.  It also provides some safety conveniences for
doing so, to ensure that the |errno| value  does not accidently change
in the midst of the call.

This is a Chez Scheme specific library, and it is expected that other systems should have 
their own way of accomplishing this."

(arcfide errno)
(export errno errno-message call-with-errno)
(import (chezscheme))

(@* "Implementation"
"To make this work, I use some rather dangerous Chez Scheme hackery, so
you should  beware!"

(@c
(define errno
  (let ([errno-ent (#%$foreign-entry "errno")])
    (lambda ()
      (foreign-ref 'int errno-ent 0))))

(define errno-message
  (let ([$strerror (foreign-procedure "strerror" (fixnum) string)])
    (lambda (num) ($strerror num))))

(define (call-with-errno thunk receiver)
  (call-with-values
    (lambda () (critical-section (let ([v (thunk)]) (values v (errno)))))
    receiver))
))

)
