#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Building applications}
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

(@l "This |resolve-library-path| procedure takes an R6RS Library
specification and returns a string path to the library's path."

(arcfide building)
(export
  resolve-library-path
  resolve-library-path/objects)
(import
  (chezscheme)
  (riastradh foof-loop))  

(@* "Implementation"
"The output file should include the desired extension."

(@c
(define (make-resolver get)
  (lambda (lib-spec)
    (let ([lib-path (spec->path lib-spec)]
          [dirs (map get (library-directories))]
          [exts (map get (library-extensions))])
      (let loop ([dirs dirs])
        (cond
          [(not (pair? dirs)) #f]
          [(resolve-ext (car dirs) lib-path exts)]
          [else (loop (cdr dirs))])))))

(define resolve-library-path (make-resolver car))
(define resolve-library-path/objects (make-resolver cdr))

(define (resolve-ext dir lib-path exts)
  (let loop ([exts exts])
    (cond
      [(not (pair? exts)) #f]
      [(let ([total-path (compose dir lib-path (car exts))])
         (and (file-exists? total-path) total-path))]
      [else (loop (cdr exts))])))
      
(define (spec->path spec)
  (cond
    [(string? spec) spec]
    [(symbol? spec) (symbol->string spec)]
    [(pair? spec) (lib-spec->path spec)]
    [else (errorf 'spec->path "unrecognized spec ~s" spec)]))
      
(define (lib-spec->path spec)
  (let* ([dir-string (string (directory-separator))]
         [fmt-string (string-append "~{~a~^" dir-string "~}")])
    (format fmt-string spec)))
      
(define (compose dir lib-path ext)
  (let ([dir-string (string (directory-separator))])
    (string-append dir dir-string lib-path ext)))
))

)
