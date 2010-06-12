(@chezweb)

"\\centerline{
  \\titlef Foreign Value Binding Constructs}
\\bigskip
\\centerline{Aaron W. Hsu {\tt <arcfide@sacrideo.us>}}
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

(@l "This library provides binding constructs designed to generate  foreign
code and obtain results from them without forcing  the user to write
their own stub files or compiling them  outside of compiling the scheme
file."

(arcfide ffi-bind)
(export define-foreign-values)
(import (chezscheme))

(@* "Binding Foreign Values"
"We define a single syntax |define-foreign-values| that binds
identifiers to values computed by a foreign function. The basic idea is
to provide a foreign shared object, the name of a function, and its
return type, and then to use that function to get various values of that
return type bound to the identifiers that we provide. This could be used
for getting things like the sizes or values of various structures, for
example. Our syntax follows the following:
 
\\medskip\\verbatim
(define-foreign-values <shared-object> <proc-name> <return-type> 
                       <binding> ...)
|endverbatim
\\medskip

\\noindent Here |<shared-object>| is a string that will be used in a
call to |load-shared-object|.

|<proc-name>| takes two forms, one with a convention, and the other
without.
 
\\medskip\\verbatim
(<conv> <name-string>)
<name-string>
|endverbatim
\\medskip

\\noindent The |<name-string>| should be a string as passed to the
function name of |foreign-procedure|. The |<conv>| is a convention as
listed by the Chez Scheme User's Guide in |foreign-procedure|. This is
used mostly on Windows where there are various calling conventions.
 
The |<return-type>| is just a valid |foreign-procedure| return type
specifier.

Each |<binding>| should be an identifier. Each |<binding>| will be bound
to the value returned by calling the foreign procedure on the string
representation of the |<binding>|.")

(@ "Let's first get the verifier for the syntax forms out of the way.
We use this to verify our syntax later."

(@> |Verify DFV syntax| 
    (capture conv shared-object proc-name type bindings)
(and (identifier? conv)
     (memq (syntax->datum conv) '(__cdecl __stdcall __com))
     (string? (syntax->datum shared-object))
     (string? (syntax->datum proc-name))
     (identifier? type)
     (for-all identifier? bindings))
))

(@* "Resolving shared objects" 
"Since these shared objects are not going to be distributed with the
rest of the code, it's fair to assume that they won't be in the normal
shared library paths. To make it possible to load these correctly then,
I want to have a resolving procedure that can be used to find things in
the current |source-directories|. This parameter is modified when the
compiler is loading in libraries to make sure that I can load things in
relative to the library's location. We need this at the meta level since
that is where we are going to be doing the loading and importing."

(@c
(meta define (resolve name)
  (let loop ([dirs (source-directories)])
    (cond
      [(not (pair? dirs)) name]
      [(let ([path (format "~a~a~a" (car dirs) (directory-separator) name)])
         (and (file-exists? path) path))]
      [else (loop (cdr dirs))])))
))

(@* "Define foreign values macro"
"The actual |define-foreign-values| macro is fairly simple. We need to
have access to the resolver, but in general, we want to create a special
|get-ffi-value| function that, when called with an identifier, will
return back the result of calling the foreign code. Once that's defined,
we simply call it on each of the bindings. Since the |foreign-procedure|
code is different depending on whether we have a convention or not,
we'll define that helper here."

(@> |Define define-foreign-values|
    (export define-foreign-values)
(...
(define-syntax define-foreign-values
  (syntax-rules ()
    [(_ shared-object (conv proc-name) type binding ...)
     (@< |Verify DFV syntax|
         #'conv #'shared-object #'proc-name #'type #'(binding ...))
     (begin 
       (meta define %get-ffi-value
         (begin 
           (load-shared-object (resolve shared-object))
           (foreign-procedure conv proc-name (string) type)))
       (@< |Define get-ffi-value| %get-ffi-value)
       (define-bindings get-ffi-value binding ...))]
    [(_ shared-object proc-name type binding ...)
     (@< |Verify DFV syntax|
         #'__cdecl #'shared-object #'proc-name #'type #'(binding ...))
     (begin 
       (meta define %get-ffi-value
         (begin 
           (load-shared-object (resolve shared-object))
           (foreign-procedure proc-name (string) type)))
       (@< |Define get-ffi-value| %get-ffi-value)
       (define-bindings get-ffi-value binding ...))]))
)))

(@ "What about |get-ffi-value|? This syntactic function needs to convert
the incoming binding into a string and pass it through to the getter. It
then needs to bind the resulting value to the original name provided."

(@> |Define get-ffi-value| 
    (export get-ffi-value) 
    (capture get)
(define-syntax (get-ffi-value x)
  (syntax-case x ()
    [(k name) (identifier? #'name)
     #`'#,(datum->syntax #'k 
            (get (symbol->string (syntax->datum #'name))))]))
))

(@ "Finally, we need a definition for |define-bindings|."

(@c
(define-syntax define-bindings
  (syntax-rules ()
    [(_ get) (begin)]
    [(_ get binding) (define binding (get binding))]
    [(_ get binding rest ...)
     (begin (define binding (get binding))
       (define-bindings get rest ...))]))
))

(@ "Let's make sure we can get to |define-foreign-values| from the
top-level."

(@c (@< |Define define-foreign-values|)))
)
