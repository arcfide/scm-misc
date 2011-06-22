#!chezscheme
(@chezweb)

"\\centerline{\\titlef Parameterize Modules}
\\bigskip
\\centerline{Aaron W. Hsu {\\tt <awhsu@indiana.edu>}}
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

(@l 
"Chez Scheme's module system is largely syntactic. This permits some
interesting extensions  of the standard module form in order to achieve
some of the features that other Scheme's  have built in to their module
systems. Chez's use of syntactic modules permits a wide  range of
extensions that may not be possible for module systems that do not exist
as a  part of the language itself, or are not malleable. 

One such commnonly desired feature is the ability to parameterize parts
of the module  form so that you can declare the same module with
different internals. This library provides a simple method for doing
this. It introduces a form |module/parameterized| to permit altering a
set of expressions in the module form when the module is
declared/imported."

(arcfide parameterized-modules)
(export module/parameterize)
(import (chezscheme))

(@* "Usage"
"The |module/paramterize| form has the following syntax:

\\medskip
\\verbatim
(module/parameterize (name p ...) (export ...) body+ ...)
|endverbatim
\\medskip

\\noindent
The |export| and |body| forms above correspond to the export and 
body clauses of the normal |module| syntax in Chez Scheme. 
The above form defines |name|, which has the following syntax:

\\medskip\\verbatim
(name module-name exp ...)
|endverbatim
\\medskip

\\noindent
The |name| form has the same effect as introducing a module form 
in the same scope as that of |name| with the given |export| 
and |body+| given when |name| was defined, with each |p|
being bound in |body+| to the value defined by the |exp ...| form 
or forms. Here is an example:

\\medskip
\\verbatim
(module/parameterize (instantiate x y) (a b)
  (define a x)
  (define b y))
(let ()
  (instantiate m
    (define x 3)
    (define y 4))
  (import m)
  (list a b))
; => '(3 4)
|endverbatim
\\medskip")

(@* "Expansion"
"Using the same names from the above, an use of |name| will expand
roughly into a form somewhat like the following:

\\medskip
\\verbatim
(module module-name (export ...)
  (module (p ...) exp ...)
  body+ ...)
|endverbatim

\\noindent
We must be careful to wrap the |p| bindings appropriately so that
they capture what is visible in |exp|.")

(@* "Implementation"
"The main definition of |module/parameterize| has a simple start:"

(@> |Define module/parameterize| (export module/parameterize)
(trace-define-syntax (module/parameterize x)
  (syntax-case x ()
    [(_ (name p ...) (e ...) b1 b2 ...)
     (@< |Generate module instantiator| name p e b1 b2)]))
))

(@
"Each |module/parameterize| will bind |name| to a module instantiator 
that is used to instantiate the final, fully defined module form.
The main key here is to create a new set of parameter bindings 
|np| which are the same as the old bindings of |p| except that they 
have new wraps for the new scope. This works around the issues of 
only having one or the other, and walking the code to change either
the old wraps to the new ones in the final module, or the old 
references to new wraps. In either one of these cases, if there is 
and implicitly bound identifier that is not visible in the code, 
then rewrapping only those identifiers that we can find will not 
work. Instead, it will result in the implicitly bound identifier 
not being seen by one or other body of code. To avoid this, 
we will maintain both and link them explicitly."   

(@> |Generate module instantiator| (capture name p e b1 b2)
#'(trace-define-syntax (name x)
    (syntax-case x ()
      [(_ mn exp (... ...))
       (with-syntax ([(np (... ...)) (with-implicit (mn p ...) #'(p ...))])
         (with-implicit (mn e ...)
	   #'(module mn (e ...)
	       (@< |Link old and new identifiers| 
	           new-ids old-ids (p ...) (np (... ...)) (exp (... ...)))
	       (import new-ids old-ids)
	       b1 b2 ...)))]))
))

(@
"To link the old and the new bindings we need to create two modules.
The first will bind the new identifiers to their expressions given 
to the instantiator. The other will bind the old identifiers to 
identifier macros that expand into references to the new identifiers."

(@> |Link old and new identifiers| 
  (export new-ids old-ids)
  (capture new-ids old-ids (p ...) (np ...) (exp ...))
(module new-ids (np ...) exp ...)
(module old-ids (p ...) (define-syntax p (identifier-syntax np)) ...)
))

(@
"We can now move the definition of |module/parameterize| to the
top-level."

(@c
(@< |Define module/parameterize|)
))

)
