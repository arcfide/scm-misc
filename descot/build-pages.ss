#! /usr/local/bin/petite --script
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Build Static Descot Web Pages
;;; 
;;; Copyright (c) 2009 Aaron Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(eval-when (compile) 
  (generate-inspector-information #f)
  (optimize-level 3))

(let ()
  (import scheme)
  (include "lib/syn-param.ss")
  (include "lib/srfi-8.ss")
  (include "lib/foof-loop.ss")
  (include "lib/nested-foof-loop.ss")
  (include "lib/check-arg.ss")
  (include "lib/let-opt.ss")
  (include "lib/char-utils.ss")
  (include "lib/srfi-14.ss")
  (include "lib/srfi-13.ss")
  (include "lib/srfi-23.ss")
  (include "lib/srfi-1.ss")
  (include "lib/srfi-45.ss")
  (include "lib/stream.ss")
  (include "lib/matcomb.ss")
  (include "lib/mattext.ss")
  (include "lib/perror.ss")
  (include "lib/parcomb.ss")
  (include "lib/partext.ss")
  (include "lib/uri.ss")
  (include "lib/rdf.ss")
  (include "lib/rdf-list-graph.ss")
  (include "lib/rdf-map.ss")
  (include "lib/rdf-turtle-parser.ss")
  (include "lib/myenv-chez.ss")
  (include "lib/oleg-util.ss")
  (include "lib/oleg-sxml-tree-trans.ss")
  (include "lib/oleg-sxml-to-html.ss")
  (include "rdf-util.ss")
  (include "web-param.ss")
  (include "web-util.ss")
  (include "web-gen.ss")

  (import descot-web-parameters)
  (import foof-loop)
  (import nested-foof-loop)

(meta define script-root "www")

(meta define valid-script?
  (lambda (fname)
    (let ([sl (string-length fname)])
      (string=? ".ss" (substring fname (- sl 3) sl)))))

(meta define script-pages
  (lambda (root)
    (collect-list (for fname (in-list (directory-list root)))
		  (if (valid-script? fname))
      (string-append script-root (string (directory-separator)) fname))))

(define-syntax include-script-files
  (lambda (x)
    (with-syntax ([(f1 ...) (script-pages script-root)])
      #'(begin (include f1) ...))))

(include-script-files)

(printf "Done!~%")

(exit)

)
