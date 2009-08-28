;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Descot Web Parameters
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

(library (arcfide descot web parameters)
  (export descot-path-prefix descot-static-prefix 
   descot-about-path descot-blog-path descot-browse-path
   descot-search-path descot-beta-msg descot-title descot-stylesheet
   descot-store-fname current-store browser-column-width
   descot-web-docroot descot-rdf-path descot-mirror-path
   descot-submit-path descot-submit-root
   descot-store-root
   descot-maintainer-email descot-maintainer-name)
  (import 
		(rnrs base) 
		(only (scheme) make-parameter)
		(riastradh schemantic-web rdf-list-graph))

(define descot-path-prefix "/descot")
(define descot-static-prefix descot-path-prefix)
(define descot-about-path (string-append descot-path-prefix "/about.xhtml"))
(define descot-blog-path (string-append descot-path-prefix "/blog.xhtml"))
(define descot-browse-path (string-append descot-path-prefix "/db/browse"))
(define descot-search-path (string-append descot-path-prefix "/db/search"))
(define descot-beta-msg "ALPHA!")
(define descot-title "Descot: Decentralized Scheme Code Tracker")
(define descot-store-fname "regress/sample.srdf.local")
(define descot-stylesheet (string-append descot-static-prefix "/style.css"))
(define descot-web-docroot "~/descot/www_static")
(define descot-rdf-path (string-append descot-path-prefix "/rdf"))
(define descot-mirror-path (string-append descot-path-prefix "/rdf-mirror"))
(define descot-submit-path (string-append descot-path-prefix "/db/submit"))
(define descot-submit-root "~/code/arcfide/descot/regress/submissions/")
(define descot-store-root "~/code/arcfide/descot/regress/store")
(define descot-maintainer-email "arcfide@sacrideo.us")
(define descot-maintainer-name "Aaron W. Hsu")

(define current-store
  (make-parameter (make-rdf-graph)
    (lambda (e) (and (rdf-graph? e) e))))

(define browser-column-width
  (make-parameter 4
    (lambda (e) (and (integer? e) e))))

)
