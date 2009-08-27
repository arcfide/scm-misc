;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web Utilities
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

#!chezscheme
(library (arcfide descot web utilities)
  (export xhtml-decl html head body title div-elem h1 h2 h3 p input button ul ol li a
   form link make-column-table-with-size categories->links 
   matching-libraries capitalize-string
   make-search-proc)
	(import
		(rnrs base)
		(rnrs mutable-strings)
		(rnrs unicode)
		(only (scheme) make-list)
		(foof irregex)
		(riastradh foof-loop)
		(only (srfi :13) string-tokenize)
		(srfi :14)
		(arcfide descot rdf utilities)
		(arcfide descot web parameters))

(define xhtml-decl
"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
    \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">
")

(define html
  (lambda (head body)
    `(html (@ (xmlns "http://www.w3.org/1999/xhtml")
	      (xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance")
	      (xsi:schemaLocation "http://www.w3.org/MarkUp/SCHEMA/xhtml11.xsd")
	      (xml:lang "en"))
       ,head
       ,body)))

(define head (lambda elems (cons 'head elems)))
(define body (lambda elems (cons 'body elems)))
(define title (lambda (text) `(title ,text)))
(define div-elem (lambda body `(div ,@body)))
(define h1 (lambda body `(h1 ,@body)))
(define h2 (lambda body `(h2 ,@body)))
(define h3 (lambda body `(h3 ,@body)))
(define p (lambda body `(p ,@body)))
(define input (lambda attrs `(input (@ ,@attrs))))
(define button (lambda body `(button ,@body)))
(define ul (lambda body `(ul ,@body)))
(define ol (lambda body `(old ,@body)))
(define li (lambda body `(li ,@body)))
(define a (lambda body `(a ,@body)))

(define form 
  (lambda (action method . body) 
    `(form (@ (action ,action) (method ,method)) ,@body)))

(define link
  (lambda (href rel type media)
    `(link (@ (href ,href) (rel ,rel) (type ,type) (media ,media)))))

(define map-list->table
  (lambda (heads tails)
    (loop ([for tail (in-list tails)]
           [with heads heads (if (null? heads) heads (cdr heads))]
           [for table 
             (listing 
               (if (null? heads) tail (cons (list 'td (car heads)) tail)))])
      => (values table heads))))

(define make-column-table-with-size
  (lambda (size lst)
    (let ([step (exact (ceiling (/ (length lst) size)))])
      (loop continue ([with table (make-list step '())] [with rest lst]
                      [until (null? rest)])
        => (cons 'table (map (lambda (e) (cons 'tr (reverse e))) table))
        (let-values ([(new-table new-rest) (map-list->table rest table)])
          (continue new-table new-rest))))))

(define categories->links
  (lambda (cats)
    (collect-list (for cat (in-list cats))
      `(a (@ (href ,(string-append descot-browse-path "?cat=" cat))) 
	 ,(capitalize-string cat)))))

(define capitalize-string
  (lambda (s)
    (let ([ns (string-copy s)])
      (string-set! ns 0 (char-upcase (string-ref ns 0)))
      ns)))
(define make-search-proc
  (lambda (query)
    (let ([terms (map (lambda (e) (irregex e 'i)) 
		   (string-tokenize query 
		     (char-set-difference char-set:graphic (char-set #\+))))])
      (lambda objects
	(collect-and (for term (in-list terms))
	  (collect-or (for object (in-list objects))
	    (irregex-search term (rdf-node->string object))))))))

(define matching-libraries
  (lambda (match? library-names)
    (collect-list (for name (in-list library-names))
		  (if (matching-library? match? name))
      name)))

(define matching-library?
  (lambda (match? name)
    (let ([db (make-filled-rdf-predicate-map (current-store) name)])
      (let ([title (library-title db)]
	    [desc (library-description db)]
	    [names (library-names (current-store) db)])
	(and title desc (apply match? `(,title ,desc ,@names)) #t)))))

)
