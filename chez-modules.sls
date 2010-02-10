;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple Chez module system in R6RS
;;; Version: 1.0
;;; 
;;; Copyright (c) 2010 Aaron W. Hsu <arcfide@sacrideo.us>
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

;;; This current version lacks the import auxilaries and import-only.

(library (arcfide chez-modules)
  (export module import)
  (import (rnrs base)
          (rnrs syntax-case)
          (only (chezscheme) define-values)
          (arcfide extended-definitions))
          
;;; define-values : syntax
;;; (define-values val-list expr)
;;; val-list := (id1 id2 ...)
;;;
;;; <expr> should return the same number of values as there are
;;; identifiers in the val-list. The identifiers are bound to the
;;; corresponding values returned by <expr>.
;;;
;;; Chez Scheme provides this by default. Uncomment if you need it.
;;; 
;;; (define-syntax define-values
;;;   (lambda (x)
;;;     (define (enumerate x) (iota (length x)))
;;;     (syntax-case x ()
;;;       [(_ formal expr)
;;;        (identifier? #'formal)
;;;        #'(define formal (let-values ([formal expr]) formal))]
;;;       [(_ (id ...) expr)
;;;        (with-syntax ([(idn ...) (enumerate #'(id ...))])
;;;          #'(begin
;;;              (define t (let-values ([(id ...) expr]) (vector id ...)))
;;;              (define id (vector-ref t idn))
;;;              ...))]
;;;       [(_ (id ... . tail) expr)
;;;        (with-syntax ([(idn ... tailn) (enumerate #'(id ... tail))])
;;;          #'(begin 
;;;              (define t 
;;;                (let-values ([(id ... . tail) expr]) 
;;;                  (vector id ... tail)))
;;;              (define id (vector-ref t idn))
;;;              ...
;;;              (define tail (vector-ref t tailn))))])))

(define-auxilary-keywords export-now)

(define-syntax module
  (lambda (x)
    (define (rewrap k ids)
      (map
        (lambda (id) (datum->syntax k (syntax->datum id)))
        ids))
    (syntax-case x ()
      [(k (export ...) b1 b2 ...)
       (with-syntax ([(outer ...) (rewrap #'k #'(export ...))])
         #'(define-values (outer ...)
             (let () 
               b1 b2 ...
               (values export ...))))]
      [(_ name (export ...) b1 b2 ...)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x (export-now)
               [(k import) 
                #`(#,(datum->syntax #'k 'module)
                   (export ...)
                   b1 b2 ...)])))])))

(define-syntax import
  (syntax-rules ()
    [(_ module-name) (module-name import)]))

)
