;;; -*- Mode: scheme -*-

;;;; Debuggable Lambda for Petite
;;;; A lambda that keeps information around for debugging purposes

;;; Copyright (c) 2011 Aaron W. Hsu <arcfide@sacrideo.us>
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

(define-condition-type &lambda &error
  lambda-error lambda-error?
  (formals lambda-error-formals)
  (arguments lambda-error-arguments)
  (body lambda-error-body))

(define-syntax (lambda/petite x)
  (syntax-case x ()
    [(_ formals b1 b2 ...)
     (let ()
       (define (valid-formals? x)
         (syntax-case x ()
           [(x . y) (and (identifier? #'x) (valid-formals? #'y))]
           [() #t]
           [x (identifier? #'x)]))
       (unless (valid-formals? #'formals)
         (error #f "Invalid formals"
           (syntax->datum #'formals)))
       #'(let ([f (lambda formals b1 b2 ...)])
           (lambda args
             (guard
               (c
                 [(serious-condition? c)
                  (raise
                    (condition c
                      (lambda-error
                        #'formals
                        args
                        #'(b1 b2 ...))))])
               (apply f args)))))]))