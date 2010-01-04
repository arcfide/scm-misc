;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Datum Weak Hashtables
;;; Version: 0.1
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

(define-record-type datum-weak-hashtable
  (fields db trail guardian)
  (protocol
    (lambda (n)
      (case-lambda
        [()
         (let ([g (make-guardian)])
           (collect-request-handler
             (make-handler cg g (collect-request-handler)))
           (n (make-eq-hashtable) (make-weak-eq-hashtable) g))]
        [(size)
         (let ([g (make-guardian)])
           (collect-request-handler
             (make-handler g (collect-request-handler)))
           (n (make-eq-hashtable size) 
              (make-weak-eq-hashtable size)
              g))]))))

(define (datum-weak-hashtable-set! table key value)
  (let ([db (datum-weak-hashtable-db table)]
        [trail (datum-weak-hashtable-trail table)]
        [guard (datum-weak-hashtable-guardian table)]
        [data (weak-cons value key)])
    (let ([cleaner (make-cleaner db data)])
      (with-interrupts-disabled
        (eq-hashtable-set! db key data)
        (eq-hashtable-set! trail value cleaner)
        (guard cleaner)))))
        
(define (datum-weak-hashtable-ref table key default)
  (let ([db (datum-weak-hashtable-db table)])
    (let ([res (eq-hashtable-ref db key my-false)])
      (if (or (eq? res my-false) (bwp-object? (car res)))
          default
          (car res)))))

(define (datum-weak-hashtable-contains? table key)
  (let ([db (datum-weak-hashtable-db table)])
    (with-interrupts-disabled
      (and (eq-hashtable-contains? db key)
           (not (bwp-object? (car (eq-hashtable-ref db key #f))))))))

(define (datum-weak-hashtable-update! table key proc default)
  (let ([db (datum-weak-hashtable-db table)]
        [trail (datum-weak-hashtable-trail table)]
        [guard (datum-weak-hashtable-guardian table)])
    (define (updater val)
      (with-interrupts-disabled
        (let ([data (if (or (eq? my-false val) (bwp-object? (car val)))
                        (weak-cons (proc default) key)
                        (weak-cons (proc (car val)) key))])
          (let ([cleaner (make-cleaner db data)])
            (guard cleaner)
            (eq-hashtable-set! trail (car data) cleaner)
            data))))
    (eq-hashtable-update! db key updater my-false)))
    
(define (datum-weak-hashtable-delete! table key)
  (let ([db (datum-weak-hashtable-db table)]
        [trail (datum-weak-hashtable-trail table)])
    (let ([res (datum-weak-hashtable-ref table key my-false)])
      (unless (eq? my-false res)
        (eq-hashtable-delete! trail res))
      (eq-hashtable-delete! db key))))

(define (datum-weak-hashtable-size table)
  (hashtable-size (datum-weak-hashtable-db table)))
  
(define (make-handler guard old-handler)
  (lambda ()
    (let loop ()
      (let ([cleaner (guard)])
        (when cleaner
          (cleaner)
          (loop))))
    (old-handler)))

(define (make-cleaner db data)
  (lambda ()
    (let ([key (cdr data)])
      (let ([res (eq-hashtable-ref db key #f)])
        (when (and res (eq? res data))
          (eq-hashtable-delete! db key))))))

(define my-false (gensym "datum-weak/false"))
