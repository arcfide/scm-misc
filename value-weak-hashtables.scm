;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Value Weak Hashtables
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

(define-record-type value-weak-hashtable
  (fields db trail guardian)
  (protocol
    (lambda (n)
      (case-lambda
        [(hash equiv?)
         (let ([g (make-guardian)])
           (n (make-hashtable hash equiv?) 
              (make-weak-eq-hashtable) 
              g))]
        [(hash equiv? size)
         (let ([g (make-guardian)])
           (n (make-hashtable hash equiv? size) 
              (make-weak-eq-hashtable size)
              g))]))))

(define (value-weak-hashtable-set! table key value)
  (let ([db (value-weak-hashtable-db table)]
        [trail (value-weak-hashtable-trail table)]
        [guard (value-weak-hashtable-guardian table)]
        [data (weak-cons value key)])
    (clean-value-weak-hashtable! guard)
    (let ([cleaner (make-cleaner db data)])
      (hashtable-set! db key data)
      (eq-hashtable-set! trail value cleaner)
      (guard cleaner))))
        
(define (value-weak-hashtable-ref table key default)
  (clean-value-weak-hashtable! (value-weak-hashtable-guardian table))
  (let ([db (value-weak-hashtable-db table)])
    (let ([res (hashtable-ref db key my-false)])
      (if (or (eq? res my-false) (bwp-object? (car res)))
          default
          (car res)))))

(define (value-weak-hashtable-contains? table key)
  (clean-value-weak-hashtable! (value-weak-hashtable-guardian table))
  (hashtable-contains? (value-weak-hashtable-db table) key))

(define (value-weak-hashtable-update! table key proc default)
  (let ([db (value-weak-hashtable-db table)]
        [trail (value-weak-hashtable-trail table)]
        [guard (value-weak-hashtable-guardian table)])
    (define (updater val)
      (let* ([res (if (or (eq? my-false val) (bwp-object? (car val)))
                      default
                      (car val))]
             [data (weak-cons (proc res) key)]
             [cleaner (make-cleaner db data)])
        (guard cleaner)
        (eq-hashtable-set! trail (car data) cleaner)
        data))
    (clean-value-weak-hashtable! guard)
    (hashtable-update! db key updater my-false)))
    
(define (value-weak-hashtable-delete! table key)
  (clean-value-weak-hashtable! (value-weak-hashtable-guardian table))
  (let ([db (value-weak-hashtable-db table)]
        [trail (value-weak-hashtable-trail table)])
    (let ([val (hashtable-ref db key my-false)])
      (unless (eq? my-false val) (eq-hashtable-delete! trail val)))
    (hashtable-delete! db key)))

(define (value-weak-hashtable-size table)
  (hashtable-size (value-weak-hashtable-db table)))
  
(define (clean-value-weak-hashtable! guardian)
  (let loop ()
    (let ([cleaner (guardian)])
      (when cleaner (cleaner) (loop)))))
  
(define (make-cleaner db data)
  (lambda () (let ([key (cdr data)]) (hashtable-delete! db key))))

(define my-false (cons #f #f))
