;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities for Sending mail from Scheme
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

(define (send-email to from cc bcc subject body variables)
  (let-values ([(op ip ep pid) 
                (run-mail-program to from cc bcc subject)])
    (put-string op
      (compose-with-variables body variables))
    (close-port op)
    (printf "~a\n"
      (get-string-all ip))))
      
(define (run-mail-program to from cc bcc subject)
  (open-process-ports
    (process-string to from cc bcc subject)
    (buffer-mode block)
    (native-transcoder)))

(define (process-string to from cc bcc subject)
  (format
    "mail ~? ~? ~? ~? ~?"
    "-s '~a'" (list subject)
    "~@[-c '~{~a~^,~}'~]" (list (and (pair? cc) cc))
    "~@[-b '~{~a~^,~}'~]" (list (and (pair? bcc) bcc))
    "-r '~a'" (list from)
    "~{'~a'~^ ~}" (list to)))

(define (compose-with-variables body variables)
  (define (lookup x)
    (let ([res (assq x variables)])
      (if res (cdr res) "")))
  (fold-left
    (lambda (s x)
      (cond
        [(string? x) (string-append s x)]
        [(symbol? x) (string-append s (lookup x))]
        [else (errorf 'send-email "invalid body element ~s" x)]))
    ""
    body))
