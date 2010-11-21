#! /usr/bin/env scheme-script
(import (chezscheme))

#!chezscheme
 
(define root (call-with-input-file "LIBPATH.txt" get-line))

(define shared-object-filename
  (format "shared-object-~a.ss"
    (if (pair? (command-line-arguments))
        (car (command-line-arguments))
        "linux")))
 
(define (srfi x) (format "~a/srfi/~a" root x))
(define (arcfide x) (format "~a/arcfide/~a" root x))

(define build-path "build")
 
(when (and (file-exists? build-path) 
           (not (file-directory? build-path)))
  (errorf 'build "'~a' is not a directory" build-path))

(unless (file-exists? build-path)
  (mkdir build-path))

(for-each delete-file
  (map (lambda (a) (format "~a/~a" build-path a))
    (directory-list build-path)))

(define counter
  (let ([i 0])
    (lambda ()
      (set! i (1+ i))
      i)))
 
(define (build x)
  (compile-file x
    (format "~a/~2,'0d_~a.so"
      build-path (counter) (path-root (path-last x)))))

(build shared-object-filename)
(build (arcfide "chezweb/cheztangle.ss"))
(build (arcfide "ffi-bind.w"))
(build (arcfide "errno.w"))
(build (arcfide "sockets/compat.chezscheme.sls"))
(build (srfi "39/parameters.chezscheme.sls"))
(build (srfi "39.sls"))
(build (srfi "23/error.sls"))
(build (srfi "23.sls"))
(build (srfi "9/records.sls"))
(build (srfi "9.sls"))
(build (srfi "private/let-opt.sls"))
(build (srfi "private/include.chezscheme.sls"))
(build (srfi "8/receive.sls"))

(library-directories
  (list (cons "." ".") (cons root root)))

(build (srfi "14/char-sets.sls"))
(build (srfi "14.sls"))
(build (srfi "13/strings.sls"))
(build (srfi "13.sls"))
(build (arcfide "sockets.w"))

(cd build-path)

(define files-sorted
  (list-sort
    (lambda (a b)
      (let ([an (string->number (substring a 0 2))]
            [bn (string->number (substring b 0 2))])
        (< an bn)))
    (directory-list ".")))
 
(define (copy out x)
  (let ([in (open-file-input-port x)])
    (let loop ()
      (let ([b (get-u8 in)])
        (unless (eof-object? b)
          (put-u8 out b)
          (loop))))
    (close-port in)))

(let ([out (open-file-output-port "../arcfide_sockets.so"
             (file-options no-fail))])
  (for-each (lambda (x) (copy out x)) files-sorted)
  (close-port out))
