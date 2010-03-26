(@chezweb)

"\\centerline{\\titlef Concurrent Scheme Programming}
\\vskip 18pt
\\centerline{Aaron W. Hsu}
\\vskip 24pt
\\centerline{Version 0.1}
\\vskip 18pt
\\centerline{\\bf License}
\\medskip
\\noindent
Copyright $\\copyright$ 2010 Aaron W. Hsu {\\tt arcfide@sacrideo.us}
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
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.\\par"

(@l ""

(arcfide pipelines)
(export pipeline enqueue with-enqueued pipeline-size =>)
(import (chezscheme))

(@* "Multi-core computing"
"Most computer today have multiple cores for programming. It make
sense to try to exploit these in some fashion or another. This is
not always easy. One has to be careful not to overload the cores with
too many threads, or the program will not benefit from optimal
use of the threads. Additionally, it's hard to organize your program in
a multi-core friendly way. This library doesn't hope to solve these
problems, but it does hope to provide a basic set of primitives for
more easily managing them.

The basic idea is to assist the programming in pipelining his program.
While most programs that I have seen are serial, it is sometimes
possible to identify places where you could pipeline your code. If
these places do exist, then it would be useful to have a way to
pipeline these pieces.

As an example, suppose that you need to process a number of files, and
while loading each of these files is pretty easy, they may be cpu
intensive when being processed. You might like to take advantage of
your multiple cores. However, if there are 50 such files, you don't
want to spawn a new thread for each of them an wait for them all to
complete. Rather, it would be better if you could spawn just three
or four threads at a time, and let your cores take care of those as
they can, so that you don't overload the cores.

\\medskip
\\verbatim
(define (process-files . fns)
  (cond
    [(pair? fns)
     (pipeline
       (=> head (intense (car fns)))
       (=> tail (process-files (cdr fns)))
       (cons head tail))]
    [else '()]))
|endverbatim
\\medskip

The above example demonstrates the idea. Simply put, every expression 
in |pipeline| will be queued for concurrent execution. Additionally,
each |(=> id exp)| form will be wrapped so that the return value of
|exp| will be bound to |id| when the thread is complete. The final
expression should not be such a binding expression."

(@c
(define-syntax pipeline
  (syntax-rules (=>)
    [(_ exp)
     (with-enqueued (result exp) result)]
    [(_ (=> id exp) rest ...)
     (with-enqueued (id exp) (pipeline rest ...))]
    [(_ exp rest ...)
     (begin (enqueue exp) (pipeline rest ...))]))))

(@ "The purpose of |with-enqueued| is to enqueue a particular 
expression in preparation to obtain its value. The bound |id| is
bound to a syntax that expands into code that waits to receive the
resulting value."

(@c
(define-syntax with-enqueued
  (syntax-rules ()
    [(_ (id exp) b1 b2 ...)
     (let ([result 'dummy] 
           [done? #f]
           [c (make-condition)] 
           [m (make-mutex)])
       (@< |Define thread thunk| result exp m c done?)
       (@< |Define id| id m done? result c)
       (define thread-object (enqueue thread-thunk))
       b1 b2 ...)]))))

(@ "The thread thunk should |set!| |result| to the result of evaluating
|exp|, and then it should grab the mutex and indicate that it is 
finished. We grab the mutex to make sure that we have exclusive access
to |done?|, since otherwise we could encounter some race conditions.
We also use |broadcast| instead of |signal| in the rare case where
a condition may be used multiple times. I don't think this should be
possible here, but this is a just in case measure."

(@> |Define thread thunk| () (thread-thunk) (result exp m c done?)
(define (thread-thunk)
  (set! result exp)
  (with-mutex m
    (set! done? #t)
    (condition-broadcast c))
  (with-mutex queue-mutex
    (queue-dec)
    (condition-signal queue-condition)))))

(@ "The actual |id| is an identifier syntax that also acquires the
exclusive access to |done?| whereupon it checks to see if the thread
has completed before us, and if not, it waits for the thread to complete
and tell us about it."

(@> |Define id| () (id) (id m done? result c)
(define-syntax id
  (identifier-syntax
    (with-mutex m
      (if done?
          result
          (begin 
            (with-mutex queue-mutex
              (queue-dec)
              (condition-signal queue-condition))
            (condition-wait c m)
            (with-mutex queue-mutex (queue-inc))
            result)))))))

(@* "The Thread Queue"
"To accomplish the parameterized control over how many threads 
actually get sent through the pipeline, we use a thread queue,
based on a FIFO approach. We use a thread manager thread that starts
automatically to manage the control of the threads.

Firstly, however, the thread queue only lets |(pipeline-size)| threads
through at a time, not counting the manager thread. This is a parameter
that takes a positive exact integer."

(@c
(define pipeline-size
  (make-parameter 4
    (lambda (x)
      (assert (integer? x))
      (assert (exact? x))
      (assert (positive? x))
      x)))))

(@ "We can only add new threads to the queue, and this is done with
|enqueue|. It takes a thread thunk has an unspecified return value."

(@c
(define (enqueue thunk)
  (with-mutex queue-mutex
    (queue-add! thunk)
    (condition-signal queue-condition)))))

(@ "The thread manager is actually just a loop that checks the
queue, and starts a thread if it can."

(@c
(define (start-pipeline-manager)
  (fork-thread
    (lambda ()
      (define (launch-next)
        (fork-thread (queue-next))
        (queue-inc))
      (let loop ()
        (with-mutex queue-mutex
          (if (and (< queue-running (pipeline-size))
                   (not (queue-empty?)))
              (launch-next)
              (condition-wait queue-condition queue-mutex)))
        (loop)))))))

(@ "The queue itself is basically implemented as a condition, a mutex,
a queue list, and the procedures |queue-add!|, |queue-next|, and
|queue-empty?|. Queue-running is a global variable that keeps
track of how many threads are currently running.

Let's start with the trivial definitions."

(@c
(define queue-running 0)
(define (queue-inc) (set! queue-running (1+ queue-running)))
(define (queue-dec) (set! queue-running (-1+ queue-running)))
(define queue-mutex (make-mutex))
(define queue-condition (make-condition))
(define queue '())
(define queue-tail #f)
(define (queue-empty?) (null? queue))))

(@ "Now, grabbing the next element from the queue just means grabbing
the |car|, and keeping track of the tail."

(@c
(define (queue-next)
  (let ([res (car queue)])
    (set! queue (cdr queue))
    (when (queue-empty?) (set! queue-tail #f))
    res))))

(@ "Adding means that we have to update the tail only, unless we are
adding an initial element."

(@c
(define (queue-add! x)
  (if (queue-empty?)
      (begin
        (set! queue (cons x queue))
        (set! queue-tail queue))
      (set-cdr! queue-tail (cons x (cdr queue-tail)))))))

(@ "Whenever the library is imported, it's pretty important to get the
pipeline manager started right away."

(@c
(start-pipeline-manager)))

)
