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
resulting value. We can look at this as a consumer/producer problem
where there is one producer and one consumer. If this is the case,
then we set up the necessary mutexes, conditions, and flags, and then
enqueue our producer to be run by the pipeline manager. The consumer
is activated when someone uses the |id| that was created."

(@c
(define-syntax with-enqueued
  (syntax-rules ()
    [(_ (id exp) b1 b2 ...)
     (let ([result 'dummy] 
           [done? #f]
           [c (make-condition)] 
           [m (make-mutex)])
       (@< |Define syntactic value consumer| id m done? result c)
       (enqueue
         (@< |Construct value thread producer| result exp m c done?))
       b1 b2 ...)]))))

(@ "Each time we launch or enqueue an object, we want to make sure that
we record that launching or forking in a counter so that the thread
manager does not launch more than we want. For this, we define a
set of bindings to manage the thread counter concurrently. Generally,
a thread that is active should call |start-thread| at the beginning
of its code, and should call |end-thread| at the end. If somewhere
in the middle the thread will be deactivating itself and waiting
for something that won't consume CPU resources, then it must
|end-thread| until it begins consuming CPU resources again, at which
point it should call |start-thread| again."

(@c
(define active-thread-count 0)
(define active-thread-count-condition (make-condition))
(define active-thread-count-mutex (make-mutex))
(define (start-thread)
  (with-mutex active-thread-count-mutex
    (set! active-thread-count (1+ active-thread-count))))
(define (end-thread)
  (with-mutex active-thread-count-mutex
    (set! active-thread-count (-1+ active-thread-count))
    (condition-signal active-thread-count-condition)))))

(@ "The value thread producer should, when run, after having been
queued, activate itself, compute its value and store it, signalling
the consumer to receive it if the consumer has already run, and
finally end."

(@> |Construct value thread producer| (capture result exp m c done?)
(lambda ()
  (start-thread)
  (set! result exp)
  (with-mutex m
    (set! done? #t)
    (condition-broadcast c))
  (end-thread))))

(@ "The value thread consumer is a classic consumer. The identifier
syntax will give us the result if it is already computed, and otherwise
wait for the result to be given to us. Note that these are designed to
be called within other threads, and do not themselves represent a 
separate thread of computation. This means that the thread should
already be active or started when it calls in here. We should take
care to end the thread and start it back up again at the appropriate
points."

(@> |Define syntactic value consumer| 
    (export id) 
    (capture id m done? result c)
(define-syntax id
  (identifier-syntax
    (with-mutex m
      (let try-again ()
        (if done?
            result
            (begin
              (end-thread)
              (condition-wait c m)
              (start-thread)
              (try-again)))))))))

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

(@ "The queue is composed of a head and tail, as well as a mutex for
controlling access, and a condition for signalling new entries. This
model is a simplified version of the bounded queue, because we do not
have to worry about the bounding here; we allow as many queued
jobs as we want."

(@c
(define queue '())
(define queue-tail #f)
(define queue-mutex (make-mutex))
(define queue-condition (make-condition))))

(@ "Enqueueing is, essentially, our producer in this problem. It
will always be able to add a new element in though, so we only need
to ensure that we have the mutex before changing the queue to ensure
our thread safety."

(@c
(define (enqueue thunk)
  (with-mutex queue-mutex
    (if queue-tail
        (set-cdr! queue-tail (cons thunk (cdr queue-tail)))
        (begin
          (set! queue (cons thunk queue))
          (set! queue-tail queue)))
    (condition-signal queue-condition)))))

(@ "Our thread manager is our consumer of the thread. It follows the
usual consumer model, with the exception that it does not spawn a new
thread if there are too many spawned threads already."

(@> |Pipeline Manager|
(fork-thread
  (lambda ()
    (with-mutex active-thread-count-mutex
      (let continue ()
        (if (< active-thread-count (pipeline-size))
            (begin
              (mutex-release active-thread-count-mutex)
              (fork-thread (@< |Get next thread|))
              (mutex-acquire active-thread-count-mutex))
            (condition-wait 
              active-thread-count-condition
              active-thread-count-mutex))
        (continue)))))))

(@ "While the thread manager is our high-level consumer, the actual
consumer in the traditional sense of conurrency problems occurs when 
we try to get the next thread, which is where we really alter the
shared queue resource. In the above, our shared resource is really
the active thread counter, until we know that it is safe to dequeue
some particular thread. Once we know that it is safe, though, we can
safely start the actual consumer process below."

(@> |Get next thread| 
(with-mutex queue-mutex
  (let try-again ()
    (if (null? queue)
        (begin
          (condition-wait queue-condition queue-mutex)
          (try-again))
        (let ([res (car queue)])
          (when (eq? queue queue-tail)
            (set! queue-tail #f))
          (set! queue (cdr queue))
          res))))))

(@ "At the end of all of our definitions, we want to start the 
pipeline manager."

(@c (@< |Pipeline Manager|)))

)
