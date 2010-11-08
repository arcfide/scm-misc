#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Pure Scheme PostgreSQL Driver}
\\bigskip
\\centerline{Aaron W. Hsu {\\tt <arcfide@sacrideo.us>}}
\\medskip
\\centerline{\\today}\\par
\\bigskip\\rendertoc\\par
\\vfill
\\noindent
Copyright $\\copyright$ 2010 Aaron W. Hsu {\\tt <arcfide@sacrideo.us>}
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
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.\\par\\break
"
#|
(@l "This library defines a pure Scheme PostgreSQL driver supporting 
the PostgreSQL Message protocol version 3.0. This protocol is supported 
by all versions of PostgreSQL 7.4 and later. It is a pure Scheme driver, 
meaning that it does not depend or interface with any foreign libraries 
for its operation. It is written in a fairly Schemely style and will 
hopefully support the full range of the protocol."

(arcfide postgresql)
(export ) |#
(import 
  (chezscheme)
  (arcfide sockets)
  (arcfide extended-definitions)
  (arcfide sockets socket-ports)
  (srfi :64))

(@* "Roadmap"
"Currently, the following stages are planned in the development of this 
library.

\\orderedlist
\\li Simple Startup protocol and handshake support
\\li Connection Termination
\\li Simple query support
\\li Request Cancellation
\\li Extended Query support
\\li Copy operations
\\li Streaming Replication
\\endorderedlist

\\noindent
Importantly, note that I do not intend to support the deprecated 
function call operations that are supported by the protocol. As 
documented in the PostgreSQL manual, new code should not use these 
operations, and should instead use the aggregate function syntax 
of SQL.")

(@* "Reading and writing utilities"
"A lot of integers are being read and put onto ports in this
interface. R6RS does not define anything that is really concise to do
this, and we know some more stuff that lets us be a little more
precise anyways. Here are some getters and putters for binary output
and input ports that are used below."
 
(@c
(define (get-s32 port)
  (let ([bv (get-bytevector-n port 4)])
    (bytevector-s32-ref bv 0 (endianness big))))
(define (put-s32 port n)
  (let ([bv (make-bytevector 4)])
    (bytevector-s32-set! bv 0 n (endianness big))
    (put-bytevector port bv)))
(define (get-s16 port)
  (let ([bv (get-bytevector-n port 2)])
    (bytevector-s16-ref bv 0 (endianness big))))
(define (put-s16 port n)
  (let ([bv (make-bytevector 2)])
    (bytevector-s16-set! bv 0 n (endianness big))
    (put-bytevector port bv)))
(define (get-pg-string port)
  (do ([res '() (cons (get-u8 port) res)])
      [(zero? (lookahead-u8 port))
       (get-u8 port)
       (bytevector->string
         (u8-list->bytevector (reverse res))
         (native-transcoder))]))
))
 
(@* "Message types"
"Each message type goes under the primary type of
|postgresql-message|. The |postgresql-message| is never used directly
but instead serves as the top level parent for the three primary 
postgresql message types: frontend, backend, and dual messages."
 
(@c
(define-record-type postgresql-message)
))
 
(@ "When we have message types that are backend and frontend messages,
it is nice to distinguish them. Backend messages don't do any writing,
and automatically register the readers that they get with the generic
|get-postgresql-message|'s |postgresql-message-readers| assocation
list parameter."
 
(@c
(define-record-type postgresql-backend-message
  (parent postgresql-message)
  (protocol
    (lambda (p) (lambda () ((p))))))
))
 
(@ "Front end messages don't do any reading. Instead, they define
writers and store extra information such as the type and length. 
A writer takes a port puts the message to the 
binary output port."
 
(@c
(define-record-type postgresql-frontend-message
  (parent postgresql-message)
  (fields type length writer)
  (protocol 
    (lambda (p)
      (lambda (type length writer)
        ((p) type length 
             (make-postgresql-message-writer type length writer))))))
))
 
(@ "The writing of messagees is done with |put-postgresql-message|,
which takes a binary output port and a message and writes out the
messages to the port in the appropriate protocol format. It uses the
writers defined in the message itself to know how to layer things.
This works because we nest the responsibilities of the write through
each definition of the message and their constructions."
 
(@c
(define (put-postgresql-message port message)
  (cond
    [(postgresql-frontend-message? message)
     ((postgresql-frontend-message-writer message) port)]
    [(postgresql-frontend/backend-message? message)
     ((postgresql-frontend/backend-message-writer message) port)]
    [else
      (errorf 'put-postgresql-message 
        "~s is not either a frontend or dual PostgreSQL message"
        message)]))
(define (make-postgresql-message-writer type length writer)
  (lambda (port)
    (put-u8 port (char->integer type))
    (put-s32 port length)
    (writer port)
    (flush-output-port port)))
))
 
(@ "The dual PostgreSQL message have the abilities of both reading and
writing messages. They are just a composition of the above."
 
(@c
(define-record-type postgresql-frontend/backend-message
  (parent postgresql-message)
  (fields type length writer)
  (protocol
    (lambda (p)
      (lambda (type length write)
        ((p) type length
             (make-postgresql-message-writer type length write))))))
))

(@* "Authentication Messages"
"The authentication records are a little weird, and they have the
most involved hierarchy, so we'll deal with them first. They are also
the first listed in the manual, so there. 
 
Each Authentication packet is sent by the backend and uses the `R'
character as its type. However, each varies in length and fields.
Each authentication message has its own type, which indicates the type
of the authentication message.
"
 
(@c
(define-record-type authentication-message
  (parent postgresql-backend-message)
  (fields)
  (protocol
    (lambda (p)
      (lambda ()
        ((p))))))
))

(@ "{\\it Authentication Okay.} These packets are eight bytes and have
a zero as their type. They have not additional fields."
 
(@c
(define-record-type authentication-okay-message
  (parent authentication-message)
  (protocol (lambda (p) (lambda () ((p))))))
))
 
(@ "{\\it Authentication Kerveros V5.}
These packets do not have any additional field and use 2 as their
authentication type."
 
(@c
(define-record-type authentication-kerberos-v5-message
  (parent authentication-message)
  (protocol (lambda (p) (lambda () ((p))))))
))
 
(@ "{\\it Authentication Cleartext Password.}
These messages have no additional fields and use 3 as their
authentication type."
 
(@c
(define-record-type authentication-cleartext-password-message
  (parent authentication-message)
  (protocol (lambda (p) (lambda () ((p))))))
))

(@ "{\\it Authentication MD5 password.}
These messages have one additional four byte field for the salt and
have 5 as their authentication type. "
 
(@c
(define-record-type authentication-md5-password-message
  (parent authentication-message)
  (fields salt)
  (protocol
    (lambda (p)
      (lambda (salt)
        (assert (bytevector? salt))
        (assert (= 4 (bytevector-length salt)))
        ((p) salt)))))
))
 
(@ "{\\it Authentication SCM Credential.}
These messages do not define any new fields, and use 6 as their
authentication type."
 
(@c
(define-record-type authentication-scm-credential-message
  (parent authentication-message)
  (protocol (lambda (p) (lambda () ((p))))))
))
 
(@ "{\\it Authentication GSS.} 
This message defines no additional fields and uses 7 as the
authentication type."
 
(@c
(define-record-type authentication-gss-message
  (parent authentication-message)
  (protocol (lambda (p) (lambda () ((p))))))
))
 
(@ "{\\it Authentication SSPI.}
This message does not define additional fields and uses 9 as its
authentication type."
 
(@c
(define-record-type authentication-sspi-message
  (parent authentication-message)
  (protocol (lambda (p) (lambda () ((p))))))
))
 
(@ "{\\it Authentication GSS Continue.}
This message has one additional field that contains the GSS or SSPI
authentication data. It uses 8 as its authentication type."
 
(@c
(define-record-type authentication-gss-continue-message
  (parent authentication-message)
  (fields data)
  (protocol
    (lambda (p)
      (lambda (data) 
        (assert (bytevector? data))
        ((p) data)))))
))
 
(@* "Reading messages"
"We want to be able to read in messages from a given binary input
port. The idea is that we can layer the readers on top of one another
so that we only do as much work as we have to, and we can isolate out
as much code duplication as possible, without going crazy. We will
start with a main reader for all messages. This
|get-postgresql-message| will take a single binary input port and
return one PostgreSQL message."
 
(@c
(define (get-postgresql-message port)
  (let* ([type (integer->char (get-u8 port))]
         [len (get-s32 port)])
    (cond
      [(assq type (postgresql-message-readers)) 
       =>
       (lambda (res)
         ((cdr res) port (- len 4)))]
      [else
        (errorf 'get-postgresql-message
          "unknown message type ~s" type)])))
(define postgresql-message-readers
  (make-parameter '()
    (lambda (alist)
      (assert (list? alist))
      (assert
        (for-all
          (lambda (a)
            (and (pair? a)
                 (char? (car a))
                 (procedure? (cdr a))))
          alist))
      alist)))
))
 
(@ "We actually want to initialize this to the appropriate set by
default, but this has to be done at the end of a Scheme file. Let's
define a chunk that we can use to initialize the readers when we are
ready."
 
(@> |Register message readers|
(postgresql-message-readers
  `((#\R . ,get-authentication-message)
    (#\B . ,get-backend-key-data-message)
    (#\2 . ,get-bind-complete-message)
    (#\3 . ,get-close-complete-message)
    (#\C . ,get-command-complete-message)
    (#\d . ,get-copy-data-message)
    (#\c . ,get-copy-done-message)
    (#\G . ,get-copy-in-response-message)
    (#\H . ,get-copy-out-response-message)
    (#\D . ,get-data-row-message)
    (#\I . ,get-empty-query-response-message)
    (#\E . ,get-error-response-message)
    (#\n . ,get-no-data-message)
    (#\N . ,get-notice-response-message)
    (#\A . ,get-notification-response-message)
    (#\t . ,get-parameter-description-message)
    (#\S . ,get-parameter-status-message)
    ))
))
 
(@ "{\\it Reading Authentication message.}
Each authentication message has a type, and there may be specific type
actions required for the various subtypes."
 
(@c
(define (get-authentication-message port len)
  (let ([type (get-s32 port)])
    (case type
      [(0) (make-authentication-okay-message)]
      [(2) (make-authentication-kerberos-v5-message)]
      [(3) (make-authentication-cleartext-password-message)]
      [(5)
       (make-authentication-md5-password-message
         (get-bytevector-n port 4))]
      [(6) (make-authentication-scm-credential-message)]
      [(7) (make-authentication-gss-message)]
      [(9) (make-authentication-sspi-message)]
      [(8)
       (make-authentication-gss-continue-message
         (get-bytevector-n port (- len 4)))]
      [else
        (errorf 'get-authentication-message
          "unknown authentication message type ~a" 
          type)])))
)) 
 
(@* "Authentication Unit Tests"
"Let's put together some unit tests to ensure that the authentication
message handling is done right."
 
(@c
(define (run-authentication-tests)
  (test-begin "Authentication")
    (run-authentication-okay-tests)
    (run-authentication-kerberos-v5-tests)
    (run-authentication-cleartext-password-tests)
    (run-authentication-md5-password-tests)
    (run-authentication-scm-credential-tests)
    (run-authentication-gss-tests)
    (run-authentication-sspi-tests)
    (run-authentication-gss-continue-tests)
  (test-end "Authentication"))
))
 
(@ "All of the messages are going to have to be tested with some basic
things, so let's make it easy to build these tests."
 
(@c
(define (make-frontend/backend-message-tester name bv make . test-list)
  (lambda ()
    (test-begin name)
    (apply run-message-predicate-test
      (string-append name " Constructors and Predicates")
      make test-list)
    (run-message-write-test (string-append name " Writing")
      bv make)
    (apply run-message-read-test (string-append name " Reading")
      bv test-list)
    (test-end name)))
(define (make-frontend-message-tester name bv make . test-list)
  (lambda ()
    (test-begin name)
    (apply run-message-predicate-test
      (string-append name " Constructors and Predicates")
      make 
      (cons (make-pred-test (string-append name " Frontend Predicate")
              postgresql-frontend-message?)
            test-list))
    (run-message-write-test (string-append name " Writing")
      bv make)
    (test-end name)))
(define (make-backend-message-tester name bv make . test-list)
  (lambda ()
    (test-begin name)
    (apply run-message-predicate-test
      (string-append name " Constructors and Predicates")
      make 
      (cons (make-pred-test (string-append name " Backend Predicate")
              postgresql-backend-message?)
            test-list))
    (apply run-message-read-test (string-append name " Reading")
      bv test-list)
    (test-end name)))
(define (run-message-predicate-test name make . test-list)
  (for-each (lambda (test) (test (make))) 
    (cons (make-pred-test (string-append name " Message Pred")
            postgresql-message?)
          test-list)))
(define (run-message-write-test name bv make)
  (test-equal name bv
    (let-values ([(port get) (open-bytevector-output-port)])
      (put-postgresql-message port (make))
      (get))))
(define (run-message-read-test name bv . test-list)
  (test-equal (string-append name " No trailing characters")
    (eof-object)
    (let* ([port (open-bytevector-input-port bv)])
      (get-postgresql-message port)
      (get-u8 port)))
  (for-each
    (lambda (test)
      (test
        (let* ([port (open-bytevector-input-port bv)]
               [v (get-postgresql-message port)])
          (close-port port)
          v)))
    test-list))
(define (make-pred-test name pred?)
  (lambda (msg) (test-assert name (pred? msg))))
(define (make-accessor-test name get expected)
  (lambda (msg) (test-equal name expected (get msg))))
))
 
(@ "Let's start with all of the basic authentication tests."
 
(@c
(define (auth-message-test name) 
  (make-pred-test name authentication-message?))

(define run-authentication-okay-tests
  (make-backend-message-tester "Okay" 
    '#vu8(82 0 0 0 8 0 0 0 0)
    make-authentication-okay-message
    (auth-message-test "Okay Auth")
    (make-pred-test "Okay Pred" authentication-okay-message?)))
(define run-authentication-kerberos-v5-tests
  (make-backend-message-tester "Kerberos V5" 
    '#vu8(82 0 0 0 8 0 0 0 2)
    make-authentication-kerberos-v5-message
    (auth-message-test "Kerberos V5 Auth")
    (make-pred-test "Kerberos V5 Pred" authentication-kerberos-v5-message?)))
(define run-authentication-cleartext-password-tests
  (make-backend-message-tester "Cleartext Password" 
    '#vu8(82 0 0 0 8 0 0 0 3)
    make-authentication-cleartext-password-message
    (auth-message-test "Cleartext Password Auth")
    (make-pred-test "Cleartext Password Pred" authentication-cleartext-password-message?)))
(define run-authentication-md5-password-tests
  (make-backend-message-tester "MD5 Password" 
    '#vu8(82 0 0 0 12 0 0 0 5 1 2 3 4)
    (lambda ()
      (make-authentication-md5-password-message '#vu8(1 2 3 4)))
    (auth-message-test "MD5 Password Auth")
    (make-pred-test "MD5 Password Pred" authentication-md5-password-message?)
    (make-accessor-test "MD5 Password Salt" 
      authentication-md5-password-message-salt
      '#vu8(1 2 3 4))))
(define run-authentication-scm-credential-tests
  (make-backend-message-tester "SCM Credential" 
    '#vu8(82 0 0 0 8 0 0 0 6)
    make-authentication-scm-credential-message
    (auth-message-test "SCM Credential Auth")
    (make-pred-test "SCM Credential Pred" authentication-scm-credential-message?)))
(define run-authentication-gss-tests
  (make-backend-message-tester "GSS" 
    '#vu8(82 0 0 0 8 0 0 0 7)
    make-authentication-gss-message
    (auth-message-test "GSS Auth")
    (make-pred-test "GSS Pred" authentication-gss-message?)))
(define run-authentication-sspi-tests
  (make-backend-message-tester "SSPI" 
    '#vu8(82 0 0 0 8 0 0 0 9)
    make-authentication-sspi-message
    (auth-message-test "SSPI Auth")
    (make-pred-test "SSPI Pred" authentication-sspi-message?)))
(define (run-authentication-gss-continue-tests)
  (make-backend-message-tester "GSS Continue" 
    '#vu8(82 0 0 0 13 0 0 0 8 0 0 0 0 0)
    (lambda ()
      (make-authentication-gss-continue-message '#vu8(0 0 0 0 0)))
    (auth-message-test "GSS Continue Auth")
    (make-pred-test "GSS Continue Pred" authentication-gss-continue-message?)
    (make-accessor-test "GSS Continue Data Field"
      authentication-gss-continue-message-data 
      '#vu8(0 0 0 0 0))))
))
 
(@* "Backend Key Data Messages"
"These backend messages are used for sending the key information to
the client so that cancellation messages can be validated and
authenticated by the server when the client sends them. It is
important that the front end save them if it wishes to send any
cancellation request messages to the server.
 
A BackendKeyData message has two 32-bit integers for fields."
 
(@c
(define-record-type backend-key-data-message
  (parent postgresql-backend-message)
  (fields pid key)
  (protocol
    (lambda (p)
      (lambda (pid key)
        (assert (integer? pid))
        (assert (integer? key))
        ((p) pid key)))))
))
 
(@ "The reader for this message is simple. Just read in the two
integers and we are done."
 
(@c
(define (get-backend-key-data-message port len)
  (let* ([pid (get-int32 port)]
         [key (get-int32 port)])
    (make-backend-key-data-message pid key)))
))
 
(@ "Let's define some unit tests for this little guy."
 
(@c
(define (run-backend-key-data-tests)
  (make-backend-message-tester "Backend Key Data"
    '#vu8(75 0 0 0 12 0 0 59 56 0 0 24 7)
    (lambda ()
      (make-backend-key-data-message 15160 06151))
    (make-pred-test "Backend Key Data Predicate"
      backend-key-data-message?)
    (make-accessor-test "Backend Key Data PID"
      backend-key-data-message-pid 15160)
    (make-accessor-test "Backend Key Data Key"
      backend-key-data-message-key 6151)))
))
 
(@* "Bind Messages"
"Bind messages seem to be one of the more involved messages. It uses
`B' as its character code type, and it has the following field
format:
 
\\medskip
\\itemitem{String}
The name of the destination portal (an empty string selects the
unnamed portal).
\\itemitem{String}
The name of the source prepared statement (an empty string selects the
unnamed prepared statement).
\\itemitem{Int16}
The number of parameter format codes that follow (denoted $C$ below).
This can be zero to indicate that there are no parameters or that the
parameters all use the default format (text); or one, in which case th
e specified format code is applied to all parameters; or it can equal
the actual number of parameters.
\\itemitem{Int16[$c$]}
The parameter format codes. Each must presently be zero (text) or one
(binary).
\\itemitem{Int16}
The number of parameter values that follow (possibly zero). This must
match the number of parameters needed by the query.
\\medskip
\\noindent
The following are the formats of each pair of fields that appear for
each parameter.
\\medskip
\\itemitem{Int32}
The length of the parameter value, in bytes (this count does not
include itself). Can be zero. As a special case, -1 indicates a NULL
parameter value. No value bytes follow in the NULL case.
\\itemitem{Byte$n$}
The value of the parameter, in the format indicated by the associated
format code. $n$ is the above length.
\\medskip
\\noindent
Once that is done, there are two more fields that appear at the end,
after all of the parameters are listed.
\\medskip
\\itemitem{Int16}
The number of result-column format codes that follow (denoted $R$
below). This can be zero to indicate that there are no result columns
or that the result columns should all use the default format (text);
or one, in which case the specified format code is applied to all
result columns (if any); or it can equal the actual number of result
columns of the query.
\\itemitem{Int16[$R$]}
The result-column format codes. Each must presently be zero (text) or
one (binary).
\\medskip
 
\\noindent
In the actual record layout, we do not store the lengths of any of the
arrays, but instead, this is implicit in the arrays themselves. Thus,
the above fields, which are in the message format, are broken into the
following fields in the record:

\\medskip
\\itemitem{String}
The name of the destination portal as a Scheme string.
\\itemitem{String}
The name of the source prepared statement, as a Scheme string.
\\itemitem{Vector}
The parameter format codes in a vector.
\\itemitem{Vector}
The parameter array as a vector of bytevectors.
\\itemitem{Vector}
The result-column codes in a vector.
\\medskip
 
\\noindent
Since this is a frontend object, we do not have to worry about writing
the reader code for it, since we will never have to read this thing
in. That's a good thing, because it would be annoying to write code
for that. The main work of this message is in the writer, which has to
make sure that all of the proper fields and sizes are written out when
they should be."
 
(@c
(define (compute-bind-msg-len portal stmnt formats columns params)
  (+ 
    4 ; length
    (1+ (string-length portal)) ; portal
    (1+ (string-length stmnt))
    2
    (* 2 (vector-length formats)) 
    2
    (* 4 (vector-length params))
    (let loop ([sum 0] [i 0])
      (if (= i (vector-length params)) 
          sum
          (loop (+ sum 
                   (let ([bv (vector-ref params i)])
                     (if bv (bytevector-length bv) 0)))
                (1+ i))))
    2
    (* 2 (vector-length columns))))
(define (make-bind-message-writer prtl stmnt fmts clmns prms)
  (lambda (port)
    (put-bytevector port 
      (string->bytevector prtl (native-transcoder)))
    (put-u8 port 0)
    (put-bytevector port 
      (string->bytevector stmnt (native-transcoder)))
    (put-u8 port 0)
    (put-s16 port (vector-length fmts))
    (vector-for-each 
      (lambda (c) (put-s16 port (format-code->number c)))
      fmts)
    (put-s16 port (vector-length prms))
    (vector-for-each
      (lambda (bv) 
        (put-s32 port (if bv (bytevector-length bv) -1))
        (when bv (put-bytevector port bv)))
      prms)
    (put-s16 port (vector-length clmns))
    (vector-for-each 
      (lambda (c) (put-s16 port (format-code->number c)))
      clmns)))
(define-record-type bind-message
  (parent postgresql-frontend-message)
  (fields portal statment format-codes column-codes parameters)
  (protocol
    (lambda (p)
      (lambda (portal stmnt formats columns params)
        (assert (string? portal))
        (assert (string? stmnt))
        (assert (vector? formats))
        (assert (vector? columns))
        (assert (vector? params))
        ((p #\B 
            (compute-bind-msg-len portal stmnt formats columns params)
            (make-bind-message-writer 
              portal stmnt formats columns params))
         portal stmnt formats columns params)))))
))
 
(@ "The unit tests for bind messages are a little involved, but
fortunately, they do not have to do anything with reading."
 
(@c
(define (run-bind-tests)
  (let ([bv-null
          '#vu8(66 0 0 0 12 0 0 0 0 0 0 0 0)]
        [bv-norm
          '#vu8(66 0 0 0 24
                120 0 120 0 
                0 1 0 0 
                0 1 0 0 0 2 120 0 0 1 0 0)]
        [bv-null-param
          '#vu8(66 0 0 0 16
                0 0
                0 0 0 1
                255 255 255 255
                0 0)])
    ((make-frontend-message-tester "Bind Message Empty" bv-null
       (lambda ()
         (make-bind-message "" "" '#() '#() '#()))
       (make-pred-test "Bind Message Empty Predicate"
         bind-message?)))
    ((make-frontend-message-tester "Bind Message Norm" bv-norm
       (lambda ()
         (make-bind-message "x" "x" '#(text) '#(text) '#(#vu8(120 0))))
       (make-pred-test "Bind Message Norm Predicate"
         bind-message?)))
    ((make-frontend-message-tester "Bind Message Null Parameter" 
       bv-null-param
       (lambda ()
         (make-bind-message "" "" '#() '#() '#(#f)))
       (make-pred-test "Bind Message Null Parameter Predicate"
         bind-message?)))))
))
 
(@ "The server will send back a Bind Complete message if the bind
suceeeds. It is an extremely simple packet, so we basically have
a no-op when we actually write the getter. It just constructs the
packet, because there is nothing left to read or do."
 
(@c
(define-record-type bind-complete-message
  (parent postgresql-backend-message)
  (protocol
    (lambda (p)
      (lambda () 
        ((p))))))
(define (get-bind-complete-message port len)
  (make-bind-complete-message))
))
 
(@ "Still, we should define some unit tests for this."
 
(@c
(define run-bind-complete-tests
  (make-backend-message-tester "Bind Complete" 
    '#vu8(50 0 0 0 4)
    make-bind-complete-message
    (make-pred-test "Bind Complete Predicate" 
      bind-complete-message?)))
))
 
(@* "Cancel Request Messages"
"A cancel request message can be sent whenever you want to cancel a
query or some execution. You have to provide the process id and key fr
the query that is being executed, and the correct server connection.
This is because the cancel request is always sent from a brand new
connection. Because of this, the format for a cancel request is also
different. It has only a length field and then a special 32-bit tag
field to avoid conflicts, followed by the two 32-bit fields for the
pid and key."
 
(@c
(define-record-type cancel-request-message 
  (parent postgresql-frontend-message)
  (fields pid key)
  (protocol
    (lambda (p)
      (lambda (pid key)
        ((p #\nul 4097 
            (make-cancel-request-message-writer pid key))
         pid key)))))
(define (make-cancel-request-message-writer pid key)
  (lambda (port)
    (let ([bv (make-bytevector 4)])
      (put-bytevector port '#vu8(2 3 4 5 6 7 8))
      (bytevector-s32-set! bv 0 pid (endianness big))
      (put-bytevector port bv)
      (bytevector-s32-set! bv 0 key (endianness big))
      (put-bytevector port bv))))
))
 
(@ "Let's add some simple unit tests for this one."
 
(@c 
(define run-cancel-request-tests
  (let ([bv '#vu8(0 0 0 16 1 2 3 4 5 6 7 8 0 0 0 56 0 0 0 57)])
    (make-frontend-message-tester "Cancel Request" bv
      (lambda ()
        (make-cancel-request-message 56 57))
      (make-pred-test "Cancel Request Predicate"
        cancel-request-message?))))
))
 
(@* "Closing and Command Completion"
"There are three messages I will discuss in this section. The first is
the simple close message. This is a message sent from the front end to
the back end to close portals or statements. It has two additional
fields, A single byte vield to indicate whether to close a statement
(`s') or to close a portal (`p'). The second is a string that contains
the name, possibly empty, of the statement or portal to close."
 
(@c
(define (make-close-message-writer type name)
  (let ([type-byte (char->integer
                     (case type
                       [(statement) #\S]
                       [(portal) #\P]))])
    (lambda (port)
      (put-u8 port type-byte)
      (put-bytevector port
        (string->bytevector name (native-transcoder)))
      (put-u8 port 0))))
(define-record-type close-message
  (parent postgresql-frontend-message)
  (fields type name)
  (protocol
    (lambda (p)
      (lambda (type name)
        (assert (memq type '(statement portal)))
        (assert (string? name))
        ((p #\C (+ 6 (string-length name)) 
            (make-close-message-writer type name))
         type name)))))
))
 
(@ "And let's have some unit tests for this."
 
(@c
(define run-close-tests
  (let ([bv '#vu8(67 0 0 0 10 83 98 108 97 104 0)])
    (make-frontend-message-tester "Close Message" bv
      (lambda () (make-close-message 'statement "blah"))
      (make-pred-test "Close Message Predicate"
        close-message?))))
))
 
(@ "When a close message is sent, the backend server will also send a
close complete message. This is one of those really simple message
types, with no fields."
 
(@c
(define-record-type close-complete-message
  (parent postgresql-backend-message)
  (protocol
    (lambda (p)
      (lambda ()
        ((p))))))
(define (get-close-complete-message port len)
  (make-close-complete-message))
))
 
(@ "And of course, we are going to have a little set of unit tests for
this one as we."
 
(@c
(define run-close-complete-tests
  (make-backend-message-tester "Close Complete" '#vu8(51 0 0 0 4)
    make-close-complete-message
    (make-pred-test "Close Complete Predicate" 
      close-complete-message?)))
))
 
(@ "When a command completes on the server side, then the backend will
send a Command Complete message to the frontend. This has a single
string field which indicates the command result. As documented in the
PostgreSQL manual, the following possiblities exist for the command
result string.
 
For an |INSERT| command, the tag is |INSERT oid rows|, where |rows| is
the number of rows inserted. |oid| is the object ID of the inserted
row if |rows| is 1 and the target table has OIDs; otherwise, |oid| is
0. 
 
For a |DELETE| command, the tag is |DELETE rows|, where |rows| is the
number of rows deleted.
 
For an |UPDATE| command, the tag is |UPDATE rows|, where |rows| is the
number of rows updated.
 
For a |SELECT| or |CREATE TABLE AS| command, the tag is |SELECT rows|,
where |rows| is the number of rows retrieved.
 
For a |MOVE| command, the tag is |MOVE rows|, where |rows| is the
number of rows by which the cursor's position has changed.
 
For a |FETCH| command, the tag is |FETCH rows|, where |rows| is the
number of rows that have been retrieved from the cursor.
 
For a |COPY| command, the tag is |COPY rows|, where |rows| is the
number of rows copied." 
 
(@c
(define-record-type command-complete-message
  (parent postgresql-backend-message)
  (fields tag)
  (protocol
    (lambda (p)
      (lambda (tag)
        ((p) tag)))))
(define (get-command-complete-message port len)
  (make-command-complete
    (bytevector->string
      (get-bytevector-n port (- len 1))
      (native-transcoder))))
))
 
(@ "And here we define some unit tests for the above. It's the basic
reader tests, with a test to make sure that the field is being read
correctly."
 
(@c
(define run-command-complete-tests
  (let ([bv '#vu8(67 0 0 0 9 98 108 97 104 0)])
    (make-backend-message-tester "Command Complete" bv
      (lambda () (make-command-complete-message "blah"))
      (make-pred-test "Command Complete Predicate"
        command-complete-message?)
      (make-accessor-test "Command Complete Tag Field"
        command-complete-message-tag "blah"))))
))
        
(@* "Copy Oriented Messages"
"The following messages are all related to the copy operations of the
protocol. There are five of them: data, done, fail, in response, and
out response. 
 
The Copy Data message is a frontend and backend message and has a
simple byte stream as its only field. The length of the byte stream is
deduced from the length field. "
 
(@c
(define-record-type copy-data-message
  (parent postgresql-frontend/backend-message)
  (fields data)
  (protocol
    (lambda (p)
      (lambda (data)
        (assert (bytevector? data))
        ((p #\d (+ 4 (bytevector-length data)) 
            (make-copy-data-message-writer data))
         data)))))
))
 
(@ "The reader simply reads in the bytes that it can based on the
length."
 
(@c
(define (get-copy-data-message port len)
  (make-copy-data-message (get-bytevector-n port len)))
))
 
(@ "The writer is similarly simple, and just has to put the data out."

(@c
(define (make-copy-data-message-writer data)
  (lambda (port) (put-bytevector port data)))
))
 
(@ "Let's get some unit tests written for this thing."
 
(@c
(define run-copy-data-tests
  (let ([bv '#vu8(100 0 0 0 8 98 108 97 104)])
    (make-frontend/backend-message-tester "Copy Data Message" bv
      (lambda () (make-copy-data-message '#vu8(98 108 97 104)))
      (make-pred-test "Copy Data Message Predicate"
        copy-data-message?)
      (make-accessor-test "Copy Data Message Data Field" 
        copy-data-message-data '#vu8(98 108 97 104)))))
))

(@ "The Copy Done message is also a dual message, as both frontends
and backends can send and receive them. However, this one is much
simpler, and it has no fields."
 
(@c
(define-record-type copy-done-message
  (parent postgresql-frontend/backend-message)
  (protocol
    (lambda (p)
      (lambda ()
        ((p #\c 4 (lambda (port) (void))))))))
(define (get-copy-done-message port len)
  (make-copy-done-message))
))
 
(@ "And of course, here follows the requisite set of unit tests."
 
(@c
(define run-copy-done-tests
  (let ([bv '#vu8(99 0 0 0 4)])
    (make-frontend/backend-message-tester "Copy Done Message" bv
      make-copy-done-message
      (make-pred-test "Copy Done Message Predicate"
        copy-done-message?))))
))
 
(@ "When a copy fails, a Copy Fail message is sent from the frontend.
It has a single string field that indicates the nature of the failure.
This is a string field, so we need to make sure that we null-terminate
the end of the string."
 
(@c
(define-record-type copy-fail-message
  (parent postgresql-frontend-message)
  (fields error)
  (protocol
    (lambda (p)
      (lambda (cause)
        (assert (string? cause))
        ((p #\f
            (+ 5
               (bytevector-length 
                 (string->bytevector cause (native-transcoder))))
            (make-copy-fail-message-writer cause))
         cause)))))
(define (make-copy-fail-message-writer cause)
  (let ([bv (string->bytevector cause (native-transcoder))])
    (lambda (port)
      (put-bytevector port bv)
      (put-u8 port 0))))
))
 
(@ "And now, here follows the unit testing suite for this one."
 
(@c
(define run-copy-fail-tests
  (let ([bv '#vu8(102 0 0 0 9 98 108 97 104 0)])
    (make-frontend-message-tester "Copy Fail Message" bv
      (lambda () (make-copy-fail-message "blah"))
      (make-pred-test "Copy Fail Message Predicate"
        copy-fail-message?))))
))
 
 
(@ "The Copy In Response and Copy Out Response messages are basically
identical, except that they have different types. They both have a
format field and an array of 16-bit integers for the format codes of
each column." 
 
(@c 
(define (make-response-constructor p)
  (lambda (format codes)
    (assert (format-code? format))
    (assert
      (and (vector? codes)
           (for-all format-code? (vector->list codes))))
    ((p) format codes)))
(define-record-type copy-in-response-message
  (parent postgresql-backend-message)
  (fields format codes)
  (protocol (lambda (p) (make-response-constructor p))))
(define-record-type copy-out-response-message
  (parent postgresql-backend-message)
  (fields format codes)
  (protocol (lambda (p) (make-response-constructor p))))
))
 
(@ "It is basically the same process for getting each of these from a
binary input port, so we'll just abstract it and use the same thing
twice."
 
(@c
(define (make-response-getter make)
  (lambda (port len)
    (let* ([fmt (number->format-code (get-u8 port))]
           [cols (get-s16 port)]
           [vec (make-vector cols)])
      (do ([i 0 (fx1+ i)])
          [(= i cols)]
        (vector-set! vec i
          (number->format-code (get-s16 port))))
      (make fmt vec))))
(define get-copy-in-response-message 
  (make-response-getter make-copy-in-response-message))
(define get-copy-out-response-message
  (make-response-getter make-copy-out-response-message))
))
 
(@ "And here are some unit tests for these poor fellows."
 
(@c
(define run-copy-in-response-tests
  (let ([bv '#vu8(71 0 0 0 11 1 0 2 0 0 0 1)])
    (make-backend-message-tester "Copy In Response Message" bv
      (lambda ()
        (make-copy-in-response-message 'binary '#(text binary)))
      (make-pred-test "Copy In Response Message Predicate"
        copy-in-response-message?)
      (make-accessor-test "Copy In Response Message Format" 
        copy-in-response-message-format 'binary)
      (make-accessor-test "Copy In Response Message Codes"
        copy-in-response-message-codes '#(text binary)))))
(define run-copy-out-response-tests
  (let ([bv '#vu8(72 0 0 0 11 1 0 2 0 0 0 1)])
    (make-backend-message-tester "Copy Out Response Message" bv
      (lambda ()
        (make-copy-out-response-message 'binary '#(text binary)))
      (make-pred-test "Copy Out Response Message Predicate"
        copy-out-response-message?)
      (make-accessor-test "Copy Out Response Message Format"
        copy-out-response-message-format 'binary)
      (make-accessor-test "Copy Out Response Message Codes"
        copy-out-response-message-codes '#(text binary)))))
))
 
(@* "Data Row Messages"
"A data row message consists of an array of column value pairs, where
each pair is a length field and a byte vector. The Lengths of the
fields are in 32-bit integers, while the column count is a 16-bit
integer. Fields can have a length of $-1$, which indicates that the
column has a null value. 
 
In Scheme, these are represented as a vector of values."
 
(@c
(define-record-type data-row-message
  (parent postgresql-backend-message)
  (fields columns)
  (protocol
    (lambda (p) (lambda (columns) ((p) columns)))))
(define (get-data-row-message port len)
  (let ([cols (get-s16 port)])
    (let ([vec (make-vector cols)])
      (do ([i 0 (fx1+ i)])
          [(= i cols)]
        (let ([len (get-s32 port)])
          (cond
            [(= -1 len)
             (vector-set! vec i #f)]
            [(not (negative? len))
             (vector-set! vec i (get-bytevector-n port len))]
            [else
              (errorf 'get-data-row-message 
                "Bad field length ~s" len)])))
      (make-data-row-message vec))))
))
 
(@ "Here are the unit tests for a data row message."
 
(@c
(define run-data-row-tests
  (let ([bv '#vu8(68 0 0 0 18 0 2 
                  255 255 255 255
                  0 0 0 4 98 108 97 104)])
    (make-backend-message-tester "Data Row Message" bv
      (lambda () (make-data-row-message '#(#f #vu8(98 108 97 104))))
      (make-pred-test "Data Row Message Predicate" 
        data-row-message?)
      (make-accessor-test "Data Row Message Columns" 
        data-row-message-columns '#(#f #vu8(98 108 97 104))))))
))
 
(@* "Describe Messages"
"This is a frontend message that is used to describe portals and
statements. It has a field to indicate whether it is for a portal
or a statement, as well as a string field for the actual description."
 
(@c
(define-record-type describe-message
  (parent postgresql-frontend-message)
  (fields type description)
  (protocol
    (lambda (p)
      (lambda (type description)
        (assert (memq type '(statement portal)))
        (assert (string? description))
        (let ([bv (string->bytevector description (native-transcoder))])
          ((p #\D (+ 2 (bytevector-length bv))
              (make-describe-message-writer type bv))
           type description))))))
(define (make-describe-message-writer type desc)
  (lambda (port)
    (put-u8 port
      (case type
        [(statement) (char->integer #\S)]
        [(portal) (char->integer #\P)]
        [else (errorf 'describe-message "Invalid type ~s" type)]))
    (put-bytevector port desc)
    (put-u8 port 0)))
))
 
(@ "The unit tests for describe messages follows. They're standard and
unsurprising."
 
(@c
(define run-describe-tests
  (let ([bv '#vu8(68 0 0 0 6 83 98 108 97 104 0)])
    (make-frontend-message-tester "Describe Message" bv
      (lambda () (make-describe-message 'statement "blah"))
      (make-pred-test "Describe Message Predicate"
        describe-message?))))
))
 
(@* "Query and Error Responses"
"The server can respond with empty query and error responses as well
as the normal responses. And empty query response is a simple query
with the `I' character as its code. It has no fields."
 
(@c
(define-record-type empty-query-response-message
  (parent postgresql-backend-message)
  (protocol
    (lambda (p) (lambda () ((p))))))
(define (get-empty-query-response-message port len)
  (make-empty-query-response-message))
))
 
(@ "Unit tests for these are trivial."
 
(@c
(define run-empty-query-tests
  (let ([bv '#vu8(73 0 0 0 4)])
    (make-backend-message-tester "Empty Query Response Message" bv
      make-empty-query-response-message
      (make-pred-test "Empty Query Response Message Predicate"
        empty-query-response-message?))))
))
 
(@ "An error response has a bit more information to it. It uses the
`E' character code, It returns a list of error code and value pairs.
The error codes are as follows:
 
\\medskip
\\item{|S|} Severity
\\item{|C|} Code
\\item{|M|} Message
\\item{|D|} Detail
\\item{|H|} Hint
\\item{|P|} Position
\\item{|p|} Internal Position
\\item{|q|} Internal Query
\\item{|W|} Where
\\item{|F|} File
\\item{|L|} Line
\\item{|R|} Routine
\\medskip
 
\\noindent
On the Scheme side we use the long hand versions, lower cased, with
dashes for spaces, as symbols, and convert to the byte versions
internally."
 
(@c
(define-record-type error-response-message
  (parent postgresql-backend-message)
  (fields codes)
  (protocol
    (lambda (p)
      (lambda (codes)
        (assert (list? codes))
        (assert (for-all pair? codes))
        (assert (for-all symbol? (map car codes)))
        (assert (for-all string? (map cdr codes)))
        ((p) codes)))))
(define (get-error-response-message port len)
  (define (get-code)
    (let* ([code (get-u8 port)]
           [string (get-pg-string port)])
      (cons (byte->error-code code) string)))
  (do ([res '() (cons (get-code) res)])
      [(zero? (lookahead-u8 port))
       (get-u8 port)
       (make-error-response-message (reverse res))]))
))
 
(@ "The |byte->error-code| procedure should take in a byte and convert
it to the error-code symbol that we use in Scheme."
 
(@c
(define (byte->error-code code)
  (case (integer->char code)
    [(#\S) 'severity]
    [(#\C) 'code]
    [(#\M) 'message]
    [(#\D) 'detail]
    [(#\H) 'hint]
    [(#\P) 'position]
    [(#\p) 'internal-position]
    [(#\q) 'internal-query]
    [(#\W) 'where]
    [(#\F) 'file]
    [(#\L) 'line]
    [(#\R) 'routine]
    [else (errorf 'byte->error-code
            "unrecognized code ~s" code)]))
))
 
(@ "Now let's write some unit tests for this guy."
 
(@c
(define run-error-response-tests
  (let ([bv '#vu8(69 0 0 0 11 77 98 108 97 104 0 0)])
    (make-backend-message-tester "Error Response Message" bv
      (lambda ()
        (make-error-response-message '((message . "blah"))))
      (make-pred-test "Error Response Message Predicate"
        error-response-message?)
      (make-accessor-test "Error Response Message Codes"
        error-response-message-codes '((message . "blah"))))))
))
 
(@* "Execute and flush message types"
"The execute message executes a query that has been parsed and
prepared. It uses `E' as its character code and it is a frontend
message type. It takes two fields, the first a possibly zero-length
string indicating the portal, the second a 32-bit integer indicating
the maximimum number of rows to return. Specifying zero here indicates
a no limit response."
 
(@c
(define-record-type execute-message
  (parent postgresql-frontend-message)
  (fields portal max-rows)
  (protocol
    (lambda (p)
      (lambda (portal max-rows)
        (assert (string? portal))
        (assert 
          (and (integer? max-rows) (exact? max-rows) 
               (not (negative? max-rows))))
        (let ([bv (string->bytevector portal (native-transcoder))])
          ((p #\E (+ 9 (bytevector-length bv))
              (make-execute-message-writer bv max-rows))
           portal max-rows))))))
))
 
(@ "The writer is pretty straightforward. Just remember that we need
to null-terminate the strings."
 
(@c
(define (make-execute-message-writer bv max-rows)
  (lambda (port)
    (put-bytevector port bv)
    (put-u8 port 0)
    (put-s32 port max-rows)))
))
 
(@ "Let's get some unit testing on this baby."
 
(@c
(define run-execute-tests
  (let ([bv '#vu8(69 0 0 0 13 98 108 97 104 0 0 0 0 100)])
    (make-frontend-message-tester "Execute Message" bv
      (lambda ()
        (make-execute-message "blah" 100))
      (make-pred-test "Execute Message Predicate" 
        execute-message?))))
))
 
(@ "We also have a flush message type for flushing things, duh. It's a
simple message with no fields and `H' as its code."
 
(@c
(define-record-type flush-message
  (parent postgresql-frontend-message)
  (protocol
    (lambda (p)
      (lambda ()
        ((p #\H 4 (lambda (port) (void))))))))
))
 
(@ "The unit tests are similarly simple."
 
(@c
(define run-flush-tests
  (let ([bv '#vu8(72 0 0 0 4)])
    (make-frontend-message-tester "Flush Message" bv
      make-flush-message
      (make-pred-test "Flush Message Predicate" flush-message?))))
))
 
(@* "More server responses"
"There are five message types that are sort of notification or
asynchronousy status reports and the like. We will define these here. 
 
The first is the no data response. This is simple and mostly trivial,
so we'll define it together with its unit tests here."
 
(@c
(define-record-type no-data-message
  (parent postgresql-backend-message)
  (protocol
    (lambda (p) (lambda () ((p))))))
(define (get-no-data-message port len)
  (make-no-data-message))
(define run-no-data-tests
  (let ([bv '#vu8(110 0 0 0 4)])
    (make-backend-message-tester "No Data Message" bv
      make-no-data-message
      (make-pred-test "No Data Message Predicate" no-data-message?))))
))
 
 
(@ "We also have the Notice response message type. This uses `N' as
its message type, followed by a list of response codes, just as in
error responses."
 
(@c
(define-record-type notice-response-message
  (parent postgresql-backend-message)
  (fields codes)
  (protocol
    (lambda (p)
      (lambda (codes)
        (assert (list? codes))
        (assert (for-all pair? codes))
        (assert (for-all symbol? (map car codes)))
        (assert (for-all string? (map cdr codes)))
        ((p) codes)))))
))
 
(@ "To read in a notice response message, we have to collect up the
list of codes, where the list is a null-terminated set of pairs of
bytes and strings."
 
(@c
(define (get-notice-response-message port len)
  (define (get-code)
    (let* ([code (get-u8 port)]
           [string (get-pg-string port)])
      (cons (byte->error-code code) string)))
  (do ([res '() (cons (get-code) res)])
      [(zero? (lookahead-u8 port))
       (get-u8 port)
       (make-notice-response-message (reverse res))]))
))
 
(@ "And here are some unit tests for it."
 
(@c
(define run-notice-response-tests
  (let ([bv '#vu8(78 0 0 0 11 83 98 108 97 104 0 0)])
    (make-backend-message-tester "Notice Response Message" bv
      (lambda () 
        (make-notice-response-message '((severity . "blah"))))
      (make-pred-test "Notice Response Message Predicate" 
        notice-response-message?)
      (make-accessor-test "Notice Response Message Codes"
        notice-response-message-codes '((severity . "blah"))))))
))
 
(@ "Notice responses are not to be confused with Notification
Responses, which I am going to define right now. The notification
response has a code `A' and includes three fields, the 32-bit integer
process id, a string for the name of the channel, and a string for the
``payload'' of the notifying process."
 
(@c
(define-record-type notification-response-message
  (parent postgresql-backend-message)
  (fields pid channel payload)
  (protocol
    (lambda (p)
      (lambda (pid channel payload)
        (assert (and (integer? pid) (exact? pid) (not (negative? pid))))
        (assert (string? channel))
        (assert (string? payload))
        ((p) pid channel payload)))))
))
 
(@ "The getter is pretty simple with the help of out utilities."
 
(@c
(define (get-notification-response-message port len)
  (let* ([pid (get-s32 port)]
         [channel (get-pg-string port)]
         [payload (get-pg-string port)])
    (make-notification-response-message pid channel payload)))
))
 
(@ "And of course, we have to have unit tests."
 
(@c
(define run-notification-response-tests
  (let ([bv '#vu8(65 0 0 0 18 
                  0 0 0 10 
                  98 108 97 104 0 
                  98 108 97 104 0)])
    (make-backend-message-tester "Notification Response Message" bv
      (lambda () 
        (make-notification-response-message 10 "blah" "blah"))
      (make-pred-test "Notification Response Message Predicate"
        notification-response-message?)
      (make-accessor-test "Notification Response Message PID"
        notification-response-message-pid 10)
      (make-accessor-test "Notification Response Message Channel"
        notification-response-message-channel "blah")
      (make-accessor-test "Notification Response Message Payload"
        notification-response-message-payload "blah"))))
))
 
(@ "The last two responses in here are parameter related message
types. The first, descriptions, encapsulates the object id of the
parameter data types for a given statement. It has an array of 32-bit
integers to represent these object ids." 
 
(@c
(define-record-type parameter-description-message
  (parent postgresql-backend-message)
  (fields oids)
  (protocol
    (lambda (p)
      (lambda (oids)
        (assert (vector? oids))
        (assert (for-all integer? (vector->list oids)))
        ((p) oids)))))
))
 
(@ "The actual message format has an extra field to indicate the
length of the array, which assists us when writing the reader for this
message type."
 
(@c
(define (get-parameter-description-message port len)
  (let ([len (get-s16 port)])
    (let ([vec (make-vector len)])
      (do ([i 0 (fx1+ i)])
          [(= i len)]
        (vector-set! vec i (get-s32 port)))
      (make-parameter-description-message vec))))
))
 
(@ "And here are the unit tests."
 
(@c
(define run-parameter-description-tests
  (let ([bv '#vu8(116 0 0 0 14 0 2 0 0 0 1 0 0 0 2)])
    (make-backend-message-tester "Parameter Description Message" bv
      (lambda () 
        (make-parameter-description-message '#(1 2)))
      (make-pred-test "Parameter Description Message Predicate"
        parameter-description-message?)
      (make-accessor-test "Parameter Description Message OIDs" 
        parameter-description-message-oids '#(1 2)))))
))
 
(@ "The last little message type for this section is the Parameter
status message. This one is for updating the client when a parameter
has been changed or altered. It has two string fields, the first being
the name of the run-time parameter that is in question, and the second
being the current value of the parameter." 
 
(@c
(define-record-type parameter-status-message
  (parent postgresql-backend-message)
  (fields name value)
  (protocol
    (lambda (p)
      (lambda (name value)
        (assert (and (string? name) (string? value)))
        ((p) name value)))))
))
 
(@ "The reader is also easy since we can use |get-pg-string|." 

(@c
(define (get-parameter-status-message port len)
  (let* ([name (get-pg-string port)]
         [value (get-pg-string port)])
    (make-parameter-status-message name value)))
)) 
 
(@ "The unit tests are next, they will check both the name and value
fields." 
 
(@c
(define run-parameter-status-tests
  (let ([bv '#vu8(83 0 0 0 14 98 108 97 104 0 98 108 97 104 0)])
    (make-backend-message-tester "Parameter Status Message" bv
      (lambda ()
        (make-parameter-status-message "blah" "blah"))
      (make-pred-test "Parameter Status Message Predicate"
        parameter-status-message?)
      (make-accessor-test "Parameter Status Message Name"
        parameter-status-message-name "blah")
      (make-accessor-test "Parameter Status Message Value"
        parameter-status-message-value "blah"))))
))
 
(@* "Parse Message Types"
"There are two parse message types, a front end Parse message, and a
backend Parse Complete message. The Parse message has two string
fields for the destination and the query. Additionally, it contains an
array of object IDs for the parameters that may be prespecified."
 
(@c
(define-record-type parse-message
  (parent postgresql-frontend-message)
  (fields destination query parameters)
  (protocol
    (lambda (p)
      (lambda (dest query params)
        (assert (string? dest))
        (assert (string? query))
        (assert (vector? params))
        (assert (for-all integer? (vector->list params)))
        (let ([dest-bv (string->bytevector dest
                         (native-transcoder))]
              [query-bv (string->bytevector query
                          (native-transcoder))])
          ((p #\P
              (+ (bytevector-length dest-bv)
                 (bytevector-length query-bv)
                 6
                 (* 4 (vector-length params)))
              (make-parse-message-writer dest-bv query-bv params))
           dest query params))))))
))
 
(@ "The writer for Parse messages just prints out the fields, making
sure to null terminate the strings and putting a count before the
field array."
 
(@c
(define (make-parse-message-writer dbv qbv pms)
  (lambda (port)
    (put-bytevector port dbv) 
    (put-u8 port 0)
    (put-bytevector port qbv)
    (put-u8 port 0)
    (put-s16 port (vector-length pms))
    (do ([i 0 (fx1+ i)]) [(= i (vector-length pms))]
      (put-s32 port (vector-ref pms i)))))
))
 
(@ "That's all for this message type, modulo the following unit
tests." 

(@c
(define run-parse-tests
  (let ([bv '#vu8(80 0 0 0 24
                  98 108 97 104 0 98 108 97 104 0 
                  0 2 0 0 0 1 0 0 0 2)])
    (make-frontend-message-tester "Parse Message" bv
      (lambda ()
        (make-parse-message "blah" "blah" '#(1 2)))
      (make-pred-test "Parse Message Predicate" parse-message?))))
)) 
 
(@* "Format codes"
"Some of the messages above deal with format codes. These shouldn't be
numbers on the Scheme side. Symbols are good analogs and the right
choice here. The following are some simple procedures for making this
difference and conversion between easier."
 
(@c
(define (format-code? x) (memq x '(text binary)))
(define (format-code->number x)
  (case x
    [(text) 0]
    [(binary) 1]
    [else (errorf 'format-code->number "unsupported format code ~a"
            x)]))
(define (number->format-code x)
  (case x
    [(0) 'text] [(1) 'binary]
    [else
      (errorf 'number->format-code "unsupported format code ~a" x)]))
))
        
(@* "Unit Test Runner"
"Let's make sure that we can run all of our unit tests at once if we
want to do so."
 
(@c
(define (run-postgresql-tests)
  (test-begin "Postgresql")
  (run-authentication-tests)
  (run-backend-key-data-tests)
  (run-bind-tests)
  (run-bind-complete-tests)
  (run-cancel-request-tests)
  (run-close-tests)
  (run-close-complete-tests)
  (run-copy-data-tests)
  (run-copy-done-tests)
  (run-copy-fail-tests)
  (run-copy-in-response-tests)
  (run-copy-out-response-tests)
  (run-data-row-tests)
  (run-describe-tests)
  (run-execute-tests)
  (run-flush-tests)
  (run-error-response-tests)
  (run-no-data-tests)
  (run-notice-response-tests)
  (run-notification-response-tests)
  (run-parameter-description-tests)
  (run-parameter-status-tests)
  (run-parse-tests)
  (test-end "Postgresql"))
))
 
(@* "Register message handlers"
"Let's register the message handlers now."
 
(@c
(@< |Register message readers|)
))
 
;)
