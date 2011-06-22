#!chezscheme
(@chezweb)

(@e
  (import (arcfide chezweb weave parameters))
  (max-simple-elems 4)
  (list-columns 2))

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

(@* "Version 8.0 Workaround"
"There is currently a bug in version 8.0 of Chez Scheme that
causes some problems with the following library. To fix this, we run
the following code as a workaround."
 
(@c
(meta-cond
  [(let-values ([(major minor patch) (scheme-version-number)])
     (and (= 8 major) (= 0 minor)))
   (#%$sremprop 'r6rs:record-constructor 'cp02)])
))

(@l "This library defines a pure Scheme PostgreSQL driver supporting 
the PostgreSQL Message protocol version 3.0. This protocol is supported 
by all versions of PostgreSQL 7.4 and later. It is a pure Scheme driver, 
meaning that it does not depend or interface with any foreign libraries 
for its operation. It is written in a fairly Schemely style and will 
hopefully support the full range of the protocol."

(arcfide postgresql)
(export 
  field-description-name
  field-description-object-id
  field-description-attribute
  field-description-type-object-id
  field-description-size
  field-description-modifier
  field-description-format
  postgresql-connect
  postgresql-connection?
  postgresql-terminate-connection
  postgresql-cancel-request
  query-result? query-result-format query-result-rows
  postgresql-simple-query
  run-postgresql-tests
)
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
 
(@* "Loading this library"
"This library relies on the |(arcfide sockets)| library, which has
some foreign library dependencies. Before attempting to load this
library, you should make sure that your application loads the correct
libraries. As of the time of this writing this should be done with the
following:
 
\\medskip\\verbatim
(load-shared-object \"libc.so.6\")
(load-shared-object \"sockets.so.1\")
(load-shared-object \"chez_errno.so.1\")
|endverbatim
\\medskip

\\noindent
The above are for Linux machines. You may have a different set of
libraries for Windows or Mac OS X boxes.")
 
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
  (parent postgresql-message))
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
  (parent postgresql-backend-message))
))

(@ "{\\it Authentication Okay.} These packets are eight bytes and have
a zero as their type. They have not additional fields."
 
(@c
(define-record-type authentication-okay-message
  (parent authentication-message))
))
 
(@ "{\\it Authentication Kerveros V5.}
These packets do not have any additional field and use 2 as their
authentication type."
 
(@c
(define-record-type authentication-kerberos-v5-message
  (parent authentication-message))
))
 
(@ "{\\it Authentication Cleartext Password.}
These messages have no additional fields and use 3 as their
authentication type."
 
(@c
(define-record-type authentication-cleartext-password-message
  (parent authentication-message))
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
  (parent authentication-message))
))
 
(@ "{\\it Authentication GSS.} 
This message defines no additional fields and uses 7 as the
authentication type."
 
(@c
(define-record-type authentication-gss-message
  (parent authentication-message))
))
 
(@ "{\\it Authentication SSPI.}
This message does not define additional fields and uses 9 as its
authentication type."
 
(@c
(define-record-type authentication-sspi-message
  (parent authentication-message))
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
    (#\K . ,get-backend-key-data-message)
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
    (#\1 . ,get-parse-complete-message)
    (#\s . ,get-portal-suspended-message)
    (#\Z . ,get-ready-for-query-message)
    (#\T . ,get-row-description-message)
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
  (let* ([pid (get-s32 port)]
         [key (get-s32 port)])
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
  (parent postgresql-backend-message))
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
  (parent postgresql-backend-message))
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
  (make-command-complete-message
    (get-pg-string port)))
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
  (parent postgresql-backend-message))
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
  (parent postgresql-backend-message))
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
        (assert (and (symbol? name) (string? value)))
        ((p) name value)))))
))
 
(@ "The reader is also easy since we can use |get-pg-string|." 

(@c
(define (get-parameter-status-message port len)
  (let* ([name (string->symbol (get-pg-string port))]
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
        (make-parameter-status-message 'blah "blah"))
      (make-pred-test "Parameter Status Message Predicate"
        parameter-status-message?)
      (make-accessor-test "Parameter Status Message Name"
        parameter-status-message-name 'blah)
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
                 8
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
 
(@ "The server response to a parse message is a Parse Complete
message, at least, if it worked. It does not have any fields, and it
uses `1' as its code."
 
(@c
(define-record-type parse-complete-message
  (parent postgresql-backend-message))
(define (get-parse-complete-message port len)
  (make-parse-complete-message))
(define run-parse-complete-tests
  (let ([bv '#vu8(49 0 0 0 4)])
    (make-backend-message-tester "Parse Complete Message" bv
      make-parse-complete-message
      (make-pred-test "Parse Complete Predicate"
        parse-complete-message?))))
))
 
(@* "Passwords, portals, and queries"
"This section details some of the messages for password messages,
portal suspended messages, and two query oriented messages. 
 
The frontend Password message contains only a single string field that
contains the possibly encrypted password. 
"
 
(@c
(define-record-type password-message
  (parent postgresql-frontend-message)
  (fields value)
  (protocol
    (lambda (p)
      (lambda (pass)
        (assert (string? pass))
        (let ([bv (string->bytevector pass (native-transcoder))])
          ((p #\p (+ 5 (bytevector-length bv))
              (lambda (port)
                (put-bytevector port bv) (put-u8 port 0)))
           pass))))))
(define run-password-tests
  (let ([bv '#vu8(112 0 0 0 9 98 108 97 104 0)])
    (make-frontend-message-tester "Password Message" bv
      (lambda () (make-password-message "blah"))
      (make-pred-test "Password Message Predicate" 
        password-message?))))
))
 
(@ "If an Execute message's row-count limit is reached, then the
backend will send a Port Suspended message to the client. This message
uses `s' as its code and has no fields."
 
(@c
(define-record-type portal-suspended-message
  (parent postgresql-backend-message))
(define (get-portal-suspended-message port len)
  (make-portal-suspended-message))
(define run-portal-suspended-tests
  (let ([bv '#vu8(115 0 0 0 4)])
    (make-backend-message-tester "Portal Suspended Message" bv
      make-portal-suspended-message
      (make-pred-test "Portal Suspended Message Predicate" 
        portal-suspended-message?))))
))
 
(@ "Now let's define two messages that work together. The first,
Query, is a frontend message that uses `Q' as its code. it has only a
string to represent the query as its sole field."
 
(@c
(define-record-type query-message
  (parent postgresql-frontend-message)
  (fields value)
  (protocol 
    (lambda (p)
      (lambda (query)
        (assert (string? query))
        (let ([bv (string->bytevector query (native-transcoder))])
          ((p #\Q (+ 5 (bytevector-length bv))
              (lambda (port)
                (put-bytevector port bv) (put-u8 port 0)))
           query))))))
(define run-query-tests
  (let ([bv '#vu8(81 0 0 0 9 98 108 97 104 0)])
    (make-frontend-message-tester "Query Message" bv
      (lambda () (make-query-message "blah"))
      (make-pred-test "Query Message Predicate" query-message?)
      (make-accessor-test "Query Message Predicate" 
        query-message-value "blah"))))
))
 
(@ "Whenever the server is ready for a new query, it will send a Ready
for Query message. This is a message that has only a single one-byte
field that contains one of the following values:
 
\\medskip
\\item{|I|} if the server is idle, that is, not a transaction block.
\\item{|T|} if the server is in a transaction block.
\\item{|E|} if the server is in a failed transaction block. In this
state, queries will be rejected until the block is ended.
\\medskip
 
\\noindent
Naturally, I want to use real symbols here instead of using these
constants, so let's start by defining some procedures for handling
this." 
 
(@c
(define (transaction-status? x) (memq x '(idle transaction error)))
(define (byte->transaction-status x)
  (case (integer->char x)
    [(#\I) 'idle]
    [(#\T) 'transaction]
    [(#\E) 'error]
    [else (errorf 'byte->transaction-status 
            "unknown transaction status ~s" x)]))
)) 
 
(@ "Now that we have those, we can define the actual message type
easily."
 
(@c
(define-record-type ready-for-query-message
  (parent postgresql-backend-message)
  (fields transaction-status)
  (protocol
    (lambda (p)
      (lambda (status)
        (assert (transaction-status? status))
        ((p) status)))))
(define (get-ready-for-query-message port len)
  (make-ready-for-query-message
    (byte->transaction-status (get-u8 port))))
))
 
(@ "And, of course, there are a few unit tests that need to be
defined, especially to ensure that the accessor work."
 
(@c
(define run-ready-for-query-tests
  (let ([bv '#vu8(90 0 0 0 5 73)])
    (make-backend-message-tester "Ready for Query Message" bv
      (lambda () (make-ready-for-query-message 'idle))
      (make-pred-test "Ready for Query Message Predicate" 
        ready-for-query-message?)
      (make-accessor-test "Ready for Query Message Status"
        ready-for-query-message-transaction-status 'idle))))
))
 
(@* "Row Description Messages"
"A server might need to send some information to the client about the
types and information about the fields in the rows that it is sending
back to the client. These are sent in the Row Description message. Row
Description Messages use `T' as their code and consist of an array of
column descriptions. Each column description is a set of fields
described as follows:
 
\\medskip
\\item{Name} This is a string indicating the field name.
\\item{OID} This is either zero or it indicates the OID
of the column if the column can be identified as belonging to a
specific table.
\\item{Attr} If the column belongs to a specific tablen this is the
attirbute of the column, and it is zero otherwise.
\\item{DOID} This is the Object ID of the field's data type.
\\item{size} This value indicates the size of the object, but a
negative type indicates a variable width type.
\\item{Mod} This is a type-specific modifier.
\\item{Fmt} This is the format code of the field.
\\medskip
 
\\noindent 
To assist in parsing the above out, let's first define a simple
structure to hold these values. "
 
(@c
(define-record-type field-description
  (fields name object-id attribute type-object-id
          size modifier format))
))
 
(@ "Armed with this we can define a reader for field descriptions." 
 
(@c
(define (get-field-description port)
  (define (convert-size x)
    (if (= -1 x) 'variable x))
  (let* ([name (get-pg-string port)]
         [oid (get-s32 port)]
         [attr (get-s16 port)]
         [doid (oid->typename (get-s32 port))]
         [size (convert-size (get-s16 port))]
         [mod (get-s32 port)]
         [fmt (number->format-code (get-s16 port))])
    (make-field-description name oid attr doid size mod fmt)))
))
 
(@ "Now we can define a record type for the row description messages."
 
(@c
(define-record-type row-description-message
  (parent postgresql-backend-message)
  (fields fields)
  (protocol
    (lambda (p)
      (lambda (fields)
        (assert (vector? fields))
        (assert (for-all field-description? (vector->list fields)))
        ((p) fields)))))
))
 
(@ "We also need to define a reader for this. We just have to grab the
initial count and then iterate over a series of
|get-field-description| calls."
 
(@c
(define (get-row-description-message port len)
  (let ([len (get-s16 port)])
    (let ([vec (make-vector len)])
      (do ([i 0 (fx1+ i)]) [(= i len)]
        (vector-set! vec i (get-field-description port)))
      (make-row-description-message vec))))
))
 
(@ "Now let's see how we can make a unit test for this thing." 
 
(@c
(define run-row-description-tests
  (let ([bv '#vu8(84 0 0 0 29 0 1 
                  98 108 97 104 0
                  0 0 0 1 0 2 0 0 0 25 0 4 0 0 0 5 0 0)])
    (make-backend-message-tester "Row Description Message" bv
      (lambda ()
        (let ([fd (make-field-description "blah" 1 2 'text 4 5 'text)])
          (make-row-description-message (vector fd))))
      (make-pred-test "Row Description Message" 
        row-description-message?)
      (make-accessor-test "Row Description Message Fields"
        (lambda (msg)
          (let ([fields (row-description-message-fields msg)])
            (let ([fd (vector-ref fields 0)])
              (list 
                (field-description-name fd)
                (field-description-object-id fd)
                (field-description-attribute fd)
                (field-description-type-object-id fd)
                (field-description-size fd)
                (field-description-modifier fd)
                (field-description-format fd)))))
        '("blah" 1 2 text 4 5 text)))))
))

(@* "SSL, Startup, Shutdown, and Sync Messages"
"This section details the startup and sync related messages. The first
being the relatively simple SSL Request message. It's a special field
that contains a series of magic values to indicate that it is a SSL
Request. It is a frontend message."
 
(@c
(define-record-type ssl-request-message
  (parent postgresql-frontend-message)
  (protocol
    (lambda (p)
      (lambda ()
        ((p #\nul 2049 
          (lambda (port)
            (put-bytevector port '#vu8(2 3 4 5 6 7 9)))))))))
(define run-ssl-request-tests
  (let ([bv '#vu8(0 0 0 8 1 2 3 4 5 6 7 9)])
    (make-frontend-message-tester "SSL Request Message" bv
      make-ssl-request-message
      (make-pred-test "SSL Request Message" ssl-request-message?))))
))
 
(@ "The first message sent out is the startup message from the client.
It is a frontend message that contains the length field and the
version number, followed by a list of parameters that is terminated by
a null byte. Each parameter is a string-string pair where the first
string is the name and the second is the value."
 
(@c
(define-record-type startup-message
  (parent postgresql-frontend-message)
  (fields major minor params)
  (protocol
    (lambda (p)
      (lambda (params)
        (assert (list? params))
        (assert (for-all pair? params))
        (assert (for-all symbol? (map car params)))
        (assert (for-all string? (map cdr params)))
        ((p #\nul (compute-startup-message-length params)
          (make-startup-message-writer params))
         3 0 params)))))
))
 
(@ "The writer for the startup message will always send the version
protocol value first, followed by the name value pairs and the final
null."
 
(@c
(define (make-startup-message-writer params)
  (lambda (port)
    (put-bytevector port '#vu8(3 0))
    (put-u8 port 0)
    (for-each
      (lambda (e)
        (put-bytevector port
          (string->bytevector (symbol->string (car e))
            (native-transcoder)))
        (put-u8 port 0)
        (put-bytevector port 
          (string->bytevector (cdr e) (native-transcoder)))
        (put-u8 port 0))
      params)
    (put-u8 port 0)))
))
 
(@ "Testing this requires that we have the right string. For this,
I'm going to be using only an |user| parameter and the user name
|arcfide|. Here is how those value map out:
 
\\medskip
\\itemitem{|user|} |#vu8(117 115 101 114)|
\\itemitem{|arcfide|} |#vu8(97 114 99 102 105 100 101)|
\\medskip
 
\\noindent
Since the version number is constant, that doesn't change anything.
Otherwise, this is a straightforward writing unit test.
"
 
(@c
(define run-startup-tests
  (let ([bv '#vu8(0 0 0 22 0 3 0 0 
                  117 115 101 114 0 97 114 99 102 105 100 101 0 0)])
    (make-frontend-message-tester "Startup Message" bv
      (lambda () (make-startup-message '((user . "arcfide"))))
      (make-pred-test "Startup Message Predicate" startup-message?))))
))
 
(@ "Computing the size of the startup message is a little involved,
so we are doing it in a separate procedure here. We have a constant
eight byte pad that is made up of the length field and the initial
version number, but then we have the strings and symbols, which
require some more work. To make it easy, we have a simple procedure
for computing the length of any one field, and another to compute the
length of each field pair."
 
(@c
(define (elem-length x)
  (1+ (bytevector-length (string->bytevector x (native-transcoder)))))
(define (sym-length x)
  (elem-length (symbol->string x)))
(define (pair-length x)
  (+ (sym-length (car x)) (elem-length (cdr x))))
(define (compute-startup-message-length params)
  (* 256 (+ 9 (apply + (map pair-length params)))))
))

(@ "The sync message is a simple front end message that indicates a
sync request from the client." 
 
(@c
(define-record-type sync-message
  (parent postgresql-frontend-message)
  (protocol
    (lambda (p) 
      (lambda ()
        ((p #\S 4 (lambda (port) (void))))))))
(define run-sync-tests
  (let ([bv '#vu8(83 0 0 0 4)])
    (make-frontend-message-tester "Sync Message" bv
      make-sync-message
      (make-pred-test "Sync Message Predicate" sync-message?))))
))
 
(@ "Our final message type is a terminate message type that the front
end will send to the back end to indicate that the client is
terminating." 
 
(@c
(define-record-type terminate-message
  (parent postgresql-frontend-message)
  (protocol
    (lambda (p)
      (lambda ()
        ((p #\X 4 (lambda (port) (void))))))))
(define run-terminate-tests
  (let ([bv '#vu8(88 0 0 0 4)])
    (make-frontend-message-tester "Terminate Message" bv
      make-terminate-message
      (make-pred-test "Terminate Message Predicate" 
        terminate-message?))))
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

(@ "In addition to the normal format codes, there are also all of the
object ids and the object types that need to be converted and
translated. Let's start with a function that translates their oid
numbers into names."
 
(@c
(define (oid->typename id)
  (case id
    [20 'bigint]
    [1560 'bit]
    [16 'boolean]
    [603 'box]
    [17 'bytea]
    [1043 'character-varying]
    [18 'character]
    [650 'cidr]
    [710 'circle]
    [1082 'date]
    [701 'double-precision]
    [869 'inet]
    [23 'integer]
    [1186 'interval]
    [628 'line]
    [601 'lseg]
    [829 'macaddr]
    [790 'money]
    [1700 'numeric]
    [602 'path]
    [600 'point]
    [604 'polygon]
    [700 'real]
    [21 'smallint]
    [25 'text]
    [1082 'time]
    [1266 'time-with-time-zone]
    [1114 'timestamp]
    [1184 'timestamp-with-time-zone]
    [3615 'tsquery]
    [3614 'tsvector]
    [2970 'txid-snapshot]
    [2950 'uuid]
    [142 'xml]
    [else id]))
))

(@ "Now, we also want to have some sort of set of procedures for doing
the appropriate conversions for these files. The input to these
procedures will be the field description for the field as well as the 
field contents, which is either a bytevector or |#f|. The purpose of
this is then to convert this into something that is reasonably used in
Scheme. For example, all of the numeric types should be turned into
the appropriate numbers, and the string types should be turned into
strings. The time types should be times, and so forth. We will not be
doing all of them, right now, but there are a few that we can handle
right away. (XXX: Finish doing the rest of the data types.)"

(@c
(define (convert-pg-field desc contents)
  (let ([convert (lookup-pg-field-converter
                   (field-description-type-object-id desc))])
    (convert contents (field-description-format desc))))
))

(@ "In |convert-pg-field| above we use a |lookup-pg-field-converter|
procedure that returns a unary conversion procedure, which takes a
bytevector or |#f| and returns the apropriate Scheme version of the
value. Let's define this here."

(@c
(define (lookup-pg-field-converter type)
  (define-syntax converter
    (syntax-rules ()
      [(_ (bv fmt) exp)
       (lambda (bv fmt) (and bv exp))]))
  (case type
    [bigint (converter (bv fmt) (convert-pg-integer-value bv fmt 8))]
    [boolean convert-pg-boolean-value]
    [double-precision (converter (bv fmt) (convert-pg-float-value bv fmt 8))]
    [integer (converter (bv fmt) (convert-pg-integer-value bv fmt 4))]
    [real (converter (bv fmt) (convert-pg-float-value bv fmt 4))]
    [smallint (converter (bv fmt) (convert-pg-integer-value bv fmt 2))]
    [(bytea bit) (converter (bv fmt) bv)]
    [else
      (converter (bv fmt)
        (case fmt
          [(text) (bytevector->string bv (native-transcoder))]
          [else bv]))]))
))

(@ "Now that we have the general idea, we want to implement the
specifics for each individual data type that we might encounter. The
integer values are the first ones to consider. In each of these, if
they are text, well, we can just read them in as anything else using
|string->number| and things should work. If they are something like a
binary, then we probably need to read them in a bit differently."
 
(@c
(define (convert-pg-integer-value bv fmt size)
  (case fmt
    [(text) 
     (string->number (bytevector->string bv (native-transcoder)))]
    [(binary)
     ((case size
        [(1) bytevector-u8-ref]
        [(2) bytevector-s16-native-ref]
        [(4) bytevector-s32-native-ref]
        [(8) bytevector-s64-native-ref]
        [else (errorf 'convert-pg-integer-value
                "we should not be getting integer sizes like ~s"
                size)])
      bv 0)]
    [else 
      (warningf 'convert-pg-integer-value
        "unrecognized format type ~s, not doing any conversion"
        fmt)
      bv]))
))
 
(@ "We need to do something similar with floating point values, but
they require that we do things a little bit differently when it comes
to the binary files. At the moment, I am just going to ignore binary
files."
 
(@c
(define (convert-pg-float-value bv fmt size)
  (case fmt
    [(text) 
     (string->number (bytevector->string bv (native-transcoder)))]
    [else 
      (warningf 'convert-pg-float-value
        "unsupported format type ~s, not doing any conversion"
        fmt)
      bv]))
))
 
(@ "The boolean values convert is pretty straightforward, but we have
to assume that we might get either `t' or `true' and similarly for the
false values."
 
(@c
(define (convert-pg-boolean-value bv fmt)
  (case fmt
    [(text)
     (= 116 (bytevector-u8-ref bv 0))]
    [(binary)
     (= 1 (bytevector-u8-ref bv 0))]
    [else
      (warningf 'convert-pg-boolean-value
        "unsupported format type ~a, not converting"
        fmt)
      bv]))
))

(@* "Sending Messages to the Server"
"While we can print to ports in the above code, it is nice to have a
simple, low-level mechanism for sending messages to the server based
on a server connection object, defined below. This procedure takes
care of that:
 
\\medskip\\verbatim
(send-message conn msg)
|endverbatim
\\medskip
 
\\noindent
The |conn| should be a |postgresql-connection| record, and the message
should be a |postgresql-message| object. It will send the postgresql
message to the connection based on the output port defined there, and
raise an error if it has trouble doing so."
 
(@c
(define (send-message conn msg)
  (assert (postgresql-connection? conn))
  (assert (postgresql-message? msg))
  (unless (postgresql-connection-port conn)
    (error 'send-message "connection is not established"
      conn))
  (put-postgresql-message (postgresql-connection-port conn)
    msg))
))
        
(@* "Handling Incoming Messages"
"When we send a message defined above to some server, we have to be
ready to handle all types of messages coming back. The PostgreSQL
manual suggests that systems not depend on any specific messages being
returned. Instead, they suggest that systems should handle all types
of messages at any state for robustness. 
 
To facilitate this, this section defines a macro for describing
message loops. In these message loops, one specifies a name for the
loop, and a binary input port from which to read postgresql messages.
It then dispatches based on the type of the message.
 
\\medskip\\verbatim
(postgresql-message-loop name args msg port clause clause ...)
|endverbatim
\\medskip
 
\\noindent
The |name| above is the name of the loop. 
The |args| have the same form as a series of named let clauses. They 
will be associated along with the |name| in the same way that a named
let does. 
The |port| is a binary
input port. The |msg| is bound to the message that is read in.
The |clause|s have the following forms:
 
\\medskip\\verbatim
(message-predicate? expr expr ...)
((message-predicate? message-predicate? ...) expr expr ...)
(else expr expr ...)
|endverbatim
\\medskip
 
\\noindent
The |message-predicate?| should be some predicate that can handle
postgrseql messages." 
 
(@c
(define-syntax postgresql-message-loop
  (syntax-rules (%internal else)
    [(_ %internal msg (c ...) (else e1 e2 ...))
     (cond c ... (else e1 e2 ...))]
    [(_ %internal msg (c ...) [(p1? p2? ...) e1 e2 ...])
     (cond c ... 
       [(p1? msg) e1 e2 ...]
       [(p2? msg) e1 e2 ...] ...)]
    [(_ %internal msg (c ...) (pred? e1 e2 ...))
     (cond c ... [(pred? msg) e1 e2 ...])]
    [(_ %internal msg (c ...) [(p1? p2? ...) e1 e2 ...] rest ...)
     (postgresql-message-loop %internal msg
       (c ...
        [(p1? msg) e1 e2 ...]
        [(p2? msg) e1 e2 ...] ...)
       rest ...)]
    [(_ %internal msg (c ...) (pred? e1 e2 ...) rest ...)
     (postgresql-message-loop %internal msg
       (c ... [(pred? msg) e1 e2 ...]) rest ...)]
    [(_ name args msg port c1 c2 ...)
     (let name args
       (let ([msg (get-postgresql-message port)])
         (postgresql-message-loop %internal msg () c1 c2 ...)))]))
))
 
(@ "XXX: At some point, I need to write some test cases for this
thing."
 
(@c
(define (run-message-loop-tests) (void))
))
 
(@* "Handling startup and connections" 
"The client connects to the server by first sending a startup message
to the server and then waiting for an appropriate response. 
I will describe the handling of responses and listening to the server
later, but for now, let's talk about how a connection is initiated and
what happens at that point. 
 
This library defines a connection procedure which takes in a set of
parameter values for the connection request, and sends them off in a
startup message. The procedure itself has the following signature.
 
\\medskip\\verbatim
(postgresql-connect . params)
|endverbatim
 
\\medskip\\noindent
The procedure returns one value, and the |params| are
expected to be an association list mapping symbol keys to their values
in suitable fashion. The user is welcome to throw in any parameters
that he wants, but the following are specifically supported and used:
 
\\medskip
\\itemitem{server} A string representing the server address, it
defaults to ``localhost.''
\\itemitem{port} A number indicating the port, by default this is
5432. 
\\itemitem{user} (Mandatory) This field indicates the user for the
database.
\\itemitem{database} A string containing the name of the database. it
defaults to the username.
\\itemitem{password} A string that contains the password for
authentication. 
\\itemitem{server-parameters} An association list containing the run-time
parameters that the user wishes to set. [XXX: Currently not
implemented.]
\\medskip
 
\\noindent 
The startup message takes a list of parameters. To send the startup
message, we need to construct this message from the above received
parameters, but only after we have converted them to the appropriate
form and stripped out anything that we don't want. We use the other
parameters to create the connection. We return a 
|postgresql-connection| object upon a successful return."
 
(@> |Define postgresql-connect| (export postgresql-connect)
(define (postgresql-connect . params)
  (let ([params (@< |Parse parameters| params)])
    (define get (@< |Make postgresql-connect getter| params))
    (let-values ([(sock addr) (@< |Get socket and address| get)]
                 [(server-params) (@< |Get server parameters| get)])
      (let ([port (@< |Connect to server| sock addr)])
        (let ([res 
               (make-postgresql-connection sock addr port params)])
          (send-message res (make-startup-message server-params))
          (@< |Handle startup response| res get)
          res)))))
))
 
(@ "The parameters will come in as a series of symbols and values, but
we want to have them in an association list. We need to parse the
input to make sure that we have the appropriate pairs and convert them
into the right form."
 
(@> |Parse parameters| (capture params)
(assert (even? (length params)))
(fold-right
  (lambda (e s)
    (cond
      [(null? s) (list e)]
      [(pair? (car s)) (cons e s)]
      [else (cons (cons e (car s)) (cdr s))]))
  '()
  params)
))

(@ "It's convenient to just have a single get procedure that will grab
out the appropriate values from the input parameters without having to
specify too many things. To that end, the following |get| procedure
provides that functionality. It takes an optional argument that
indicates a default value to provide if the value is not defined in
the parameters (listed as |params| above). However, if this is not
provided, then it will raise an error if it cannot find the parameter
that was requested."

(@> |Make postgresql-connect getter| (capture params)
(lambda (x . maybe-opt)
  (let ([res (assq x params)])
    (or (and res (cdr res))
        (if (pair? maybe-opt)
            (car maybe-opt)
            (errorf 'postgresql-connect
              "~a is mandatory but you have not provided it"
              x)))))
))

(@ "When we make a connection, we need a socket that works. We should
start by creating an appropriate socket and address object that works
the way that we want. We won't make a connection to it right away.
Rather, we will do a bit more processing before we actually make that
final connection, so all we need to do is make sure that they are
created."

(@> |Get socket and address| (capture get)
(let ([server-name (get 'server "localhost")]
      [port (get 'port 5432)])
  (values
    (create-socket
      socket-domain/internet
      socket-type/stream
      socket-protocol/auto)
    (let-values ([(addrs ignore)
                  (get-address-info server-name port
                    socket-domain/internet
                    socket-type/stream
                    socket-protocol/auto)])
      (when (null? addrs)
        (error 'postgresql-connect "no addresses found"))
      (address-info-address (car addrs)))))
))

(@ "Once we actually have the server socket and address values, we
can make an attempt to connect to the server. In this, we're just 
trying to make the connection. It probably will work, but just in case
it doesn't, we need to make sure that we raise the right sort of error
and such. We use blocking sockets at the moment. We'll assume that the
errors raised by |connect-socket| is good enough for the moment.
Additionally, we should construct two values, an input and and output
porth, both binary, as our final value, since we will be using these
to do the majority of our communication with the server."

(@> |Connect to server| (capture sock addr)
(set-socket-nonblocking! sock #f)
(connect-socket sock addr)
(socket->input/output-port sock)
))
 
(@ "The server parameters are made up of the server parameters that
were explicitely given as run-time values in the initial connection
arguments, but also the user and database values. The problem here is
that the arguments to |postgresql-connect| have a mix of different
values in them, and it's possible that some of the values may have
been received as something other than strings, but the server
parameters that are passed through to the server in a packet must all
be strings. Thus, we have to create the server parameters by grabbing
all of the run-time values and converting them to strings, as well as
including the user and database values, also converted to strings. In
these cases, we are just going to use the standard string conversion
based on |DISPLAY| rather than anything else."
 
(@> |Get server parameters| (capture get)
(define (symbolify e)
  (unless (symbol? e)
    (errorf 'postgresql-connect
      "Parameter name ~s is not a symbol"
      e))
  e)
(define (stringify e)
  (if (string? e) e (format "~a" e)))
(fold-right
  (lambda (e s)
    (cons 
      (cons 
        (symbolify (car e)) 
        (stringify (cdr e)))
      s))
  '()
  (cons (cons 'user (get 'user))
    (let ([db (get 'database #f)]
          [rt-params (get 'server-parameters '())])
      (if db (cons (cons 'database db) rt-params) rt-params))))
))

(@ "We will define a specific postgresql connection record to hold the
connection information for a postgresql connection. These will include
the following:
 
\\medskip
\\itemitem{parameters} This is the argument received by
|postgresql-connect| so that one can query these parameters at a later
time. 
\\itemitem{server-socket} This is a |socket| object that is use to
connect to the server.
\\itemitem{server-address} This is a |socket-address| object that is
used in the connection to the server.
\\itemitem{input-port} This is the input port to the server, if the
server is no connected, then it will be |#f|, but if it is connected,
this will be a binary input-port.
\\itemitem{output-port} This is a binary output port to the server
based on the above |server-socket| object assuming that the server is
connected, and |#f| otherwise. 
\\itemitem{backend-pid} This is the pid value from any backend-data
messages that are received on this connection.
\\itemitem{backend-key} This is the key value from any
backend-key-data messages that are received.
\\medskip
 
\\noindent 
In the above record, the socket and socket address do not need to be
mutable, but the other fields do. We need the ports mutable
becuase they may be set to |#f| in the case that the server is
disconnected, the parameters if we get parameter status messages, and
the backend fields need to be mutable because we set them explicitly
after we have created the record, instead of during creation time."
 
(@c
(define-record-type postgresql-connection
  (fields socket address
    (mutable port)
    (mutable parameters)
    (mutable backend-pid) (mutable backend-key))
  (protocol 
    (lambda (n)
      (lambda (sock addr port param)
        (n sock addr port param #f #f)))))
))
 
(@ "When we deal with postgresql connections, it may happen that one
of the parameters is changed by the server. In this case, we want to
adjust the parameters that are stored in the postgresql connection
object. We use the following procedure to help us to do that."
 
(@c
(define (postgresql-connection-parameter-update! conn key val)
  (let* ([params (postgresql-connection-parameters conn)]
         [res (assq key params)])
    (if res
        (set-cdr! res val)
        (postgresql-connection-parameters-set! conn
          (cons (cons key val) params)))))
))
 
(@ "Once the startup message has been sent, we have to deal with a
number of various responses that the server could send back to us.
Either it
will require authentication or the client may be given a final
response to authentication immediately.
The final responses are one of two messages:
 
\\unorderedlist
\\li An Authentication Okay message,
\\li or, an Error Response message.
\\endunorderedlist
 
\\noindent
These messages will indicate whether the client can continue. If the
client receives the error message, the connection will have been
closed by the server.
 
The server may request that authentication be performed. There are a
number of authentication messages:
 
\\unorderedlist
\\li Kerberos V5 
\\li Clear text
\\li MD5
\\li SCM Credential
\\li GSS
\\li SSPI
\\li GSS Continue
\\endunorderedlist
 
\\noindent
At the moment, this client only supports clear text, unencrypted
communication. This is dangerous, but expedient. You can wrap the
connection, hopefully in some other SSL protocols, but for the moment,
this is all that we are supporting until we can get some time to do
the others correctly. 
 
When the client receives an authentication request that it cannot
handle, it will immediately close the connection. After an 
authentication okay message has been sent, the client must still
process a series of messages, such as backend key, notice, parameter
status, and error messages. An error response is treated the same
before or after the authentication okay message. The parameter status
and other messages change the connection state in the connection
object. Finally, after these messages are processed, if everything
goes okay, a ready for query message should be sent by the server, in
which case we are all done."

(@> |Handle startup response| (capture res get)
(assert (postgresql-connection? res))
(postgresql-message-loop continue () msg 
    (postgresql-connection-port res)
  [authentication-okay-message? (continue)]
  [ready-for-query-message? (void)]
  [error-response-message?
   (raise
     (build-postgresql-condition 
       (error-response-message-codes msg)
       (make-error)
       (make-who-condition 'postgresql-connect)
       (make-message-condition "postgresql server error")))]
  [authentication-cleartext-password-message?
   (send-message res (make-password-message (get 'password)))
   (continue)]
  [authentication-message?
   (close-socket (postgresql-connection-socket res))
   (errorf 'postgresql-connect
     "server requested unsupported authentication type ~a, closing connection"
     (cond
       [(authentication-kerberos-v5-message? msg) "Kerberos V5"]
       [(authentication-md5-password-message? msg) "MD5 Password"]
       [(authentication-scm-credential-message? msg) "SCM Credential"]
       [(authentication-gss-message? msg) "GSS"]
       [(authentication-sspi-message? msg) "SSPI"]
       [(authentication-gss-continue-message? msg) "GSS Continue"]
       [else "Unknown"]))]
  [backend-key-data-message?
   (postgresql-connection-backend-pid-set! res
     (backend-key-data-message-pid msg))
   (postgresql-connection-backend-key-set! res
     (backend-key-data-message-key msg))
   (continue)]
  [parameter-status-message?
   (postgresql-connection-parameter-update! res
     (parameter-status-message-name msg)
     (parameter-status-message-value msg))
   (continue)]
  [notice-response-message? 
   (raise-continuable
     (build-postgresql-condition
       (notice-response-message-codes msg)
       (make-warning)
       (make-who-condition 'postgresql-connect)
       (make-message-condition "postgresql server notice")))
   (continue)]
  [else
    (warning 'postgresql-connect "unhandled server response" msg)
    (continue)])
))


(@ "Now that we have constructed everything related to making a 
postgresql connection, let's define it to be visible to the rest of
the world."
 
(@c
(@< |Define postgresql-connect|)
))
 
(@ "Now that we can connect to our database, it's probably a good idea
that we also know how to terminate from it gracefully. This consists
of sending a termination message to the server and closing the
connection."
 
(@c
(define (postgresql-terminate-connection conn)
  (assert (postgresql-connection? conn))
  (send-message conn (make-terminate-message))
  (let ([port (postgresql-connection-port conn)])
    (when port 
      (close-port port)
      (postgresql-connection-port-set! conn #f))))
))
 
(@ "The last sort of thing that is very similar to a normal startup
sequence is the cancelation protocol. If someone wants to cancel a
transaction that is occuring, he will send a cancellation message as
the first message of a new connection to the server. This requires a
bit of extra work, but it is done for efficiency on the server side.
Let's define a procedure for handling this."
 
(@c
(define (postgresql-cancel-request conn)
  (assert (postgresql-connection? conn))
  (let ([pid (postgresql-connection-backend-pid conn)]
        [key (postgresql-connection-backend-key conn)]
        [saddr (postgresql-connection-address conn)]
        [s (create-socket
             socket-domain/internet 
             socket-type/stream
             socket-protocol/auto)])
    (let-values ([(in out) (@< |Connect to server| s saddr)])
      (put-postgresql-message out 
        (make-cancel-request-message pid key))
      (close-port in) 
      (close-port out))))
))

(@* "Handling simple queries"
"Simple queries are ones that are not broken up and parsed. They are
sent as a single string and then the results are parsed by the
frontend. 
We are going to define a single record type for this, which helps us
to think about query results. A query result is a formatting
associated with a list of query rows. "
 
(@c
(define-record-type query-result
  (fields format rows))
))
 
(@ "Let's see if we can get the proper procedure for a simple query.
It will take a single string for the query."
 
(@> |Define postgresql-simple-query| (export postgresql-simple-query)
(define (postgresql-simple-query conn query)
  (assert (postgresql-connection? conn))
  (assert (string? query))
  (let ([msg (make-query-message query)])
    (send-message conn msg)
    (@< |Handle simple query response| conn)))
))
 
(@ "Our message loop will handle all the conceivalbe messages that we
could be receiving.
In this case, there are a few different situations that we
handle and deal with explicitly:
 
\\unorderedlist
\\li The server could encounter an error in any case, in which case we
fail with an error.
\\li The server could send us any number of incidental messages, which
we will handle by printing or updating the state of the system
somehow, and then continuing.
\\li We could receive a command complete message before processing any
row data or row description messages, which means that the query was
purely side-effecting and we don't have to return anything.
\\li We could receive some copy in or copy out responses, which are
things that we currently don't support. In this case, we just raise an
error. XXX: This needs to be fixed.
\\li We might encounter the empty query response, which means that the
query which we sent was entirely empty. In this case, we'll want to
raise a continuable error indicating that this was the case and let
them go on their merry way. 
\\li We might receive a single row description and set of data row
messages. This means that we have only one result. In this case, we
return a single record indicating this sort of thing.
\\li We might have a number of row description messages and
accompanying data row messages. In this case, a list of records about
these responses should be returned. 
\\endunorderedlist
 
\\noindent
The important part about the above is that regardless of whether we
have an error or an empty response, we will always receive a ready for
query message after that. This means that we need to store the errors
and continue processing. Eventually, we will get the ready for query
message, and then we can do the right thing based on what messages we
have collected. This means that we are going to keep track of some
state during our message processing loop.
 
\\medskip
\\itemitem{empty?} This flag will indicate whether we have received an
empty query response.
\\itemitem{error} This will be false unless an error response message
was given, in which case we will store the error condition
\\itemitem{desc} This will be the current query row
description.
\\itemitem{rows} These will be the current row values.
information.
\\itemitem{queries} This will start false, but 
if
we receive two row descriptions, this becomes a list of the previous
query results.
\\medskip
 
\\noindent
In this loop, we want to just maintain the state, and avoid doing
any of the final processing. That means that I'll be doing the other
work outside of here in another chunk. The main job here is just to
control the collection and appropriate construction of the states
above that we need for the final query response."
 
(@> |Handle simple query response| (capture conn)
(postgresql-message-loop continue
    ([empty? #f] [err #f] [desc #f] [rows #f] [queries #f])
    msg (postgresql-connection-port conn)
  [ready-for-query-message? 
   (@< |Construct simple query response| 
       empty? err desc rows queries)]
  [empty-query-response-message?
   (continue #t err desc rows queries)]
  [error-response-message?
   (continue empty? msg desc rows queries)]
  [command-complete-message? 
   (continue empty? err desc rows queries)]
  [(copy-in-response-message? copy-out-response-message?)
   (error 'postgresql-simple-query
     "copy operations are not supported right now")]
  [row-description-message? 
   (continue empty? err msg '()
     (if desc
         (cons (make-query-result desc (reverse rows)) 
               queries)
         '()))]
  [data-row-message? 
   (continue empty? err desc (cons msg rows) queries)]
  [notice-response-message?
   (raise-continuable
     (build-postgresql-condition
       (notice-response-message-codes msg)
       (make-warning)
       (make-who-condition 'postgresql-simple-query)
       (make-message-condition "PostgreSQL server notice")))
   (continue empty? err desc rows queries)]
  [else (warning 'postgresql-simple-query "unhandled message" msg)
    (continue empty? err desc rows queries)])
))
 
(@ "Now let's see what we need to do to actually construct the final
response. We should address each state variable in order to make sure
that we get the empty response first, which is a continuable error,
and then the error if there is one, which is a non-continuable error,
followed by the construction of the query results if there are any,
otherwise returning void."
 
(@> |Construct simple query response|
    (capture empty? err desc rows queries)
(when empty?
  (raise-continuable 
    (condition
      (make-warning)
      (make-empty-query-string-condition)
      (make-who-condition 'postgresql-simple-query)
      (make-message-condition "empty query"))))
(when err
  (raise
    (build-postgresql-condition
      (error-response-message-codes err)
      (make-who-condition 'postgresql-simple-query)
      (make-message-condition "postgresql server error")
      (make-error))))
(when desc
  (let ([res (make-query-result 
               (row-description-message-fields desc)
               (convert-rows desc (reverse rows)))])
    (if (null? queries)
        res
        (reverse (cons res queries)))))
))

(@ "When we make the query result, we convert the rows based on their
field descriptions. This is accomplished with the help of the
|convert-rows| procedure defined here."
 
(@c
(define (convert-rows desc rows)
  (define (get-field-description i)
    (vector-ref (row-description-message-fields desc) i))
  (define (convert-row row)
    (let* ([vlen (vector-length row)]
           [vres (make-vector vlen)])
      (do ([i 0 (1+ i)]) [(= i vlen) vres]
        (vector-set! vres i
          (convert-pg-field (get-field-description i)
            (vector-ref row i))))))
  (map convert-row (map data-row-message-columns rows)))
))
 
(@ "In the above, we use a special condition to handle empty string
conditions. We also want to define some sort of condition for
all of the various errors that we can encounter and their fields.
We can start this definition with a parent condition
|postgresql-condition| that houses all of the postgresql conditions."
 
(@c
(define-condition-type &postgresql &condition
  make-postgresql-condition postgresql-condition?)
))

(@ "Once we have the basic housing condition, we need some conditions
for the various types of situations that we might encounter. We need a
condition type that corresponds to an empty query string response, but
we also need a condition that encapsulates the state or information
returned during error and notice messages that is not captures by
existing R6RS messages. Let's start with the emtpy query string."

(@c
(define-condition-type &empty-query-string &postgresql
  make-empty-query-string-condition empty-query-string-condition?)
))

(@ "The following table indicates the particular types of fields that
may be returned by PostgreSQL error or notice messages, and the
appropriate conditions that encapsulate them. Some of them may be R6RS
conditions that are predefined, and others may be new.

$$\\vbox{
\\offinterlineskip
\\halign{
\\strut {\\rm #} & \\vrule \\quad # \\hfill\\cr
{\\bf Field} & {\\bf Condition Name} \\cr
\\noalign{\\hrule}
Severity & |&postgresql-severity| \\cr
Code & |&postgresql-code| \\cr
Message & |&postgresql-message| \\cr
Detail & |&postgresql-detail| \\cr
Hint & |&postgresql-hint| \\cr
Position & |&postgresql-position| \\cr
Internal Position & |&postgresql-internal-position| \\cr
Internql Query & |&postgresql-internal-query| \\cr
Where & |&postgresql-where| \\cr
File & |&postgresql-file| \\cr
Line & |&postgresql-line| \\cr
Routine & |&postgresql-routine| \\cr
}
}$$

\\noindent Note that the above are meant to be related to the
postgresql server messages that are sent. Additional conditions may be
created that assist in understanding what happens when dealing with
client side problems, or to give the client's perspective on the
error. There may be a mix of conditions in any conditions that are
finally returned."
    
(@c
(define-condition-type &postgresql-severity &postgresql
  make-postgresql-severity-condition postgresql-severity-condition?
  (severity postgresql-severity))
(define-condition-type &postgresql-code &postgresql
  make-postgresql-code-condition postgresql-code-condition?
  (code postgresql-code))
(define-condition-type &postgresql-message &postgresql
  make-postgresql-message-condition postgresql-message-condition?
  (message postgresql-message-condition-message))
(define-condition-type &postgresql-detail &postgresql
  make-postgresql-detail-condition postgresql-detail-condition?
  (detail postgresql-detail))
(define-condition-type &postgresql-hint &postgresql
  make-postgresql-hint-condition postgresql-hint-condition?
  (hint postgresql-hint))
(define-condition-type &postgresql-position &postgresql
  make-postgresql-position-condition postgresql-position-condition?
  (position postgresql-position))
(define-condition-type &postgresql-internal-position &postgresql
  make-postgresql-internal-position-condition
  postgresql-internal-position-condition? 
  (position postgresql-internal-position))
(define-condition-type &postgresql-internal-query &postgresql
  make-postgresql-internal-query-condition
  postgresql-internal-query-condition?
  (query postgresql-internal-query))
(define-condition-type &postgresql-where &postgresql
  make-postgresql-where-condition postgresql-where-condition?
  (where postgresql-where))
(define-condition-type &postgresql-file &postgresql
  make-postgresql-file-condition postgresql-file-condition?
  (file postgresql-file))
(define-condition-type &postgresql-line &postgresql
  make-postgresql-line-condition postgresql-line-condition?
  (line postgresql-line))
(define-condition-type &postgresql-routine &postgresql
  make-postgresql-routine-condition postgresql-routine-condition? 
  (routine postgresql-routine))
))

(@ "When we have these conditions, it's not exactly an easy thing to
just make them up and constantly create them on the fly. Instead, we
have to take something which is in the form of an association list of
messages, which is what the postgresql message notice and error
packets encapsulate, and somehow turn this into a reasonable condition
object that can be sent upwards using |raise|. We also have to
consider that we probably want to use other conditions to augment the
information contained in the server message, so we should probably
have some way of handling that as well.

My interpretation of how this should be done is to have a procedure
that takes an association list of the error and notice fields, and
an additional set of condition objects that should be used to create a
final compound condition object.

\\medskip\\verbatim
build-postgresql-condition : field-alist . extra-conditions
|endverbatim
\\medskip

\\noindent |build-postgresql-condition| returns a condition object
that contains all of the extra conditions as well as the conditions
that correspond to the fields listed in the association list, but none
of the conditions that are not listed in the association list."

(@c
(define (build-postgresql-condition fields . extras)
  (apply condition
    (append 
      (map field->condition fields)
      extras)))
(define (field->condition field)
  (define (-> make) (make (cdr field)))
  (case (car field)
    [severity (-> make-postgresql-severity-condition)]
    [code (-> make-postgresql-code-condition)]
    [message (-> make-postgresql-message-condition)]
    [detail (-> make-postgresql-detail-condition)]
    [hint (-> make-postgresql-hint-condition)]
    [position (-> make-postgresql-position-condition)]
    [internal-position (-> make-postgresql-internal-position-condition)]
    [internal-query (-> make-postgresql-internal-query-condition)]
    [where (-> make-postgresql-where-condition)]
    [file (-> make-postgresql-file-condition)]
    [line (-> make-postgresql-line-condition)]
    [routine (-> make-postgresql-routine-condition)]))
))

(@ "Now that we have finished the definition of
|postgresql-simple-query|, let's throw it into the top-level."
 
(@c
(@< |Define postgresql-simple-query|)
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
  (run-parse-complete-tests)
  (run-parse-tests)
  (run-password-tests)
  (run-portal-suspended-tests)
  (run-query-tests)
  (run-ready-for-query-tests)
  (run-row-description-tests)
  (run-ssl-request-tests)
  (run-startup-tests)
  (run-sync-tests)
  (run-terminate-tests)
  (run-message-loop-tests)
  (test-end "Postgresql"))
))
 
(@* "Register message handlers"
"Let's register the message handlers now."
 
(@c
(@< |Register message readers|)
))

)
