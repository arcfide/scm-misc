#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Socket Ports for R6RS}
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
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.\\par
"

(@l "The following is a simple socket ports abstraction for R6RS.  It
allows one to convert a socket to a port and operate on it at that
level.  However, there are a number of caveats.  Firstly, this code is
neither the most reliable nor the most efficient, so if possible, use
an optimized implementation of this library designed specifically with
your Scheme's features in mind.

Beyond this, one should not mix using ports that correlate to sockets
with the usual socket operations.  In order to use ports in R6RS, we
mark the socket as blocking, and this can make your life difficult on
some of the other operations if you assume that the sockets are
non-blocking, which is the usual case.  We do this because R6RS
operations are blocking.  and this makes our lifes easier.  This will
be fixed later when a better abstraction is written on top of the
sockets."

(arcfide socket-ports)
(export socket->port socket->input-port socket->output-port)
(import
	(rnrs base)
	(rnrs control)
	(rnrs bytevectors)
	(rnrs io ports)
	(only (chezscheme) format foreign-procedure)
	(arcfide sockets))

(@* "Implementation"
"\\medskip\\verbatim
(socket->port socket buffer-mode transcoder)
|endverbatim
\\medskip

\\noindent
This creates an input and output port. It ignores the given buffer mode, 
at the moment, but uses the transcoder if provided.
|socket->input-port| and |socket->output-port| operate the same, 
but return either an input or an output port, respectively.

{\\it XXX: There must be a more efficient way to implement the 
|read!| and |write!| procedures here.}"

(@> |Define socket port procedures|
(export socket->port socket->input-port socket->output-port)
(define (make-reader sock)
	(@< |Read Socket| sock))

(define (make-writer sock)
	(@< |Write Socket| sock))

(define (make-closer sock)
	(@< |Close Socket| sock))

(define (socket-name sock) (format "Socket ~a" (socket-fd sock)))

(define socket->port
	(case-lambda
		[(sock)
			(let ([name (socket-name sock)])
				(set-socket-nonblocking! sock #f)
				(values
					(make-custom-binary-input-port name
						(make-reader sock) #f #f (make-closer sock))
					(make-custom-binary-output-port name
						(make-writer sock) #f #f (make-closer sock))))]
		[(sock b-mod) (socket->port sock)]
		[(sock b-mode transcoder)
			(let-values ([(in out) (socket->port sock)])
				(values 
					(transcoded-port in transcoder)
					(transcoded-port out transcoder)))]))

(define (make-socket-converter make-port operator)
	(define maker
		(case-lambda
			[(sock)
				(set-socket-nonblocking! sock #f)
				(make-port (socket-name sock) (operator sock) #f #f (make-closer sock))]
			[(sock b) (maker sock)]
			[(sock b transcoder) (transcoded-port (maker sock) transcoder)]))
	maker)

(define socket->input-port 
	(make-socket-converter make-custom-binary-input-port make-reader))
(define socket->output-port
	(make-socket-converter make-custom-binary-output-port make-writer))
))

(@ "We use the |read(2)| procedure to handle the reading. 
Since the system |read| procedure does not take a position, 
whereas the R6RS |read!| procedure is expected to take a 
position, we have to use another intermediate vector, for the 
moment."

(@> |Read Socket| (capture sock)
(let ([$read (foreign-procedure "read" (fixnum string fixnum) fixnum)])
	(lambda (bv s c)
		(let ([buf (make-bytevector c)])
			(let ([res ($read (socket-fd sock) buf c)])
				(bytevector-copy! buf 0 bv s res)
				res))))
))

(@ "Writing works in mostly the same way, 
except that the R6RS |write!| differs from the normal 
|write(2)| system call because a zero count write sends 
EOF to the file. "

(@> |Write Socket| (capture sock)
(let ([$write (foreign-procedure "write" (fixnum string fixnum) fixnum)])
	(lambda (bv s c)
		(let ([buf (make-bytevector c)])
			(bytevector-copy! bv s buf 0 c)
			(let ([res ($write (socket-fd sock) buf c)])
				(cond
					[(= -1 res) (socket-error 'socket->port 'write)]
					[(negative? res) (error 'socket-write "Unknown foreign return value.")]
					[else res])))))
))

(@ "Closing a socket is the simple matter of using |close-socket|."

(@> |Close Socket| (capture sock)
(lambda () (close-socket sock))
))

(@ "Now we define these procedures at the top-level."
(@c
(@< |Define socket port procedures|)
))
)