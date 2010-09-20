#!chezscheme
(@chezweb)

"\\centerline{
  \\titlef Socket Ports for Chez Scheme}
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

(@l "Normal socket operations [[send-to-socket]] and
[[receive-from-socket]] are not generally useful when dealing with
connected sockets that will have simple textual data.  Instead, it is
more useful to convert these to ports.  The following library provides
Chez Scheme specific features for converting a socket into a port."

(arcfide sockets socket-ports)
(export socket->port socket->input-port socket->output-port)
(import 
	(rnrs base) 
	(arcfide sockets)
	(only (chezscheme) 
		open-fd-input-port 
		open-fd-output-port
		open-fd-input/output-port))

(@* "Implementation"
"Chez Scheme already provides most of the features necessary, so the actual incantation is 
quite simple.

\\medskip\\verbatim
(socket->port socket b-mode transcoder)
|endverbatim
\\medskip

\\noindent 
|socket->port| returns two values, an input port and an
output port.  The |b-mode| and |transcoder| correspond to
their R6RS interpretations for port creators.  There are also
analagous |socket->input-port| and |socket->output-port|
operations, which may be used if separate |b-mode| and
|transcoder| values should be used for the input and output ports."

(@c
(define (socket->port	sock . args)
	(let ([fd (socket-fd sock)])
		(values
			(apply open-fd-input-port (cons fd args))
			(apply open-fd-output-port (cons fd args)))))

(define (socket->input-port sock . args)
	(apply open-fd-input-port (cons (socket-fd sock) args)))

(define (socket->output-port sock . args)
	(apply open-fd-output-port (cons (socket-fd sock) args)))
))

)