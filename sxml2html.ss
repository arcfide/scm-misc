#! /usr/bin/env scheme-script
(import 
	(rnrs base)
	(except (rnrs io ports) current-output-port)
	(rnrs control)
	(rnrs programs)
	(only (chezscheme) 
		command-line-arguments 
		parameterize 
		path-root
		printf
		current-output-port)
	(oleg sxml-to-html))

(define html44-decl 
"
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
        \"http://www.w3.org/TR/html4/strict.dtd\">
")

(define (build-html sxml-file)
	(let* (
			[html-file (string-append (path-root sxml-file) ".html")]
			[oport 
				(open-file-output-port 
					html-file 
					(file-options no-fail) 
					(buffer-mode block) 
					(make-transcoder (utf-8-codec)))])
		(put-string oport html44-decl)
		(parameterize ([current-output-port oport])
			(sxml->html 
				(let (
						[input 
							(open-file-input-port
								sxml-file
								(file-options)
								(buffer-mode block)
								(make-transcoder (utf-8-codec)))])
					(let ([res (get-datum input)])
						(close-port input)
						res))))
		(close-port oport)))

(when (null? (command-line-arguments))
	(printf "~a : <sxml-file> <sxml-file> ...\n" (car (command-line)))
	(exit 1))

(for-each build-html (command-line-arguments))