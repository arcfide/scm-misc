;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatibility file for Portable Sockets
;;; Version: 2.0
;;; 
;;; Copyright (c) 2009 Aaron W. Hsu <arcfide@sacrideo.us>
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

#!chezscheme
(library (arcfide sockets compat)
  (export
    (rename (runtime-windows? windows?))
    void
    $error-again $error-in-progress $error-would-block #;$file-get-flag
    $file-set-flag $format-message-allocate-buffer
    $format-message-from-system $ipproto-ip $ipproto-raw $ipproto-tcp
    $ipproto-udp $option-non-blocking $socket-error $sol-socket
    %ai/canonname %ai/numerichost %ai/passive %msg/dont-route %msg/dont-wait
    %msg/out-of-band %msg/peek %msg/wait-all %shutdown/read
    %shutdown/read&write %shutdown/write %socket-domain/internet
    %socket-domain/internet-v6 %socket-domain/local %socket-domain/unix
    %socket-type/datagram %socket-type/random %socket-type/raw
    %socket-type/sequence-packet %socket-type/stream %somaxconn af-inet
    af-unix invalid-socket size-of/addr-in size-of/addr-un size-of/addrinfo
    size-of/integer size-of/ip size-of/pointer size-of/port size-of/protoent
    size-of/sa-family size-of/size-t size-of/sockaddr-in size-of/sockaddr-un
    size-of/socklen-t size-of/wsa-data unix-max-path
    call-with-errno errno-message
    $getaddrinfo $gai_strerror $socket $getprotoent $getprotobyname 
    $getprotobynumber $setprotoent $endprotoent $bind $listen 
    $accept $connect $close $shutdown $sendto $recvfrom $getsockopt
    $setsockopt $fcntl
    $accept-blocking $connect-blocking 
    $recvfrom-blocking $sendto-blocking
    foreign-free foreign-alloc
    make-foreign-unix-address foreign-unix-address-path
    make-foreign-ipv4-address foreign-ipv4-address-ip
    foreign-ipv4-address-port
    make-foreign-pointer foreign-address-info-canonical-name
    foreign-pointer-value make-foreign-address-info
    foreign-address-info-next foreign-address-info-domain
    foreign-address-info-type foreign-address-info-protocol
    foreign-address-info-address foreign-address-info-address-length
    foreign-protocol-entry-name
    foreign-protocol-entry-aliases
    foreign-protocol-entry-protocol
    make-foreign-size-buffer foreign-size-buffer-value
    %set-blocking)
  (import (chezscheme) (arcfide ffi-bind))

(define (runtime-windows?) (memq (machine-type) '(i3nt it3nt)))
(meta define (windows?) (memq (machine-type) '(i3nt ti3nt)))
(meta-cond
  [(windows?)
   (define-foreign-values "socket-ffi-values.dll" 
     (__cdecl "_get_ffi_value")
      int
     $error-again $error-in-progress $error-would-block #;$file-get-flag
     $file-set-flag $format-message-allocate-buffer
     $format-message-from-system $ipproto-ip $ipproto-raw $ipproto-tcp
     $ipproto-udp $option-non-blocking $socket-error $sol-socket
     %ai/canonname %ai/numerichost %ai/passive %msg/dont-route %msg/dont-wait
     %msg/out-of-band %msg/peek %msg/wait-all %shutdown/read
     %shutdown/read&write %shutdown/write %socket-domain/internet
     %socket-domain/internet-v6 %socket-domain/local %socket-domain/unix
     %socket-type/datagram %socket-type/random %socket-type/raw
     %socket-type/sequence-packet %socket-type/stream %somaxconn af-inet
     af-unix invalid-socket size-of/addr-in size-of/addr-un size-of/addrinfo
     size-of/integer size-of/ip size-of/pointer size-of/port size-of/protoent
     size-of/sa-family size-of/size-t size-of/sockaddr-in size-of/sockaddr-un
     size-of/socklen-t size-of/wsa-data unix-max-path)]
  [else
    (define-foreign-values "socket-ffi-values.so" "get_ffi_value" int
      $error-again $error-in-progress $error-would-block #;$file-get-flag
      $file-set-flag $format-message-allocate-buffer
      $format-message-from-system $ipproto-ip $ipproto-raw $ipproto-tcp
      $ipproto-udp $option-non-blocking $socket-error $sol-socket
      %ai/canonname %ai/numerichost %ai/passive %msg/dont-route %msg/dont-wait
      %msg/out-of-band %msg/peek %msg/wait-all %shutdown/read
      %shutdown/read&write %shutdown/write %socket-domain/internet
      %socket-domain/internet-v6 %socket-domain/local %socket-domain/unix
      %socket-type/datagram %socket-type/random %socket-type/raw
      %socket-type/sequence-packet %socket-type/stream %somaxconn af-inet
      af-unix invalid-socket size-of/addr-in size-of/addr-un size-of/addrinfo
      size-of/integer size-of/ip size-of/pointer size-of/port size-of/protoent
      size-of/sa-family size-of/size-t size-of/sockaddr-in size-of/sockaddr-un
      size-of/socklen-t size-of/wsa-data unix-max-path)])
(meta-cond
  [(windows?)
   (define errno (foreign-procedure "WSAGetLastError" () int))
   (define errno-message
     (let ([$format-message (foreign-procedure __stdcall "FormatMessageA"
                              (unsigned-32 uptr unsigned-32 unsigned-32
                               uptr unsigned-32 uptr)
                              unsigned-32)]
           [$local-free 
             (foreign-procedure __stdcall "LocalFree" (uptr) void)])
       (lambda (num)
         (let* ([ptr (make-foreign-pointer)]
                [ret-res ($format-message 
                           (bitwise-xor 
                             $format-message-allocate-buffer
                             $format-message-from-system)
                           0 num 0 ptr 0 0)]
                [res (and (not (zero? ret-res))
                          (get-foreign-string 
                            (foreign-pointer-value ptr)))])
           ($local-free (foreign-pointer-value ptr))
           (foreign-free ptr)
           res))))
   (define (call-with-errno thunk receiver)
     (call-with-values
       (lambda () 
         (with-interrupts-disabled 
           (let ([v (thunk)]) (values v (errno)))))
       receiver))]
  [else (import (arcfide errno))])
(define (unsupported-feature feature)
  (error feature "this feature is not supported on this platform"))

(meta-cond 
  [(windows?)
   (define $socket 
     (foreign-procedure __stdcall "socket" (fixnum fixnum fixnum) fixnum))
   (define $getaddrinfo
     (foreign-procedure __stdcall "getaddrinfo" (string string uptr uptr) int))
   (define $getprotobyname 
     (foreign-procedure __stdcall "getprotobyname" (string) uptr))
   (define $getprotobynumber
     (foreign-procedure __stdcall "getprotobynumber" (fixnum) uptr))
   (define $bind
     (foreign-procedure __stdcall "bind" (fixnum uptr fixnum) fixnum))
   (define $listen
     (foreign-procedure __stdcall "listen" (fixnum fixnum) fixnum))
   (define $accept
     (foreign-procedure __stdcall "accept" (fixnum uptr uptr) fixnum))
   (define $connect
     (foreign-procedure __stdcall "connect" (fixnum uptr fixnum) fixnum))
   (define $shutdown
     (foreign-procedure __stdcall "shutdown" (fixnum fixnum) fixnum))
   (define $sendto
     (foreign-procedure __stdcall "sendto" 
       (fixnum u8* fixnum fixnum uptr fixnum) 
       fixnum))
   (define $recvfrom
     (foreign-procedure __stdcall "recvfrom" 
       (fixnum u8* fixnum fixnum uptr uptr)
       fixnum))
   (define $getsockopt
     (foreign-procedure __stdcall "getsockopt" (int int int uptr uptr) int))
   (define $setsockopt
     (foreign-procedure __stdcall "setsockopt" (int int int uptr int) int))
      (define ($getprotoent)
     (unsupported-feature '$getprotoent))
   (define ($setprotoent)
     (unsupported-feature '$setprotoent))
   (define ($endprotoent)
     (unsupported-feature '$endprotoent))
   (define $gai_strerror errno-message)
   (define $close
     (foreign-procedure __stdcall "closesocket" (unsigned) int))
   (define $fcntl
     (foreign-procedure __stdcall "ioctlsocket" (unsigned unsigned unsigned) int))]
  [else
    (define $socket 
      (foreign-procedure "socket" (fixnum fixnum fixnum) fixnum))
    (define $getaddrinfo
      (foreign-procedure "getaddrinfo" (string string uptr uptr) int))
    (define $getprotobyname 
      (foreign-procedure "getprotobyname" (string) uptr))
    (define $getprotobynumber
      (foreign-procedure "getprotobynumber" (fixnum) uptr))
    (define $bind
      (foreign-procedure "bind" (fixnum uptr fixnum) fixnum))
    (define $listen
      (foreign-procedure "listen" (fixnum fixnum) fixnum))
    (define $accept
      (foreign-procedure "accept" (fixnum uptr uptr) fixnum))
    (define $connect
      (foreign-procedure "connect" (fixnum uptr fixnum) fixnum))
    (define $shutdown
      (foreign-procedure "shutdown" (fixnum fixnum) fixnum))
    (define $sendto
      (foreign-procedure "sendto" 
        (fixnum u8* fixnum fixnum uptr fixnum) 
        fixnum))
    (define $recvfrom
      (foreign-procedure "recvfrom" 
        (fixnum u8* fixnum fixnum uptr uptr)
        fixnum))
    (define $getsockopt
      (foreign-procedure "getsockopt" (int int int uptr uptr) int))
    (define $setsockopt
      (foreign-procedure "setsockopt" (int int int uptr int) int))
    (define $getprotoent 
      (foreign-procedure "getprotoent" () uptr))
    (define $setprotoent
      (foreign-procedure "setprotoent" (boolean) void))
    (define $endprotoent
      (foreign-procedure "endprotoent" () void))
    (define $gai_strerror
      (foreign-procedure "gai_strerror" (int) string))
    (define $close
      (foreign-procedure "close" (fixnum) fixnum))
    (define $fcntl
      (foreign-procedure "fcntl" (fixnum fixnum fixnum) fixnum))])
(define $accept-blocking
  (foreign-procedure "accept_block" (fixnum uptr uptr) int))
(define $connect-blocking
  (foreign-procedure "connect_block" (fixnum uptr fixnum) int))
(define $sendto-blocking
  (foreign-procedure "sendto_block" 
    (fixnum u8* fixnum fixnum uptr fixnum) 
    int))
(define $recvfrom-blocking
  (foreign-procedure "recvfrom_block" 
    (fixnum u8* fixnum fixnum uptr uptr)
    int))
(define make-foreign-unix-address 
  (let ([$strcpy (foreign-procedure "strcpy" (uptr string) void)])
    (lambda (path)
      (let ([res (foreign-alloc size-of/sockaddr-un)]
            [path-len (string-length path)])
        (assert (< path-len unix-max-path))
        (foreign-set! 'unsigned-short res 0 af-unix)
        ($strcpy (+ res size-of/sa-family) path)
        res))))
    
(define foreign-unix-address-path
  (let ([$strncpy (foreign-procedure "strncpy" (u8* uptr fixnum) string)])
    (lambda (addr)
      ($strncpy (make-bytevector unix-max-path 0) 
                (+ addr size-of/sa-family)
                unix-max-path))))
(define (make-foreign-ipv4-address port ip)
  (let ([res (foreign-alloc size-of/sockaddr-in)])
    (foreign-set! 'unsigned-short res 0 af-inet)
    (foreign-set! 'unsigned-16 
                  res 
                  (foreign-sizeof 'unsigned-short)
                  (host->network/u16 port))
    (foreign-set! 'unsigned-32
                  res
                  (+ (foreign-sizeof 'unsigned-short)
                     (foreign-sizeof 'unsigned-16))
                  (host->network/u32 ip))
    res))
    
(define (foreign-ipv4-address-ip addr)
  (network->host/u32
    (foreign-ref 'unsigned-32 
                 addr
                 (+ (foreign-sizeof 'unsigned-short) 
                    (foreign-sizeof 'unsigned-16)))))

(define (foreign-ipv4-address-port addr)
  (network->host/u16
    (foreign-ref 'unsigned-16
                 addr
                 (foreign-sizeof 'unsigned-short))))

(define host->network/u16
  (if (eq? (native-endianness) (endianness big))
      (lambda (x) x)
      (lambda (x)
        (let ([buf (make-bytevector 2)])
          (bytevector-u16-set! buf 0 x (endianness big))
          (bytevector-u16-ref buf 0 (native-endianness))))))

(define host->network/u32
  (if (eq? (native-endianness) (endianness big))
      (lambda (x) x)
      (lambda (x)
        (let ([buf (make-bytevector 4)])
          (bytevector-u32-set! buf 0 x (endianness big))
          (bytevector-u32-ref buf 0 (native-endianness))))))
          
(define network->host/u32 host->network/u32)
(define network->host/u16 host->network/u16)
(define (make-foreign-pointer)
  (foreign-alloc (foreign-sizeof 'void*)))

(define (foreign-pointer-value x)
  (foreign-ref 'void* x 0))
  
(define family-offset (foreign-sizeof 'int))
(define type-offset (+ family-offset (foreign-sizeof 'int)))
(define proto-offset (+ type-offset (foreign-sizeof 'int)))
(define addrlen-offset (+ proto-offset (foreign-sizeof 'int)))
(define addr-offset (+ addrlen-offset (foreign-sizeof 'unsigned-long)))
(define name-offset (+ addr-offset (foreign-sizeof 'void*)))
(define next-offset (+ name-offset (foreign-sizeof 'void*)))
  
(define (make-foreign-address-info 
          flags family type proto addrlen addr name next)
  (let ([res (foreign-alloc size-of/addrinfo)])
    (foreign-set! 'int res 0 flags)
    (foreign-set! 'int res family-offset family)
    (foreign-set! 'int res type-offset type)
    (foreign-set! 'int res proto-offset proto)
    (foreign-set! 'unsigned-long res addrlen-offset addrlen)
    (foreign-set! 'void* res addr-offset addr)
    (foreign-set! 'void* res name-offset name)
    (foreign-set! 'void* res next-offset next)
    res))

(define (foreign-address-info-canonical-name addrinfo)
  (let ([ptr (foreign-ref 'void* addrinfo name-offset)])
    (if (zero? ptr) #f (get-foreign-string ptr))))

(define (foreign-address-info-domain addrinfo)
  (foreign-ref 'int addrinfo family-offset))
(define (foreign-address-info-type addrinfo)
  (foreign-ref 'int addrinfo type-offset))
(define (foreign-address-info-protocol addrinfo)
  (foreign-ref 'int addrinfo proto-offset))
(define (foreign-address-info-address addrinfo)
  (foreign-ref 'void* addrinfo addr-offset))
(define (foreign-address-info-address-length addrinfo)
  (foreign-ref 'unsigned-long addrinfo addrlen-offset))
(define (foreign-address-info-next addrinfo)
  (foreign-ref 'void* addrinfo next-offset))
(define (foreign-protocol-entry-name x)
  (get-foreign-string (foreign-pointer-value x)))

(define (foreign-protocol-entry-aliases x)
  (do ([ptr (foreign-ref 'void* x (foreign-sizeof 'void*))
            (+ ptr (foreign-sizeof 'void*))]
       [res '()  (cons (get-foreign-string (foreign-pointer-value ptr))
                       res)])
      [(zero? (foreign-pointer-value ptr)) (reverse res)]))

(define (foreign-protocol-entry-protocol x)
  (foreign-ref 'int x (* 2 (foreign-sizeof 'void*))))
(define get-foreign-string
  (let ([$strlen (foreign-procedure "strlen" (uptr) fixnum)]
        [$strcpy (foreign-procedure "strcpy" (u8* uptr) string)])
    (lambda (x)
      (let* ([len ($strlen x)]
             [buf (make-bytevector (1+ len))])
        ($strcpy buf x)))))
(define (make-foreign-size-buffer size)
  (let ([res (foreign-alloc (foreign-sizeof 'unsigned-long))])
    (foreign-set! 'unsigned-long res 0 size)
    res))

(define (foreign-size-buffer-value buf)
  (foreign-ref 'unsigned-long buf 0))
(meta-cond
  [(windows?)
   (define (%set-blocking fd yes?)
     (let ([buf (make-foreign-size-buffer (if yes? 0 1))])
       ($fcntl fd $file-set-flag buf)))]
  [else
    (define (%set-blocking fd yes?)
      ($fcntl fd $file-set-flag (if yes? 0 $option-non-blocking)))])
(meta-cond
  [(windows?)
   (define $wsa-startup
     (foreign-procedure __stdcall "WSAStartup"
       (unsigned-16 uptr)
       int))
   (define winsock-version (+ (bitwise-arithmetic-shift 2 8) 2))
   (let ([buf (foreign-alloc size-of/wsa-data)])
     ($wsa-startup winsock-version buf)
     (foreign-free buf))]
  [else (void)])

)
