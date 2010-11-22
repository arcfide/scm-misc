/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Socket Stubs
 * 
 * Copyright (c) 2010 Aaron Hsu <arcfide@sacrideo.us>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef __NT__
#define WIN32
#define EXPORTED(type) __declspec ( dllexport ) type cdecl
#endif
#ifdef __WINDOWS__
#define WIN32
#define EXPORTED(type) __declspec ( dllexport ) type cdecl
#endif

#ifndef WIN32
#define EXPORTED(type) type
#endif

#ifdef WIN32
#include <stddef.h>
#include <winsock2.h>
#include <ws2tcpip.h>
typedef int ssize_t;
extern __declspec(dllimport) int cdecl Sactivate_thread(void);
extern __declspec(dllimport) void cdecl Sdeactivate_thread(void);
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <sys/unistd.h>
#include <sys/fcntl.h>
#include "scheme.h"
#endif

/* Blocking Accept */
EXPORTED(int)
accept_block(int fd, struct sockaddr *addr, socklen_t *addrlen) {
        int ret;
        
        Sdeactivate_thread();
        ret = accept(fd, addr, addrlen);
        Sactivate_thread();
        
        return ret;
}

/* Blocking Connect */
EXPORTED(int)
connect_block(int fd, const struct sockaddr *addr, socklen_t addrlen) {
        int ret;
        
        Sdeactivate_thread();
        ret = connect(fd, addr, addrlen);
        Sactivate_thread();
        
        return ret;
}

/* Blocking Receive */
EXPORTED(ssize_t)
recvfrom_block(int fd, void *buf, size_t len, int flags,
    struct sockaddr *src_addr, socklen_t *addrlen) {
        ssize_t ret;
        
        Sdeactivate_thread();
        ret = recvfrom(fd, buf, len, flags, src_addr, addrlen);
        Sactivate_thread();
        
        return ret;
}

/* Blocking Send To */
EXPORTED(int)
sendto_block(int fd, const void *buf, size_t len, int flags,
    const struct sockaddr *dest_addr, socklen_t addrlen) {
        ssize_t ret;
        
        Sdeactivate_thread();
        ret = sendto(fd, buf, len, flags, dest_addr, addrlen);
        Sactivate_thread();
        
        return ret;
}
