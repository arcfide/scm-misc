/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Socket Foreign Values
 * 
 * Copyright (c) 2009 Aaron Hsu <arcfide@sacrideo.us>
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

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <sys/unistd.h>
#include <sys/fcntl.h>

int get_ffi_value(const char *val) {
  struct sockaddr_in sai;
  struct sockaddr_un sau;

  if (!strcmp(val, "$ipproto-ip")) return IPPROTO_IP;
  if (!strcmp(val, "$ipproto-raw")) return IPPROTO_RAW;
  if (!strcmp(val, "$ipproto-tcp")) return IPPROTO_TCP;
  if (!strcmp(val, "$ipproto-udp")) return IPPROTO_UDP;
  if (!strcmp(val, "$sol-socket")) return SOL_SOCKET;
  if (!strcmp(val, "%ai/canonname")) return AI_CANONNAME;
  if (!strcmp(val, "%ai/numerichost")) return AI_NUMERICHOST;
  if (!strcmp(val, "%ai/passive")) return AI_PASSIVE;
  if (!strcmp(val, "%msg/dont-route")) return MSG_DONTROUTE;
  if (!strcmp(val, "%msg/dont-wait")) return MSG_DONTWAIT;
  if (!strcmp(val, "%msg/out-of-band")) return MSG_OOB;
  if (!strcmp(val, "%msg/peek")) return MSG_PEEK;
  if (!strcmp(val, "%msg/wait-all")) return MSG_WAITALL;
  if (!strcmp(val, "%shutdown/read")) return SHUT_WR;
  if (!strcmp(val, "%shutdown/read&write")) return SHUT_RDWR;
  if (!strcmp(val, "%shutdown/write")) return SHUT_RD;
  if (!strcmp(val, "%socket-domain/internet")) return AF_INET;
  if (!strcmp(val, "%socket-domain/internet-v6")) return AF_INET6;
  if (!strcmp(val, "%socket-domain/local")) return AF_LOCAL;
  if (!strcmp(val, "%socket-domain/unix")) return AF_UNIX;
  if (!strcmp(val, "%socket-type/datagram")) return SOCK_DGRAM;
  if (!strcmp(val, "%socket-type/random")) return SOCK_RDM;
  if (!strcmp(val, "%socket-type/raw")) return SOCK_RAW;
  if (!strcmp(val, "%socket-type/sequence-packet")) return SOCK_SEQPACKET;
  if (!strcmp(val, "%socket-type/stream")) return SOCK_STREAM;
  if (!strcmp(val, "%somaxconn")) return SOMAXCONN;
  if (!strcmp(val, "af-inet")) return AF_INET;
  if (!strcmp(val, "af-unix")) return AF_UNIX;
  if (!strcmp(val, "invalid-socket")) return -1;
  if (!strcmp(val, "size-of/addr-in")) return sizeof(struct sockaddr_in);
  if (!strcmp(val, "size-of/addr-un")) return sizeof(struct sockaddr_un);
  if (!strcmp(val, "size-of/addrinfo")) return sizeof(struct addrinfo);
  if (!strcmp(val, "size-of/integer")) return sizeof(int);
  if (!strcmp(val, "size-of/ip")) return sizeof(struct in_addr);
  if (!strcmp(val, "size-of/pointer")) return sizeof(void *);
  if (!strcmp(val, "size-of/port")) return sizeof(sai.sin_port); 
  if (!strcmp(val, "size-of/protoent")) return sizeof(struct protoent);
  if (!strcmp(val, "size-of/sa-family")) return sizeof(sa_family_t);
  if (!strcmp(val, "size-of/size-t")) return sizeof(size_t);
  if (!strcmp(val, "size-of/sockaddr-in")) return sizeof(struct sockaddr_in);
  if (!strcmp(val, "size-of/sockaddr-un")) return sizeof(struct sockaddr_un);
  if (!strcmp(val, "size-of/socklen-t")) return sizeof(socklen_t);
  if (!strcmp(val, "unix-max-path")) return sizeof(sau.sun_path);
  if (!strcmp(val, "$file-get-flag")) return F_GETFL;
  if (!strcmp(val, "$file-set-flag")) return F_SETFL;
  if (!strcmp(val, "$option-non-blocking")) return O_NONBLOCK;
  if (!strcmp(val, "$error-again")) return EAGAIN;
  if (!strcmp(val, "$error-in-progress")) return EINPROGRESS;
  if (!strcmp(val, "$error-would-block")) return EWOULDBLOCK;
  if (!strcmp(val, "$socket-error")) return -1;
  
  return 0;
}
