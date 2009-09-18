/* This is a stupid errno function that we need for sockets */

#include <errno.h>

int get_errno() {
	return errno;
}