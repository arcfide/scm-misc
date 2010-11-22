#include <errno.h>

#ifdef __NT__
#define WIN32
#elif __WINDOWS__
#define WIN32
#endif

#ifdef WIN32
#define EXPORTED(type) __declspec(dllexport) type cdecl
#else
#define EXPORTED(type) type
#endif

EXPORTED(int)
get_errno()
{
	return errno;
}
