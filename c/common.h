#ifndef __COMMON_H__
#define __COMMON_H__

#ifdef DEBUG
#define dprintf(format, args...)  printf (format , ## args)
#else
#define dprintf(format, args...)
#endif

#endif