#ifndef __CHERLY__
#define __CHERLY__

#include <Judy.h>
#include <cprops/heap.h>
#include "double_link.h"

//the destroy callback, this is needed to free memory for shit
typedef void (*DestroyCallback)(char*, int, void*, int);

typedef struct _cherly_t {
  Pvoid_t judy;
  d_list_t *lru;
  unsigned long size;
  unsigned long max_size;
} cherly_t;

typedef struct _lru_item_t {
  char * key;
  int keylen;
  void * value;
  int vallen;
  DestroyCallback destroy;
} lru_item_t;

void cherly_init(cherly_t *cherly, int options, unsigned long max_size);
void * cherly_get(cherly_t *cherly, char * key, int length);
void cherly_put(cherly_t *cherly, char * key, int length, void *value, int size, DestroyCallback);
void cherly_remove(cherly_t *cherly, char * key, int length);
void cherly_destroy(cherly_t *cherly);

#endif