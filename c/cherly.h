#ifndef __CHERLY__
#define __CHERLY__

#include <Judy.h>
#include "lru.h"

#define cherly_size(cherly) ((cherly)->size)
#define cherly_items_length(cherly) ((cherly)->items_length)
#define cherly_max_size(cherly) ((cherly)->max_size)

typedef struct _cherly_t {
  Pvoid_t judy;
  lru_t *lru;
  unsigned long size;
  unsigned long items_length;
  unsigned long max_size;
} cherly_t;

void cherly_init(cherly_t *cherly, int options, unsigned long max_size);
void * cherly_get(cherly_t *cherly, char * key, int length);
void cherly_put(cherly_t *cherly, char * key, int length, void *value, int size, DestroyCallback);
void cherly_remove(cherly_t *cherly, char * key, int length);
void cherly_destroy(cherly_t *cherly);

#endif