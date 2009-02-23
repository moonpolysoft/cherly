#include <Judy.h>
#include "cherly.h"

void cherly_init(cherly_t *cherly, int options, unsigned long max_size) {
  cherly->judy = NULL;
  cherly->size = 0;
  cherly->max_size = max_size;
}

void cherly_put(cherly_t *cherly, void *key, int length, void *value) {
  JHSI(value, cherly->judy, key, length);
}

void * cherly_get(cherly_t *cherly, void *key, int length) {
  void *value;
  
  JHSG(value, cherly->judy, key, length);
  return value;
}

void cherly_destroy(cherly_t *cherly) {
  PWord_t bytes;
  JHSFA(bytes, cherly->judy);
}