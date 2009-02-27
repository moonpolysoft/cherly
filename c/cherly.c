#include <Judy.h>
#include "cherly.h"
#include "double_link.h"



void cherly_init(cherly_t *cherly, int options, unsigned long max_size) {
  cherly->judy = NULL;
  cherly->lru  = d_list_create();
  cherly->size = 0;
  cherly->max_size = max_size;
}

void cherly_put(cherly_t *cherly, char *key, int length, void *value) {
  PWord_t PValue;
  JHSI(PValue, cherly->judy, key, length);
  *PValue = value;
  if (value == PJERR) {
    printf("shit\n");
  }
}

void * cherly_get(cherly_t *cherly, char *key, int length) {
  PWord_t PValue;
  
  JHSG(PValue, cherly->judy, key, length);
  
  if (NULL == PValue) {
    return NULL;
  }
  
  return (void *)*PValue;
}

void cherly_destroy(cherly_t *cherly) {
  PWord_t bytes;
  JHSFA(bytes, cherly->judy);
}