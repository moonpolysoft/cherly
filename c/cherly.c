#include <Judy.h>
#include "cherly.h"

void cherly_init(cherly_t *cherly, int options, unsigned long max_size) {
  cherly->judy = NULL;
  cherly->lru  = lru_create();
  cherly->size = 0;
  cherly->items_length = 0;
  cherly->max_size = max_size;
}

// node -> item -> value

void cherly_put(cherly_t *cherly, char *key, int length, void *value, int size, DestroyCallback destroy) {
  PWord_t PValue;
  lru_item_t * item;
  
  JHSG(PValue, cherly->judy, key, length);
  if (NULL != PValue) {
    item = (lru_item_t*)*PValue;
    printf("removing an existing value\n");
    cherly_remove(cherly, lru_item_key(item), lru_item_keylen(item));
  }
  
  if (cherly->size + size > cherly->max_size) {
    cherly->size -= lru_eject_by_size(cherly->lru, size - (cherly->max_size - cherly->size), (EjectionCallback)cherly_remove, cherly);
  }
  
  item = lru_insert(cherly->lru, key, length, value, size, destroy);
  
  JHSI(PValue, cherly->judy, key, length);
  *PValue = (Word_t)item;
  cherly->size += lru_item_size(item);
  cherly->items_length++;
}

void * cherly_get(cherly_t *cherly, char *key, int length) {
  PWord_t PValue;
  lru_item_t * item;
  
  JHSG(PValue, cherly->judy, key, length);
  
  if (NULL == PValue) {
    return NULL;
  } else {
    item = (lru_item_t *)*PValue;
    lru_touch(cherly->lru, item);
    return lru_item_value(item);
  }
}

void cherly_remove(cherly_t *cherly, char *key, int length) {
  PWord_t PValue;
  lru_item_t *item;
  
  JHSG(PValue, cherly->judy, key, length);
  
  if (NULL == PValue) {
    return;
  }
  
  item = (lru_item_t *)*PValue;
  lru_remove_and_destroy(cherly->lru, item);
  cherly->size -= lru_item_size(item);
}



void cherly_destroy(cherly_t *cherly) {
  Word_t bytes;
  JHSFA(bytes, cherly->judy);
  lru_destroy(cherly->lru);
  free(cherly);
}