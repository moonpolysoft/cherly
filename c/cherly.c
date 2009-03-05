#include <Judy.h>
#include "cherly.h"
#include "common.h"

static void cherly_eject_callback(cherly_t *cherly, char *key, int length);

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
  
  dprintf("inserting with keylen %d vallen %d\n", length, size);
  JHSG(PValue, cherly->judy, key, length);
  if (NULL != PValue) {
    item = (lru_item_t*)*PValue;
    dprintf("removing an existing value\n");
    cherly_remove(cherly, lru_item_key(item), lru_item_keylen(item));
  }
  
  if (cherly->size + size > cherly->max_size) {
    dprintf("projected new size %d is more than max %d\n", cherly->size + size, cherly->max_size);
    cherly->size -= lru_eject_by_size(cherly->lru, (length + size) - (cherly->max_size - cherly->size), (EjectionCallback)cherly_eject_callback, cherly);
  }
  
  item = lru_insert(cherly->lru, key, length, value, size, destroy);
  
  JHSI(PValue, cherly->judy, key, length);
  *PValue = (Word_t)item;
  cherly->size += lru_item_size(item);
  dprintf("new cherly size is %d\n", cherly->size);
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

static void cherly_eject_callback(cherly_t *cherly, char *key, int length) {
  PWord_t PValue;
  lru_item_t *item;
  int ret;
  
  JHSG(PValue, cherly->judy, key, length);
  if (NULL == PValue) {
    return;
  }
  item = (lru_item_t*)*PValue;
  
  JHSD(ret, cherly->judy, key, length);
  if (ret) {
    cherly->items_length--;
    cherly->size -= lru_item_size(item);
  }
}

void cherly_remove(cherly_t *cherly, char *key, int length) {
  PWord_t PValue;
  int ret;
  lru_item_t *item;
  
  JHSG(PValue, cherly->judy, key, length);
  
  if (NULL == PValue) {
    return;
  }
  
  item = (lru_item_t *)*PValue;
  lru_remove_and_destroy(cherly->lru, item);
  cherly->size -= lru_item_size(item);
  cherly->items_length--;
  JHSD(ret, cherly->judy, key, length);
}



void cherly_destroy(cherly_t *cherly) {
  Word_t bytes;
  dprintf("judy %p\n", cherly->judy);
  JHSFA(bytes, cherly->judy);
  dprintf("called JHSFA\n");
  lru_destroy(cherly->lru);
  dprintf("lru destroy\n");
}