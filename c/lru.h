#ifndef __LRU__
#define __LRU__

#include "double_link.h"


//the destroy callback, this is needed to free memory for shit
typedef void (*DestroyCallback)(char*, int, void*, int);

//this is the callback for LRU ejection, so that upper layers can cleanup
//first arg is the containing struct.  can this be cleaner?
typedef void (*EjectionCallback)(void*, char*, int);

typedef struct _lru_t {
  d_list_t *list;
}  lru_t;

typedef struct _lru_item_t {
  char * key;
  int keylen;
  void * value;
  int vallen;
  d_node_t * node;
  DestroyCallback destroy;
} lru_item_t;

#define lru_item_key(item) ((item)->key)
#define lru_item_keylen(item) ((item)->keylen)
#define lru_item_value(item) ((item)->value)
#define lru_item_vallen(item) ((item)->vallen)
#define lru_item_size(item) (lru_item_keylen(item) + lru_item_vallen(item))

lru_t * lru_create();
void lru_destroy(lru_t *lru);
int lru_eject_by_size(lru_t *lru, int size, EjectionCallback cb, void * container);
lru_item_t * lru_insert(lru_t *lru, char* key, int keylen, void * value, int size, DestroyCallback destroy);
void lru_touch(lru_t *lru, lru_item_t *item);
void lru_remove_and_destroy(lru_t *lru, lru_item_t *item);

#endif