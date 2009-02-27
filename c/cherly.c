#include <Judy.h>
#include "cherly.h"
#include "double_link.h"



void cherly_init(cherly_t *cherly, int options, unsigned long max_size) {
  cherly->judy = NULL;
  cherly->lru  = d_list_create();
  cherly->size = 0;
  cherly->max_size = max_size;
}

// node -> item -> value

void cherly_put(cherly_t *cherly, char *key, int length, void *value, int size, DestroyCallback destroy) {
  PWord_t PValue;
  lru_item_t * item;
  d_node_t *node;
  
  while (cherly->size + size > cherly->max_size) {
    //eject shit from the back of the lru
    node = d_list_shift(cherly->lru);
    if (NULL == node) { //lolwut? is the size too small?
      break;
    }
    item = node->data;
    cherly->size -= item->vallen;
    (*destroy)(item->key, item->keylen, item->value, item->vallen);
    free(item);
    d_node_destroy(node);
  }
  
  item = malloc(sizeof(lru_item_t));
  node = d_node_create(item);
  item->key = key;
  item->keylen = length;
  item->value = value;
  item->vallen = size;
  item->destroy = destroy;

  
  JHSI(PValue, cherly->judy, key, length);
  *PValue = node;
  cherly->size += size;
  
  d_list_push(cherly->lru, node);
}

void * cherly_get(cherly_t *cherly, char *key, int length) {
  PWord_t PValue;
  lru_item_t * item;
  d_node_t * node;
  
  JHSG(PValue, cherly->judy, key, length);
  
  if (NULL == PValue) {
    return NULL;
  } else {
    node = (d_node_t *)*PValue;
    d_list_remove(cherly->lru, node);
    d_list_push(cherly->lru, node);
    item = (lru_item_t*)node->data;
    return item->value;
  }
}

void cherly_destroy(cherly_t *cherly) {
  PWord_t bytes;
  JHSFA(bytes, cherly->judy);
}