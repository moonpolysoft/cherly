#include "lru.h"
#include "double_link.h"
#include <stdlib.h>
#include "common.h"

static void lru_destroy_item(lru_item_t *item);

lru_t * lru_create() {
  lru_t * lru = malloc(sizeof(lru_t));
  lru->list = d_list_create();
  return lru;
}

void lru_destroy(lru_t *lru) {
  d_node_t *node;
  lru_item_t *item;
  
  node = lru->list->head;
  while (NULL != node) {
    item = (lru_item_t*)node->data;
    lru_destroy_item(item);
    
    node = node->next;
  } 
  
  d_list_destroy(lru->list);
  free(lru);
}

int lru_eject_by_size(lru_t *lru, int size, EjectionCallback eject, void * container) {
  int ejected = 0;
  lru_item_t *item;
  d_node_t *node;
  d_node_t *next;
  
  dprintf("ejecting %d bytes\n", size);
  
  while(ejected < size) {
    node = d_list_shift(lru->list);
    if (NULL == node) {
      break;
    }
    item = (lru_item_t*)node->data;
    ejected += lru_item_size(item);
    if (NULL != eject) {
      (*eject)(container, item->key, item->keylen);
    }
    lru_destroy_item(item);
    next = node->next;
    d_node_destroy(node);
    node = next;
  }
  return ejected;
}

lru_item_t * lru_insert(lru_t *lru, char* key, int keylen, void * value, int size, DestroyCallback destroy) {
  lru_item_t *item;
  
  item = malloc(sizeof(lru_item_t));
  item->key = key;
  item->keylen = keylen;
  item->value = value;
  item->vallen = size;
  item->destroy = destroy;
  item->node = d_node_create(item);
  d_list_push(lru->list, item->node);
  return item;
}

void lru_touch(lru_t *lru, lru_item_t *item) {
  d_list_remove(lru->list, item->node);
  d_list_push(lru->list, item->node);
}

void lru_remove_and_destroy(lru_t *lru, lru_item_t *item) {
  d_list_remove(lru->list, item->node);
  d_node_destroy(item->node);
  lru_destroy_item(item);
}

static void lru_destroy_item(lru_item_t *item) {
  if (NULL != item->destroy) {
    (*(item->destroy))(item->key, item->keylen, item->value, item->vallen);
  }
  free(item);
}