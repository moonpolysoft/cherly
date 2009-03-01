#ifndef __DOUBLE_LINK_H__
#define __DOUBLE_LINK_H__

typedef struct _d_node_t {
  struct _d_node_t * previous;
  struct _d_node_t * next;
  void * data;
} d_node_t;

typedef struct _d_list_t {
  d_node_t * head;
  d_node_t * tail;
  unsigned long size;
} d_list_t;

#define d_list_size(list) ((list)->size)
  

d_list_t * d_list_create();
void d_list_destroy(d_list_t * list);
void d_list_push(d_list_t *list, d_node_t *node);
d_node_t* d_list_pop(d_list_t *list);
void d_list_unshift(d_list_t *list, d_node_t *node);
d_node_t* d_list_shift(d_list_t *list);
void d_list_remove(d_list_t *list, d_node_t *node);

d_node_t * d_node_create(void * data);
void d_node_destroy(d_node_t* node);
#endif