#include "double_link.h"
#include <stdlib.h>
#include "common.h"

d_list_t * d_list_create() {
  d_list_t * list = malloc(sizeof(d_list_t));
  list->head = NULL;
  list->tail = NULL;
  list->size = 0;
  return list;
}

void d_list_destroy(d_list_t *list) {
  d_node_t * node = list->head;
  d_node_t * tmp = NULL;
  while (NULL != node) {
    tmp = node->next;
    d_node_destroy(node);
    node = tmp;
  }
  free(list);
}

void d_list_push(d_list_t *list, d_node_t *node) {
  if (NULL == list->head && NULL == list->tail) {
    list->head = node;
    list->tail = node;
    node->previous = NULL;
    node->next = NULL;
  } else {
    node->previous = NULL;
    list->head->previous = node;
    node->next = list->head;
    list->head = node;
  }
  list->size++;
}

d_node_t * d_list_pop(d_list_t *list) {
  d_node_t * node = list->head;
  if (NULL == node) {
    return NULL;
  }
  
  list->head = node->next;
  list->size--;
  if (NULL != list->head) {
    list->head->previous = NULL;
  } else {
    list->tail = NULL;
  }
  return node;
}

void d_list_unshift(d_list_t *list, d_node_t *node) {
  if (NULL == list->head && NULL == list->tail) {
    list->head = node;
    list->tail = node;
    node->previous = NULL;
    node->next = NULL;
  } else {
    node->previous = list->tail;
    list->tail->next = node;
    node->next = NULL;
    list->tail = node;
  }
  list->size++;
}

d_node_t * d_list_shift(d_list_t *list) {
  d_node_t * node = list->tail;
  if (NULL == node) {
    return NULL;
  }
  
  list->tail = node->previous;
  list->size--;
  if (NULL != list->tail) {
    list->tail->next = NULL;
  } else {
    list->head = NULL;
  }
  return node;
}

void d_list_remove(d_list_t *list, d_node_t *node) {
  d_node_t *previous;
  d_node_t *next;
  
  if (list->head == node) {
    d_list_pop(list);
    return;
  } else if (list->tail == node) {
    d_list_shift(list);
    return;
  }
  
  previous = node->previous;
  next = node->next;
  
  node->previous = NULL;
  node->next = NULL;
  
  if (NULL != previous) {
    previous->next = next;
  }
  
  if (NULL != next) {
    next->previous = previous;
  }
}

d_node_t * d_node_create(void *data) {
  d_node_t * node;
  
  node = malloc(sizeof(d_node_t));
  node->previous = NULL;
  node->next = NULL;
  node->data = data;
  
  return node;
}

void d_node_destroy(d_node_t * node) {
  free(node);
}