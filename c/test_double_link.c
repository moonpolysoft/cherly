#include "double_link.h"
#include "stdlib.h"
#include <assert.h>

void test_double_link() {
  d_list_t *list;
  d_node_t *node;
  int i;
  
  list = d_list_create();
  node = d_node_create(NULL);
  assert(node != NULL);
  d_list_push(list, node);
  assert(1 == d_list_size(list));
  
  for(i=0; i<5; i++) {
    node = d_node_create(NULL);
    d_list_push(list, node);
  }
  assert(6 == d_list_size(list));
  
}