#include "double_link.h"
#include "stdlib.h"
#include <check.h>
#include "common.h"

START_TEST(create_and_remove)
{
  d_list_t *list;
  d_node_t *node;
  int i;
  
  list = d_list_create();
  node = d_node_create(NULL);
  fail_unless(node != NULL, NULL);
  d_list_push(list, node);
  fail_unless(1 == d_list_size(list), NULL);
  
  for(i=0; i<5; i++) {
    node = d_node_create(NULL);
    d_list_push(list, node);
  }
  fail_unless(6 == d_list_size(list), NULL);
}
END_TEST

Suite * double_link_suite(void) {
  Suite *s = suite_create ("double_link.c");

  /* Core test case */
  TCase *tc_core = tcase_create ("Core");
  tcase_add_test (tc_core, create_and_remove);
  suite_add_tcase (s, tc_core);

  return s;
}