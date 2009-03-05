#include "lru.h"
#include "stdlib.h"
#include "stdio.h"
#include <check.h>
#include "common.h"

START_TEST(create_and_insert) {
  lru_t *lru;
  
  lru = lru_create();
  fail_unless(NULL != lru->list);
  lru_insert(lru, "key", 3, "value", 5, NULL);
  fail_unless(NULL != lru->list->head);
  lru_destroy(lru);
}
END_TEST

START_TEST(touch) {
  lru_t *lru;
  lru_item_t * one;
  
  lru = lru_create();
  one = lru_insert(lru, "one", 3, "one", 3, NULL);
  lru_insert(lru, "two", 3, "two", 3, NULL);
  lru_insert(lru, "three", 5, "three", 5, NULL);
  lru_touch(lru, one);
  fail_unless(one == (lru_item_t*)lru->list->head->data);
  lru_destroy(lru);
}
END_TEST

START_TEST(eject_one) {
  lru_t *lru;
  int ejected = 0;
  
  lru = lru_create();
  lru_insert(lru, "one", 3, "one", 3, NULL);
  lru_insert(lru, "two", 3, "two", 3, NULL);
  lru_insert(lru, "three", 5, "three", 5, NULL);
  ejected = lru_eject_by_size(lru, 3, NULL, NULL);
  dprintf("ejected %d\n", ejected);
  fail_unless(ejected > 0);
}
END_TEST

START_TEST(eject_multiple) {
  lru_t *lru;
  int ejected = 0;
  lru_item_t *three;
  mark_point();
  lru = lru_create();
  mark_point();
  lru_insert(lru, "one", 3, "one", 3, NULL);
  lru_insert(lru, "two", 3, "two", 3, NULL);
  three = lru_insert(lru, "three", 5, "three", 5, NULL);
  ejected = lru_eject_by_size(lru, 12, NULL, NULL);
  dprintf("test ejected %d\n", ejected);
  fail_unless((lru_item_t*)lru->list->head->data == three);
  fail_unless(ejected == 12);
  lru_destroy(lru);
}
END_TEST

Suite * lru_suite(void) {
  Suite *s = suite_create("lru.c");
  
  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, create_and_insert);
  tcase_add_test(tc_core, touch);
  tcase_add_test(tc_core, eject_one);
  tcase_add_test(tc_core, eject_multiple);
  suite_add_tcase(s, tc_core);
  
  return s;
}