#include "cherly.h"
#include <check.h>

START_TEST(basic_get_and_put)
{  cherly_t cherly;
  char* stuff = "stuff";
  
  cherly_init(&cherly, 0, 120);
  fail_unless(NULL == cherly_get(&cherly, "noexist", 7));
  cherly_put(&cherly, "exist", 5, stuff, 5, NULL);
  fail_unless(NULL == cherly_get(&cherly, "noexist", 7));
  fail_unless(stuff == cherly_get(&cherly, "exist", 5));
}
END_TEST

START_TEST(put_already_there)
{
  cherly_t cherly;
  char* stuff = "stuff";
  char* otherstuff = "blah";
  
  cherly_init(&cherly, 0, 120);
  cherly_put(&cherly, "exist", 5, stuff, 5, NULL);
  cherly_put(&cherly, "exist", 5, otherstuff, 4, NULL);
  
}
END_TEST

Suite * cherly_suite(void) {
  Suite *s = suite_create("cherly.c");

  /* Core test case */
  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, basic_get_and_put);
  tcase_add_test(tc_core, put_already_there);
  suite_add_tcase(s, tc_core);

  return s;
}