#include <stdio.h>
#include <stdlib.h>
#include <check.h>

Suite * cherly_suite();
Suite * double_link_suite();
Suite * lru_suite();
Suite * cherly_drv_suite();

int main (void) {
  int number_failed;
  
  SRunner *sr = srunner_create(cherly_suite());
  srunner_add_suite(sr, double_link_suite());
  srunner_add_suite(sr, lru_suite());
  // srunner_add_suite(sr, cherly_drv_suite());
  
  srunner_run_all(sr, CK_NORMAL);
  number_failed = srunner_ntests_failed(sr);
  srunner_free(sr);
  return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
