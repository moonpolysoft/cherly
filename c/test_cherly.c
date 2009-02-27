#include "cherly.h"
#include "stdlib.h"
#include <assert.h>

void test_cherly() {
  cherly_t cherly;
  char* stuff = "stuff";
  
  cherly_init(&cherly, 0, 120);
  assert(NULL == cherly_get(&cherly, "noexist", 7));
  cherly_put(&cherly, "exist", 5, stuff);
  assert(NULL == cherly_get(&cherly, "noexist", 7));
  assert(stuff == cherly_get(&cherly, "exist", 5));
  
}