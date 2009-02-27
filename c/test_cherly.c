#include "cherly.h"
#include "stdlib.h"
#include <assert.h>

void test_cherly() {
  cherly_t cherly;
  
  cherly_init(&cherly, 0, 120);
  assert(NULL == cherly_get(&cherly, "noexist", 7));
}