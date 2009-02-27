#include <Judy.h>
#include <erl_driver.h>
#include <ei.h>
#include <stdio.h>
#include <string.h>
#include "cherly.h"

#define read_int32(s)  ((((int)(((unsigned char*) (s))[0]))  << 24) | \
                        (((int)(((unsigned char*) (s))[1]))  << 16) | \
                        (((int)(((unsigned char*) (s))[2]))  << 8)  | \
                        (((int)(((unsigned char*) (s))[3]))))

#define INIT 'i'
#define GET 'g'
#define PUT 'p'
#define DELETE 'd'

static ErlDrvData start(ErlDrvPort port, char *cmd);
static void stop(ErlDrvData handle);
static void outputv(ErlDrvData handle, ErlIOVec *ev);
static void destroy(char * key, int keylen, void * value, int vallen);

static ErlDrvEntry cherly_driver_entry = {
    NULL,                             /* init */
    start, 
    stop, 
    NULL,                             /* output */
    NULL,                             /* ready_input */
    NULL,                             /* ready_output */ 
    "cherly_drv",                     /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    outputv,                          /* outputv */
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

typedef struct _cherly_drv_t {
  ErlDrvPort port;
  cherly_t* cherly;
  } cherly_drv_t;

DRIVER_INIT(cherly_driver) {
  return &cherly_driver_entry;
}

static ErlDrvData start(ErlDrvPort port, char *cmd) {
  cherly_drv_t *cherly_drv = (cherly_drv_t*)driver_alloc(sizeof(cherly_drv_t));
  cherly_drv->port = port;
  cherly_drv->cherly = driver_alloc(sizeof(cherly_t));
  return (ErlDrvData) cherly_drv;
}

static void stop(ErlDrvData handle) {
  cherly_drv_t *cherly_drv = (cherly_drv_t *)handle;
  cherly_destroy(cherly_drv->cherly);
  
}

static void init(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  SysIOVec *iov;
  int index = 0;
  long options;
  unsigned long max_size;
  
  iov = &ev->iov[2];
  ei_decode_version(iov->iov_base, &index, NULL);
  ei_decode_tuple_header(iov->iov_base, &index, NULL);
  ei_decode_long(iov->iov_base, &index, &options);
  ei_decode_ulong(iov->iov_base, &index, &max_size);
  cherly_init(cherly_drv->cherly, options, max_size);
}

static void get(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  SysIOVec *key;
  ErlIOVec *value;
  
  key = &ev->iov[2];
  value = (ErlIOVec *) cherly_get(cherly_drv->cherly, key->iov_base, key->iov_len);
  driver_outputv(cherly_drv->port, "", 0, value, 0);
}

static void put(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  SysIOVec *key;
  ErlIOVec *value;
  ErlDrvBinary *bin;
  ErlDrvBinary **binv;
  char* copied_key;
  int i;
  int size;
  
  key = &ev->iov[2];
  value = driver_alloc(sizeof(ErlIOVec));
  value->iov = driver_alloc(sizeof(SysIOVec) * (ev->vsize-3)); //will this work
  // value->iov = NULL;
  value->vsize = ev->vsize-3;
  value->size = 0;
  //we need to copy this to the new vec
  binv = driver_alloc(sizeof(ErlDrvBinary*) * (ev->vsize-3));
  for(i=3; i < ev->vsize; i++) {
    bin = ev->binv[i];
    value->size += bin->orig_size;
    binv[i-3] = bin;
    driver_binary_inc_refc(bin);
    value->iov[i-3].iov_len = bin->orig_size;
    value->iov[i-3].iov_base = bin->orig_bytes;
  }
  value->binv = binv;
  //need to copy the key here
  copied_key = driver_alloc(sizeof(char) * key->iov_len);
  memcpy(copied_key, key->iov_base, key->iov_len);
  
  cherly_put(cherly_drv->cherly, copied_key, key->iov_len, value, value->size, &destroy);
}

static void delete(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  
}

static void outputv(ErlDrvData handle, ErlIOVec *ev) {
  cherly_drv_t *cherly_drv = (cherly_drv_t *)handle;
  char command;
  
  //command will come thru in the first binary
  command = ev->iov[1].iov_base[0];
  
  switch(command) {
  case INIT:
    init(cherly_drv, ev);
    break;
  case GET:
    get(cherly_drv, ev);
    break;
  case PUT:
    put(cherly_drv, ev);
    break;
  case DELETE:
    delete(cherly_drv, ev);
    break;
  };
  
  
  
  
  // printf("ev->size %d\n", ev->size);
  // printf("ev->vsize %d\n", ev->vsize);
  // printf("ev->iov %p\n", ev->iov);
  // printf("ev->binv %p\n", ev->binv[1]);
  // for(i=0; i < ev->vsize; i++) {
  //   printf("\"%d\"\n", ev->iov[i].iov_len);
  // }
  // printf("binaries\n");
  // for(i=1; i < ev->vsize; i++) {
  //   printf("\"%d\"\n", ev->binv[i]->orig_size);
  // }
  // for(i=0; i < ev->vsize; i++) {
  //   printf("\"");
  //   for(n=0; n < ev->binv[i]->orig_size; n++) {
  //     printf("%c", ev->binv[i]->orig_bytes[n]);
  //   }
  //   printf("\"\n");
  // }
}

// value = driver_alloc(sizeof(ErlIOVec));
// value->iov = driver_alloc(sizeof(SysIOVec) * (ev->vsize-3)); //will this work
// // value->iov = NULL;
// value->vsize = ev->vsize-3;
// value->size = 0;
// //we need to copy this to the new vec
// binv = driver_alloc(sizeof(ErlDrvBinary*) * (ev->vsize-3));
// for(i=3; i < ev->vsize; i++) {
//   bin = ev->binv[i];
//   value->size += bin->orig_size;
//   binv[i-3] = bin;
//   driver_binary_inc_refc(bin);
//   value->iov[i-3].iov_len = bin->orig_size;
//   value->iov[i-3].iov_base = bin->orig_bytes;
// }
// value->binv = binv;

static void destroy(char * key, int keylen, void * value, int vallen) {
  ErlIOVec* ev = (ErlIOVec*)value;
  int i;
  
  free(key);
  for(i=0; i < ev->vsize; i++) {
    driver_free_binary(ev->binv[i]);
  }
  driver_free(ev->iov);
  driver_free(ev->binv);
  
}