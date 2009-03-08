#include <Judy.h>
#include <erl_driver.h>
#include <ei.h>
#include <stdio.h>
#include <string.h>
#include "cherly.h"
#include "common.h"

#define read_int32(s)  ((((int)(((unsigned char*) (s))[0]))  << 24) | \
                        (((int)(((unsigned char*) (s))[1]))  << 16) | \
                        (((int)(((unsigned char*) (s))[2]))  << 8)  | \
                        (((int)(((unsigned char*) (s))[3]))))

#define INIT 'i'
#define GET 'g'
#define PUT 'p'
#define REMOVE 'r'
#define SIZE 's'
#define ITEMS 't'

static ErlDrvData start(ErlDrvPort port, char *cmd);
static void stop(ErlDrvData handle);
static void outputv(ErlDrvData handle, ErlIOVec *ev);
static void destroy(char * key, int keylen, void * value, int vallen);
static void send_long(ErlDrvPort port, long num);
static void send_atom(ErlDrvPort port, char* atom);
static void print_ev(ErlIOVec *vec);

ErlIOVec* copy_io_vec(ErlIOVec *src);
char* io_vec2str(ErlIOVec *src, int skip, int length);

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
  dprintf("starting a new cherly\n");
  return (ErlDrvData) cherly_drv;
}

static void stop(ErlDrvData handle) {
  cherly_drv_t *cherly_drv;
  dprintf("handle %p\n", handle);
  cherly_drv = (cherly_drv_t *)handle;
  cherly_destroy(cherly_drv->cherly);
  driver_free(cherly_drv->cherly);
  driver_free(cherly_drv);
}

static void init(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  SysIOVec *iov;
  int index = 0;
  long options;
  unsigned long max_size;
  
  iov = &ev->iov[1];
  dprintf("ev->vsize %d\n", ev->vsize);
  dprintf("iov %p\n", iov);
  dprintf("iov base %p\n", iov->iov_base);
  ei_decode_version(&iov->iov_base[1], &index, NULL);
  ei_decode_tuple_header(&iov->iov_base[1], &index, NULL);
  ei_decode_long(&iov->iov_base[1], &index, &options);
  ei_decode_ulong(&iov->iov_base[1], &index, &max_size);
  dprintf("cherly init %d\n", max_size);
  cherly_init(cherly_drv->cherly, options, max_size);
}

static void get(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  char *key;
  ErlIOVec *value;
  int length = 0;
  
  print_ev(ev);
  
  length = read_int32(&(ev->binv[1]->orig_bytes[1]));
  key = io_vec2str(ev, 5, length);
  dprintf("key = %c%c%c\n", key[0], key[1], key[2]);
  
  value = (ErlIOVec *) cherly_get(cherly_drv->cherly, key, length);
  dprintf("get value %p\n", value);
  dprintf("ev %p\n", ev);
  driver_free(key);
  dprintf("freed\n");
  if (NULL != value) {
    print_ev(value);
    driver_outputv(cherly_drv->port, "", 0, value, 0);
  } else {
    dprintf("value was NULL\n");
    send_atom(cherly_drv->port, "not_found");
  }
}

static void put(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  ErlDrvBinary *bin = NULL;
  char* copied_key;
  ErlIOVec *copied_vec;
  int length = 0;
  int v=0;
  
  print_ev(ev);
  dprintf("ev size %d\n", ev->size);
  dprintf("iovec %p\n", ev);
  //need to copy the key here
  while (NULL == bin && v < ev->vsize) {
    dprintf("v %d\n", v);
    bin = ev->binv[v++];
  }
  
  dprintf("v is %d\n", v);
  for(v = 0; v < ev->vsize; v++) {
    if (NULL != ev->binv[v]) {
      driver_binary_inc_refc(ev->binv[v]);
    }
  }
  length = read_int32(&bin->orig_bytes[1]);
  dprintf("length is %d\n", length);
  copied_key = io_vec2str(ev, 5, length);
  dprintf("here\n");
  copied_vec = copy_io_vec(ev);
  dprintf("copied_key %p\n", copied_key);
  cherly_put(cherly_drv->cherly, copied_key, length, copied_vec, copied_vec->size, &destroy);
}

static void chd_remove(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  char * key;
  int length;
  
  length = read_int32(&(ev->binv[1]->orig_bytes[1]));
  key = io_vec2str(ev, 5, length);
  
  cherly_remove(cherly_drv->cherly, key, length);
  driver_free(key);
}

static void size(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  int size = cherly_size(cherly_drv->cherly);
  send_long(cherly_drv->port, size);
}

static void items(cherly_drv_t *cherly_drv, ErlIOVec *ev) {
  int length = cherly_items_length(cherly_drv->cherly);
  send_long(cherly_drv->port, length);
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
  case REMOVE:
    chd_remove(cherly_drv, ev);
    break;
  case SIZE:
    size(cherly_drv, ev);
    break;
  case ITEMS:
    items(cherly_drv, ev);
    break;
  }
}

static void print_ev(ErlIOVec *ev) {
  #ifdef DEBUG
  int i=0;
  char *tmp;
  ErlDrvBinary *bin;
  if (NULL == ev) {
    dprintf("NULL");
    return;
  }
  dprintf("[");
  for(i=0; i<ev->vsize; i++) {
    bin = ev->binv[i];
    if (NULL == bin) {
      dprintf("NULL ");
      continue;
    }
    if (bin->orig_size == 0) {
      dprintf("\"\" ");
    } else {
      tmp = malloc(sizeof(char) * (bin->orig_size+1));
      strncpy(tmp, bin->orig_bytes, bin->orig_size);
      dprintf("\"%s\" ", tmp);
      free(tmp);
    }
  }
  dprintf("]\n");
  #endif
}

static void send_long(ErlDrvPort port, long num) {
  ei_x_buff x;
  
  ei_x_new_with_version(&x);
  ei_x_encode_long(&x, num);
  driver_output(port, x.buff, x.index);
  ei_x_free(&x);
}

static void send_atom(ErlDrvPort port, char *atom) {
  ErlDrvTermData spec[] = {ERL_DRV_PORT, driver_mk_port(port), ERL_DRV_ATOM, driver_mk_atom(atom), ERL_DRV_TUPLE, 2};
  driver_output_term(port, spec, 6);
}

static void destroy(char * key, int keylen, void * value, int vallen) {
  ErlIOVec* ev = (ErlIOVec*)value;
  int i;
  driver_free(key);
  for(i=0; i < ev->vsize; i++) {
    if (NULL != ev->binv[i]) {
      driver_free_binary(ev->binv[i]);
    }
  }
  driver_free(ev->iov);
  driver_free(ev->binv);
  driver_free(ev);
}

ErlIOVec* copy_io_vec(ErlIOVec *ev) {
  ErlIOVec *to = driver_alloc(sizeof(ErlIOVec));
  ErlDrvBinary *bin;
  int i;
  
  dprintf("to %p\n", to);
  dprintf("ev %p\n", ev);
  
  to->iov = driver_alloc(sizeof(SysIOVec) * ev->vsize);
  to->vsize = ev->vsize;
  to->size = ev->size;
  to->binv = driver_alloc(sizeof(ErlDrvBinary*) * ev->vsize);
  for(i=0; i < ev->vsize; i++) {
    bin = ev->binv[i];
    if (NULL == bin) {
      to->binv[0] = NULL;
      to->iov[i].iov_len = 0;
      to->iov[i].iov_base = NULL;
    } else {
      to->binv[i] = bin;
      to->iov[i].iov_len = bin->orig_size;
      to->iov[i].iov_base = bin->orig_bytes;
    }
  }
  return to;
  // ErlIOVec *value = driver_alloc(sizeof(ErlIOVec));
  // value->iov = driver_alloc(sizeof(SysIOVec) * (ev->vsize-3)); //will this work
  // // value->iov = NULL;
  // value->vsize = ev->vsize-3;
  // value->size = 0;
  // //we need to copy this to the new vec
  // binv = driver_alloc(sizeof(ErlDrvBinary*) * (ev->vsize-3));
  // dprintf("ev->vsize %d\n", ev->vsize);
  // for(i=3; i < ev->vsize; i++) {
  //   bin = ev->binv[i];
  //   value->size += bin->orig_size;
  //   binv[i-3] = bin;
  //   driver_binary_inc_refc(bin);
  //   value->iov[i-3].iov_len = bin->orig_size;
  //   value->iov[i-3].iov_base = bin->orig_bytes;
  // }
  // value->binv = binv;
}

char* io_vec2str(ErlIOVec *src, int skip, int length) {
  char *target;
  int v = 0;
  int i = 0;
  int pos = 0;
  ErlDrvBinary * bin;
  
  if (skip > src->size) {
    dprintf("returning null\n");
    return NULL;
  }
  // target = driver_alloc(sizeof(char) * length);
  // driver_free(target);
  target = driver_alloc(sizeof(char) * length);
  dprintf("target %p\n", target);
  //skip whole binaries
  while(v < src->vsize) {
    bin = src->binv[v];
    if (NULL == bin) {
      v++;
      continue;
    }
    if (bin->orig_size >= skip) {
      dprintf("skip size is smaller than the bin\n");
      break;
    } else {
      v++;
      skip -= bin->orig_size;
    }
  }
  i = skip;
  
  while(v < src->vsize && length > 0) {
    bin = src->binv[v];
    dprintf("v is %d\n", v);
    if (length > (bin->orig_size - skip)) {
      dprintf("copying %d bytes.  length is %d\n", bin->orig_size - skip, length);
      memcpy(&target[pos], &bin->orig_bytes[skip], bin->orig_size - skip);
      length -= bin->orig_size - skip;
      pos += bin->orig_size - skip;
    } else {
      dprintf("copying %d bytes from %d.  length is %d. pos is %d\n", length, skip, length, pos);
      memcpy(&target[pos], &bin->orig_bytes[skip], length);
      dprintf("target = %c%c%c\n", target[0], target[1], target[2]);
      pos += length;
      break;
    }
    skip = 0;
  }
  return target;
}