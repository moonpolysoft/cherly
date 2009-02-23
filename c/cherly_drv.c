#include <erl_driver.h>
#include <ei.h>
#include <stdio.h>

#define read_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))


static ErlDrvData init(ErlDrvPort port, char *cmd);
static void stop(ErlDrvData handle);
static void outputv(ErlDrvData handle, ErlIOVec *ev);

static ErlDrvEntry cherly_driver_entry = {
    NULL,                             /* init */
    init, 
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

static ErlDrvData init(ErlDrvPort port, char *cmd) {
  cherly_drv_t *cherly_drv = (cherly_drv_t*)driver_alloc(sizeof(cherly_drv_t));
  cherly->port = port;
  cherly->cherly = driver_alloc(sizeof(cherly_t));
  return (ErlDrvData) cherly_drv;
}

static void stop(ErlDrvData handle) {
  cherly_drv_t *cherly_drv = (cherly_drv_t *)handle;
  cherly_destroy(cherly_drv->cherly);
  
}

static void outputv(ErlDrvData handle, ErlIOVec *ev) {
  cherly_drv_t *cherly_drv = (cherly_drv_t *)handle;
  int i=0;
  char command;
  SysIOVec *iov;
  
  //command will come thru in the first binary
  iov = ev->iov[1];
  command = iov->iov_base[0];
  
  switch(command) {
    case INIT:
      cherly_init()
  };
  
  
  
  
  printf("ev->size %d\n", ev->size);
  printf("ev->vsize %d\n", ev->vsize);
  printf("ev->iov %p\n", ev->iov);
  printf("ev->binv %p\n", ev->binv[1]);
  for(i=0; i < ev->vsize; i++) {
    printf("\"%d\"\n", ev->iov[i].iov_len);
  }
  printf("binaries\n");
  for(i=1; i < ev->vsize; i++) {
    printf("\"%d\"\n", ev->binv[i]->orig_size);
  }
  // for(i=0; i < ev->vsize; i++) {
  //   printf("\"");
  //   for(n=0; n < ev->binv[i]->orig_size; n++) {
  //     printf("%c", ev->binv[i]->orig_bytes[n]);
  //   }
  //   printf("\"\n");
  // }
}
