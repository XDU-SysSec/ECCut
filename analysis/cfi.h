#define _GNU_SOURCE
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<sys/mman.h>
int pkey, status;
unsigned long *buffer;

#define HASH_KEY_RANGE 1000000

typedef struct CFI_ITEM {
    unsigned long origin;
    unsigned long originCtx;
    unsigned long ref_id;
    unsigned long target;
    struct CFI_ITEM *next;
} cfiItem;

typedef struct MPX_ENTRY {
    unsigned long origin;
    unsigned long originCtx;
} mEntry;

void update_mpx_table(unsigned long, unsigned long, unsigned long, unsigned long, unsigned long );
mEntry get_entry_mpx_table(unsigned long, unsigned long);
void pcall_reference_monitor(unsigned long, unsigned long, unsigned long);

void ECCut_init();
void ECCut_end();

