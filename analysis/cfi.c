#include "cfi.h"

// will be used for statistical purpose
unsigned long cfi_stats[4] = {0};
char *stats_name[4] = {
    "update_mpx: ",      "get_entry: ",     "cfi_vcall: ",
    "cfi_pcall: ",       "ref_pcall",       "ref_vcall"};

__attribute__((__used__)) __attribute__((section("cfg_label_data")))
const int *PCALL_ECCut[] = {};
__attribute__((__used__)) unsigned int PCALL_ECCut_C = 0;
// Format: ref_id, target, origin, originCtx
cfiItem *CFI_HASH_TABLE[HASH_KEY_RANGE] = {NULL};

// add new ECCutItem in the ECCut_HASH_TABLE
void __attribute__((__used__))
ECCut_hash_insert(unsigned long ref_id, unsigned long target,
                  unsigned long origin, unsigned long originCtx) {
  unsigned long hash_key =
      (ref_id ^ target ^ origin /*^ originCtx*/ ) % HASH_KEY_RANGE;
  cfiItem *item = (cfiItem *)malloc(sizeof(cfiItem));
  item->ref_id = ref_id;
  item->origin = origin;
  item->originCtx = originCtx;
  item->target = target;
  item->next = NULL;

  if (CFI_HASH_TABLE[hash_key] == NULL) {
    CFI_HASH_TABLE[hash_key] = item;
  } else {
    cfiItem *temp = CFI_HASH_TABLE[hash_key];
    while (temp->next != NULL) {
      temp = temp->next;
    }
    temp->next = item;
  }
}

// initialize the hash table at the beginning of the program execution
void __attribute__((__used__))
  ECCut_init(void) {
  buffer = mmap(NULL, sizeof(unsigned long)*10000*4, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  pkey = pkey_alloc(0, 0);
  pkey_set(pkey, 0);
  pkey_mprotect(buffer, sizeof(unsigned long)*10000*4, PROT_WRITE | PROT_READ, pkey);
  pkey_set(pkey, PKEY_DISABLE_ACCESS);
}

// update mpk table
void __attribute__((__used__))
update_mpx_table(unsigned long ptr_addr, unsigned long ptr_val,
                 unsigned long origin, unsigned long originCtx, unsigned long icallnum) {
  int offset, hashptr;
  hashptr = ptr_addr % 10000;
  offset = hashptr * 4;
  pkey_set(pkey, 0);
  while(1) {
    if (buffer[offset] == 0 || buffer[offset] == ptr_addr) {
      buffer[offset] = ptr_addr;
      buffer[offset + 1] = ptr_val;
      buffer[offset + 2] = origin;
      buffer[offset + 3] = originCtx;
      break;
    } else {
      offset += 4;
    }
  }
  pkey_set(pkey, PKEY_DISABLE_ACCESS);
  cfi_stats[0]++;
}

// get entry from mpk table
mEntry __attribute__((__used__))
get_entry_mpx_table(unsigned long ptr_addr, unsigned long ptr_val ) {
  mEntry entry;
  int offset, hashptr;
  hashptr = ptr_addr % 10000;
  offset = hashptr * 4;
  pkey_set(pkey, 0);
  while(1) {
    if (buffer[offset] == ptr_addr && buffer[offset + 1] == ptr_val) {
      entry.origin = buffer[offset + 2];
      entry.originCtx = buffer[offset + 3];
      break;
    } else if (buffer[offset] == 0) {
      break;
    } else {
      offset += 4;
    }
  }
  pkey_set(pkey, PKEY_DISABLE_ACCESS);
  cfi_stats[1]++;
  return entry;
}


void __attribute__((__used__))
pcall_reference_monitor(unsigned long ref_id, unsigned long ptr_addr,
                        unsigned long ptr_val) {
  cfi_stats[4]++;
}

void __attribute__((__used__))
cfi_pcall_ctx_reference_monitor(unsigned long ref_id, unsigned long ptr_addr,
                              unsigned long ptr_val) {
    mEntry entry = get_entry_mpx_table(ptr_addr, ptr_val);
    if (entry.origin == 0) {
        entry = get_entry_mpx_table(ref_id, ptr_val);
    }
    if (entry.origin == 0) {
        fprintf(stderr, "[ECCut-LOG] Something wrong with mpx metadata table\n");
    }
    unsigned long long hash_key =
            ((ref_id ^ ptr_val ^ entry.origin ^ (entry.originCtx+1) ) % HASH_KEY_RANGE);
    if (CFI_HASH_TABLE[hash_key] != NULL) {
        cfiItem *temp = CFI_HASH_TABLE[hash_key];
        while (temp != NULL) {
            if (temp->ref_id == ref_id && temp->target == ptr_val &&
                temp->origin == entry.origin  && temp->originCtx == entry.originCtx+1 ) {
                cfi_stats[3]++;
                return;
            }
            temp = temp->next;
        }
    }
    fprintf(stderr,
            "[ECCut-LOG] Failed validation for <pcall origin with CTX "
            "sensitivity> {%lu "
            "=> %lx} [%lu, %lx]\n",
            ref_id, ptr_val, entry.origin, entry.originCtx+1);
}

void __attribute__((__used__))
cfi_pcall_reference_monitor(unsigned long ref_id, unsigned long ptr_addr,
                              unsigned long ptr_val) {
  mEntry entry = get_entry_mpx_table(ptr_addr, ptr_val);
    if (entry.origin == 0) {
        entry = get_entry_mpx_table(ref_id, ptr_val);
    }
    if (entry.origin == 0) {
        fprintf(stderr, "[ECCut-LOG] Something wrong with mpx metadata table\n");
    }
    unsigned long long hash_key =
            ((ref_id ^ ptr_val ^ entry.origin ) % HASH_KEY_RANGE);
    if (CFI_HASH_TABLE[hash_key] != NULL) {
        cfiItem *temp = CFI_HASH_TABLE[hash_key];
        while (temp != NULL) {
            if (temp->ref_id == ref_id && temp->target == ptr_val &&
                temp->origin == entry.origin ) {
                cfi_stats[3]++;
                return;
            }
            temp = temp->next;
        }
    }

  fprintf(stderr,
          "[ECCut-LOG] Failed validation for <pcall origin w/o CTX "
          "sensitivity> {%lu "
          "=> %lx} [%lu]\n",
          ref_id, ptr_val, entry.origin);
}


void __attribute__((__used__)) ECCut_end() {
  unsigned long i;
  fprintf(stderr, "PRINT END DATA\n");
  fprintf(
      stderr,
      "-----------------------------------------------------------------\n");
  for (i = 0; i < 4; i++) {
    fprintf(stderr, "%-20s%20lu\n", stats_name[i], cfi_stats[i]);
  }
  fprintf(
      stderr,
      "-----------------------------------------------------------------\n");
}

__attribute__((section(".preinit_array"),
               used)) void (*_ocscfi_preinit)(void) = ECCut_init;

__attribute__((section(".fini_array"),
               used)) void (*_ocscfi_fini)(void) = ECCut_end;
