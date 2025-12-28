#include "allocator.h"

#define UCR_PROT_READ 0x1
#define UCR_PROT_WRITE 0x2
#define UCR_MAP_PRIVATE 0x02
#define UCR_MAP_ANONYMOUS 0x20

static SageAllocator sage;
static int sage_initialized = 0;

void sage_init(size_t total_size) {
  if (sage_initialized)
    return;

  sage.page_size = 4096;
  size_t alloc_size =
      ((total_size + sage.page_size - 1) / sage.page_size) * sage.page_size;

  // Talk to the Kernel directly via URC
  sage.os_heap = mmap((void *)0, alloc_size, UCR_PROT_READ | UCR_PROT_WRITE,
                      UCR_MAP_PRIVATE | UCR_MAP_ANONYMOUS, -1, 0);

  if (sage.os_heap == (void *)-1) {
    sage.os_heap = (void *)0;
    sage.os_heap_size = 0;
    return;
  }

  sage.os_heap_size = alloc_size;

  size_t max_possible_records = alloc_size / (sizeof(SageRecord) * 2);

  // Metadata Carve-out, The LIFO lives at the very beginning of the heap
  sage.lifo_capacity =
      (max_possible_records < 1024) ? max_possible_records : 1024;

  if (sage.lifo_capacity == 0 && alloc_size > sizeof(SageRecord)) {
    sage.lifo_capacity = 1;
  }
  size_t lifo_byte_size = sizeof(SageRecord) * sage.lifo_capacity;
  sage.lifo = (SageRecord *)sage.os_heap;
  sage.lifo_count = 0;

  // Data Alignment, The user's stack starts after the LIFO, 16-byte aligned
  uintptr_t data_start = (uintptr_t)sage.os_heap + lifo_byte_size;
  data_start = (data_start + 15) & ~15;

  sage.stack.baseptr = (unsigned char *)data_start;
  sage.stack.frameptr = (unsigned char *)data_start;
  sage.stack.limit = (unsigned char *)sage.os_heap + sage.os_heap_size;

  sage_initialized = 1;
}

void *sage_alloc(size_t size, size_t alignment) {
  if (!sage_initialized || !sage.os_heap)
    return NULL;

  if (alignment == 0)
    alignment =
        1; // To avoid dividing by 0 incase we get weird stuff from the caller

  uintptr_t current = (uintptr_t)sage.stack.frameptr;

  // Round up to the next multiple of alignment
  uintptr_t aligned = (current + (alignment - 1)) & ~(alignment - 1);

  // Check for stack overflow
  if ((unsigned char *)(aligned + size) > sage.stack.limit)
    return NULL; // out of memory

  // Check the record space
  if (sage.lifo_count >= sage.lifo_capacity)
    return NULL;

  // Address of the object we want to return
  void *object_address = (void *)aligned;
  // Advance the frame pointer
  sage.stack.frameptr = (unsigned char *)(aligned + size);

  // Push the record to LIFO
  if (sage.lifo_count < sage.lifo_capacity) {
    SageRecord *record = &sage.lifo[sage.lifo_count++];
    record->start = (unsigned char *)object_address;
    record->size = size;
    record->alignment = alignment;
  } else {
    return NULL; // LIFO Overflow
  }

  return object_address;
}

void sage_free(void) {
  if (!sage_initialized || sage.lifo_count == 0)
    return;

  // Get the record of the last allocation
  SageRecord last = sage.lifo[--sage.lifo_count];

  unsigned char *new_frame = last.start;

  // Underflow check
  if (new_frame < sage.stack.baseptr) {
    new_frame = sage.stack.baseptr;
  }

  // Update the bookmark
  sage.stack.frameptr = new_frame;
}

void sage_destroy(void) {
  if (!sage_initialized)
    return; // Not initialized why bother

  if (sage.os_heap) {
    munmap(sage.os_heap, sage.os_heap_size);
    sage.os_heap = NULL;
    sage.os_heap_size = 0;
  }

  sage.stack.baseptr = NULL;
  sage.stack.frameptr = NULL;
  sage.stack.limit = NULL;

  sage.lifo = NULL;
  sage.lifo_count = 0;
  sage.lifo_capacity = 0;

  sage_initialized = 0;
}
