#include "gpa.h"
#define UCR_PROT_READ 0x1
#define UCR_PROT_WRITE 0x2
#define UCR_MAP_PRIVATE 0x02
#define UCR_MAP_ANONYMOUS 0x20

#define PAGE_SIZE 4096

AllocationData *freeHead = NULL;

void split(AllocationData *originalAlloc, size_t requested_size) {
  AllocationData *newAllocation =
      (AllocationData *)((char *)(originalAlloc) + sizeof(AllocationData) +
                         requested_size);

  // Calculate the info of the remaining chunk after allocation
  newAllocation->size =
      originalAlloc->size - requested_size - sizeof(AllocationData);
  newAllocation->isFree = 1; // Yes it is free
  newAllocation->next =
      originalAlloc->next; // New guy points to original's old friend
  newAllocation->previous = originalAlloc; // New guy points back to original

  // Update the neighbor to point back to the new guy
  if (newAllocation->next != NULL) {
    newAllocation->next->previous = newAllocation;
  }

  originalAlloc->size =
      requested_size; // It now takes on the size of the request like say 4bytes
  originalAlloc->isFree = 0; // It isnt free anymore
  originalAlloc->next =
      newAllocation; // It now points to the bigger newAllocation
}

AllocationData *findFit(size_t requested_size) {
  AllocationData *current = freeHead;
  while (current != NULL) {
    if (current->isFree && current->size >= requested_size) {
      return current;
    }
    current = current->next;
  }
  return NULL;
}

void *alloc(size_t size) {
  AllocationData *block = findFit(size); // Get a suitable block
  if (block == NULL) {
    size_t bytesToRequest = (size + sizeof(AllocationData) > PAGE_SIZE)
                                ? (size + sizeof(AllocationData))
                                : PAGE_SIZE;
    void *ptr = mmap(0, bytesToRequest, UCR_PROT_READ | UCR_PROT_WRITE,
                     UCR_MAP_PRIVATE | UCR_MAP_ANONYMOUS, -1, 0);

    if (ptr == (void *)-1)
      return NULL; // The OS has refused

    block = (AllocationData *)ptr;
    block->size = bytesToRequest - sizeof(AllocationData);
    block->isFree = 1; // Yes it is free

    block->next = freeHead; // Update it to point to NULL
    freeHead = block;       // Make it the head of the free list
  }

  if (block->size >= (size + sizeof(AllocationData) + 8)) {
    split(block, size);
  }

  block->isFree = 0;
  return (void *)(block + 1);
}

void free(void *p) {
  if (!p)
    return;

  AllocationData *data = (AllocationData *)p - 1;
  data->isFree = 1;

  // melt forward
  if (data->next != NULL && data->next->isFree) {
    data->size += sizeof(AllocationData) + data->next->size;
    data->next = data->next->next;
    if (data->next != NULL) {
      data->next->previous = data;
    }
  }

  // melt backward
  if (data->previous != NULL && data->previous->isFree) {
    AllocationData *prev = data->previous;
    prev->size += sizeof(AllocationData) + data->size;
    prev->next = data->next;
    if (data->next != NULL) {
      data->next->previous = prev;
    }
  }
}
