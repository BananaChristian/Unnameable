#ifndef GPA_H
#define GPA_H
#include <stddef.h>
#include <stdint.h>

typedef struct AllocationData AllocationData;

struct AllocationData {
  AllocationData *previous; // Pointer to the allocation before
  size_t size;              // Size of the allocation say i32 is 4bytes
  int isFree;               // Is the allocation block taken
  AllocationData *next;     // Pointer to the next allocation
};

// URC forward declarations
void *mmap(void *addr, size_t len, int prot, int flags, int fd, long offset);
int munmap(void *addr, size_t len);

// Main interface functions
void *alloc(size_t size);
void free(void *p);

// Helpers
void split(AllocationData *orginalAlloc,
           size_t requested_size); // Split bigger blocks into smaller ones

AllocationData *findFit(size_t requested_size); // Find a proper block

#endif
