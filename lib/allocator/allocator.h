#ifndef ALLOCATOR_H
#define ALLOCATOR_H
#include <stddef.h>
#include <stdint.h>

typedef struct
{
    unsigned char *baseptr;  // Pointer at the start of the memory allocated for the stack
    unsigned char *frameptr; // Pointer that looks at the current object
    unsigned char *limit;    // End of the given memory block
} StackHeap;

typedef struct
{
    unsigned char *start;
    size_t size;
    size_t alignment;
} SageRecord;

typedef struct
{
    void *os_heap;       // Pointer to the heap region the OS has given
    size_t os_heap_size; // The actual size of the os_heap
    size_t page_size;    // The actual page size

    StackHeap stack;

    SageRecord *lifo;

    size_t lifo_count;
    size_t lifo_capacity;
} SageAllocator;

//URC forward declarations
void* mmap(void* addr, size_t len, int prot, int flags, int fd, long offset);
int munmap(void* addr, size_t len);

void sage_init(size_t total_size);
void *sage_alloc(size_t size, size_t alignment);
void sage_free();
void sage_destroy();

#endif