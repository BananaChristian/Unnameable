#ifdef _WIN32
#include <windows.h>
#else

#define _POSIX_C_SOURCE 200112L
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/mman.h>
#endif

#include "allocator.h"
#include <stdalign.h>
#include <stdlib.h>


static SageAllocator sage;
static int sage_initialized = 0;

void sage_init(size_t total_size)
{
    if (sage_initialized)
        return;

    sage.os_heap = NULL;
    sage.os_heap_size = total_size;

#ifdef _WIN32
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    sage.page_size = sysInfo.dwPageSize;

    // Round of to the nearest page size
    size_t alloc_size = ((total_size + sage.page_size - 1) / sage.page_size) * sage.page_size;

    sage.os_heap = VirtualAlloc(NULL, alloc_size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

    if (!sage.os_heap)
    {
        sage.os_heap_size = 0;
        return;
    }

    sage.os_heap_size = alloc_size;
#else
    sage.page_size = (size_t)getpagesize();

    size_t alloc_size = ((total_size + sage.page_size - 1) / sage.page_size) * sage.page_size;
    sage.os_heap = mmap(NULL, alloc_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (sage.os_heap == MAP_FAILED)
    {
        sage.os_heap = NULL;
        sage.os_heap_size = 0;
        return;
    }

    sage.os_heap_size = alloc_size;
#endif
    // Initialize the stack heap pointers
    sage.stack.baseptr = (unsigned char *)sage.os_heap;
    sage.stack.frameptr = (unsigned char *)sage.os_heap;
    sage.stack.limit = (unsigned char *)sage.os_heap + sage.os_heap_size;

    // Initialize LIFO
    sage.lifo_count = 0;
    sage.lifo_capacity = 1024;
    sage.lifo = (SageRecord *)malloc(sizeof(SageRecord) * sage.lifo_capacity);
    if (!sage.lifo)
    {
        sage.os_heap = NULL;
        sage.os_heap_size = 0;
        return;
    }
    sage.lifo_count = 0;

    sage_initialized = 1;
}

void *sage_alloc(size_t size, size_t alignment)
{
    if (!sage_initialized || !sage.os_heap)
        return NULL;

    if (alignment == 0)
        alignment = 1; // To avoid dividing by 0 incase we get weird stuff from the caller

    uintptr_t current = (uintptr_t)sage.stack.frameptr;

    // Round up to the next multiple of alignment
    uintptr_t aligned = (current + (alignment - 1)) & ~(alignment - 1);

    // Check for stack overflow
    if ((unsigned char *)(aligned + size) > sage.stack.limit)
        return NULL; // out of memory

    // Address of the object we want to return
    void *object_address = (void *)aligned;

    // Advance the frame pointer
    sage.stack.frameptr = (unsigned char *)(aligned + size);

    // Push the record to LIFO
    if (sage.lifo_count < sage.lifo_capacity)
    {
        SageRecord *record = &sage.lifo[sage.lifo_count++];
        record->start = (unsigned char *)object_address;
        record->size = size;
        record->alignment = alignment;
    }
    else
    {
        return NULL; // LIFO Overflow
    }

    return object_address;
}

void sage_free(void)
{
    if (!sage_initialized || sage.lifo_count == 0)
        return;

    SageRecord last = sage.lifo[--sage.lifo_count];

    uintptr_t current = (uintptr_t)sage.stack.frameptr;
    size_t mask = last.alignment - 1;
    uintptr_t aligned = (current - last.size + mask) & ~mask;

    unsigned char *new_frame = (unsigned char *)aligned;

    // Prevent an underflow
    if (new_frame < sage.stack.baseptr)
        new_frame = sage.stack.baseptr;

// Decommit pages if the platform allows it
#ifdef _WIN32
    {
        uintptr_t old_aligned = current & ~(sage.page_size - 1);
        uintptr_t new_aligned = (aligned + sage.page_size - 1) & ~(sage.page_size - 1);

        if (new_aligned < old_aligned)
        {
            size_t to_decommit = old_aligned - new_aligned;
            void *decommit_start = (void *)new_aligned;
            VirtualFree(decommit_start, to_decommit, MEM_DECOMMIT);
        }
    }
#else
    {
        uintptr_t old_aligned = current & ~(sage.page_size - 1);
        uintptr_t new_aligned = (aligned + sage.page_size - 1) & ~(sage.page_size - 1);

        if (new_aligned < old_aligned)
        {
            size_t to_unmap = old_aligned - new_aligned;
            void *unmap_start = (void *)new_aligned;
            munmap(unmap_start, to_unmap);
        }
    }
#endif

    sage.stack.frameptr = new_frame;
}

void sage_destroy(void)
{
    if (!sage_initialized)
        return; // Not initialized why bother

    if (sage.os_heap)
    {
#ifdef _WIN32
        VirtualFree(sage.os_heap, 0, MEM_RELEASE);
#else
        munmap(sage.os_heap, sage.os_heap_size);
#endif
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