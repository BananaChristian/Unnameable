#include "allocator.hpp"
#include <iostream>
#include <cstdint>

Allocator *Allocator::global_allocator = nullptr;

extern "C"
{
    void sage_init(size_t total_size)
    {
        if (Allocator::global_allocator)
            delete Allocator::global_allocator;

        Allocator::global_allocator = new Allocator(total_size);
        std::cout << "[SAGE] Initialized global allocator with "
                  << total_size << " bytes\n";
    }

    void *sage_alloc(size_t size, size_t alignment)
    {
        if (!Allocator::global_allocator)
        {
            std::cerr << "[SAGE ERROR]: Allocator not initialized!\n";
            return nullptr;
        }
        return Allocator::global_allocator->sage_alloc(size, alignment);
    }

    void sage_free()
    {
        if (!Allocator::global_allocator)
        {
            std::cerr << "[SAGE ERROR]: Allocator not initialized!\n";
            return;
        }
        Allocator::global_allocator->sage_free();
    }

    void sage_destroy()
    {
        if (Allocator::global_allocator)
        {
            delete Allocator::global_allocator;
            Allocator::global_allocator = nullptr;
            std::cout << "[SAGE] Destroyed global allocator\n";
        }
    }
}

#ifdef _WIN32
// WINDOWS IMPLEMENTATION
#include <windows.h>

Allocator::Allocator(size_t total_heap_size)
{
    SYSTEM_INFO sysInfo;
    GetSystemInfo(&sysInfo);
    pagesize = sysInfo.dwPageSize;

    os_heap_size = ((total_heap_size + pagesize - 1) / pagesize) * pagesize;
    os_heap = VirtualAlloc(nullptr, os_heap_size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (!os_heap)
    {
        std::cerr << "VirtualAlloc failed with error code: " << GetLastError() << "\n";
        os_heap_size = 0;
        return;
    }

    general_stack_heap.baseptr = os_heap;
    general_stack_heap.frameptr = os_heap;
    general_stack_heap.limit = (char *)os_heap + os_heap_size;

    std::cout << "Heap granted (VirtualAlloc): " << os_heap_size
              << " bytes, starting at: " << os_heap
              << ", ending at: " << general_stack_heap.limit << "\n";
}

void *Allocator::sage_alloc(size_t component_size, size_t alignment)
{

    uintptr_t current = (uintptr_t)general_stack_heap.frame_ptr;
    uintptr_t aligned = (current + (alignment + 1)) & ~(alignment - 1);

    if ((char *)aligned + component_size > (char *)general_stack_heap.limit)
    {
        std::cout << "sage_alloc -> Hit allocated memory limit\n";
        return nullptr;
    }

    void *object_address = (void *)aligned;

    general_stack_heap.frameptr = (char *)aligned + component_size;

    sage_lifo.push_back({object_address, component_size, alignment});

    std::cout << "sage_alloc -> Allocated " << component_size
              << " bytes at " << object_address
              << ", new frameptr: " << general_stack_heap.frameptr << "\n";

    return object_address;
}

void Allocator::sage_free(size_t component_size, size_t alignment)
{
    if (sage_lifo.empty())
    {
        std::cout << "sage_free -> nothing to free\n";
        return;
    }

    auto last=sage_lifo.back();
    sage_lifo.pop_back();

    uintptr_t current = (unintptr_t)general_stack_heap.frameptr;
    size_t mask = last.alignment - 1;
    uintptr_t aligned = (current - last.size + mask) & ~mask;

    char *new_frame = (char *)aligned;
    if (new_frame < (char *)general_stack_heap.baseptr)
    {
        std::cout << "sage_free -> Underflow, cannot free "
                  << last.size << " bytes\n";
        return;
    }

    uintptr_t new_frame_addr = (uintptr_t)new_frame;

    uintptr_t old_aligned = current & ~(pagesize - 1);
    uintptr_t new_aligned = (new_frame_addr + pagesize - 1) & ~(pagesize - 1);

    if (new_aligned < old_aligned)
    {
        size_t to_decommit = old_aligned - new_aligned;
        void *decommit_start = (void *)new_aligned;

        if (!VirtualFree(decommit_start, to_decommit, MEM_DECOMMIT))
        {
            std::cerr << "sage_free -> VirtualFree failed with code: "
                      << GetLastError() << "\n";
        }
        else
        {
            std::cout << "sage_free -> Decommitted " << to_decommit
                      << " bytes starting at " << decommit_start << "\n";
        }
    }

    general_stack_heap.frameptr = new_frame;

    std::cout << "sage_free -> Freed " << last.size
              << " bytes, new frameptr: " << general_stack_heap.frameptr << "\n";
}

void *Allocator::heap_alloc(size_t object_size)
{
    void *heap_ptr = VirtualAlloc(nullptr, object_size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (!heap_ptr)
    {
        std::cerr << "heap_alloc -> VirtualAlloc failed with code: "
                  << GetLastError() << "\n";
        return nullptr;
    }

    std::cout << "heap_alloc -> Allocated " << object_size
              << " bytes at " << heap_ptr << "\n";
    return heap_ptr;
}

void Allocator::heap_free(void *ptr, size_t object_size)
{
    (void)object_size;
    if (!VirtualFree(ptr, 0, MEM_RELEASE))
    {
        std::cerr << "heap_free -> VirtualFree failed with code: "
                  << GetLastError() << "\n";
    }
    else
    {
        std::cout << "heap_free -> Released memory at " << ptr << "\n";
    }
}

Allocator::~Allocator()
{
    if (os_heap)
    {
        if (!VirtualFree(os_heap, 0, MEM_RELEASE))
        {
            std::cerr << "Destroying Allocator -> VirtualFree failed with code: "
                      << GetLastError() << "\n";
        }
        else
        {
            std::cout << "Destroying Allocator -> Released entire heap at "
                      << os_heap << " (" << os_heap_size << " bytes)\n";
        }
        os_heap = nullptr;
        os_heap_size = 0;
    }
}

#else
// LINUX IMPLEMENTATION
#include <sys/mman.h>
#include <unistd.h>

Allocator::Allocator(size_t total_heap_size)
{
    pagesize = getpagesize();
    os_heap_size = ((total_heap_size + pagesize - 1) / pagesize) * pagesize;

    os_heap = mmap(nullptr, os_heap_size, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (os_heap == MAP_FAILED)
    {
        perror("mmap failed");
        os_heap = nullptr;
        os_heap_size = 0;
        return;
    }

    general_stack_heap.baseptr = os_heap;
    general_stack_heap.frameptr = os_heap;
    general_stack_heap.limit = (char *)os_heap + os_heap_size;

    std::cout << "Heap granted (mmap): " << os_heap_size
              << " bytes, starting at: " << os_heap
              << ", ending at: " << general_stack_heap.limit << "\n";
}

void *Allocator::sage_alloc(size_t component_size, size_t alignment)
{
    uintptr_t current = (uintptr_t)general_stack_heap.frameptr;
    uintptr_t aligned = (current + (alignment - 1)) & ~(alignment - 1);

    if ((char *)aligned + component_size > (char *)general_stack_heap.limit)
    {
        std::cout << "sage_alloc -> Hit allocated memory limit\n";
        return nullptr;
    }

    void *object_address = (void *)aligned;

    general_stack_heap.frameptr = (char *)aligned + component_size;

    sage_lifo.push_back({object_address, component_size, alignment});

    std::cout << "sage_alloc -> Allocated " << component_size << " bytes at "
              << object_address << ", new frameptr: " << general_stack_heap.frameptr << "\n";

    return object_address;
}

void Allocator::sage_free()
{

    if (sage_lifo.empty())
    {
        std::cout << "sage_free -> nothing to free\n";
        return;
    }

    auto last = sage_lifo.back();
    sage_lifo.pop_back();

    uintptr_t current = (uintptr_t)general_stack_heap.frameptr;
    size_t mask = last.alignment - 1;
    uintptr_t aligned = (current - last.size + mask) & ~mask;

    char *new_frame = (char *)aligned;
    if (new_frame < (char *)general_stack_heap.baseptr)
    {
        std::cout << "sage_free -> Underflow, cannot free "
                  << last.size << " bytes\n";
        return;
    }

    uintptr_t new_frame_addr = (uintptr_t)new_frame;

    uintptr_t old_aligned = current & ~(pagesize - 1);
    uintptr_t new_aligned = (new_frame_addr + pagesize - 1) & ~(pagesize - 1);

    if (new_aligned < old_aligned)
    {
        size_t to_unmap = old_aligned - new_aligned;
        void *unmap_start = (void *)new_aligned;

        if (munmap(unmap_start, to_unmap) != 0)
        {
            perror("munmap failed in sage_free");
        }
        else
        {
            std::cout << "sage_free -> Unmapped " << to_unmap
                      << " bytes starting at " << unmap_start << "\n";
        }
    }

    general_stack_heap.frameptr = new_frame;

    std::cout << "sage_free -> Freed " << last.size
              << " bytes, new frameptr: " << general_stack_heap.frameptr << "\n";
}

void *Allocator::heap_alloc(size_t object_size)
{
    void *heap_ptr = mmap(nullptr, object_size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (heap_ptr == MAP_FAILED)
    {
        perror("heap_alloc mmap failed");
        return nullptr;
    }
    std::cout << "heap_alloc -> Allocated " << object_size
              << " bytes at " << heap_ptr << "\n";
    return heap_ptr;
}

void Allocator::heap_free(void *ptr, size_t object_size)
{
    if (munmap(ptr, object_size) != 0)
    {
        perror("heap_free munmap failed");
    }
    else
    {
        std::cout << "heap_free -> Freed " << object_size
                  << " bytes at " << ptr << "\n";
    }
}

Allocator::~Allocator()
{
    if (os_heap)
    {
        munmap(os_heap, os_heap_size);
        std::cout << "Destroying Allocator -> Unmapped entire heap at "
                  << os_heap << " (" << os_heap_size << " bytes)\n";
        os_heap = nullptr;
        os_heap_size = 0;
    }
}
#endif
