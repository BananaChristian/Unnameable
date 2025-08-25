#include "allocator.hpp"

Allocator::Allocator(size_t total_heap_size)
{
    // Requesting for memory from the OS
    os_heap = malloc(total_heap_size);

    // Getting the actual size of what the OS gave us and storing it in our heap_size variable
    os_heap_size = total_heap_size;

    // Initialise the stack heap pointers
    // These two all start at the base pointer given to us by the OS
    general_stack_heap.baseptr = os_heap;
    general_stack_heap.frameptr = os_heap;

    // This is the pointer to the end of the heap
    general_stack_heap.limit = (char *)os_heap + os_heap_size;
}

// STACK HEAP METHODS
void *Allocator::sage_alloc(size_t component_size)
{
    // Let us check if the frame pointer has not exceeded the limit pointer
    if ((char *)general_stack_heap.frameptr + component_size > (char *)general_stack_heap.limit)
    {
        return nullptr;
    }
    // Getting the current location of the frame ptr
    void *object_address = general_stack_heap.frameptr;

    // Advancing the frame pointer to the new location
    general_stack_heap.frameptr = (char *)general_stack_heap.frameptr + component_size;

    return object_address;
}

void Allocator::bulk_free(size_t component_size)
{
    // Here we shall move the frame pointer backwards by the size of the component being freed
    if ((char *)general_stack_heap.frameptr - component_size < (char *)general_stack_heap.baseptr)
    {
        return;
    }
    general_stack_heap.frameptr = (char *)general_stack_heap.frameptr - component_size;
}

// RAW HEAP METHODS
// The raw heap shall request for its own memory when the user promotes an object to it
void *Allocator::heap_alloc(size_t object_size)
{
    // Request for memory from the OS using malloc and return the pointer
    void *heap_ptr = malloc(object_size);
    return heap_ptr;
}

void Allocator::heap_free(void *ptr)
{
    // Just calling free
    free(ptr);
}

Allocator::~Allocator()
{
    free(os_heap);
}