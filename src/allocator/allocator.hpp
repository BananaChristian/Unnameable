#pragma once
#include <unordered_map>
#include <string>
#include <vector>


// This is the structure of the general stack heap
struct StackHeap
{
    void *baseptr = nullptr;  // This is the pointer to the start of the stack
    void *frameptr = nullptr; // This is the pointer to the current object
    void *limit = nullptr;    // This is the end of the memory block
};

struct SageRecord
{
    void *start;
    size_t size;
    size_t alignment;
};

class Allocator
{
private:
    void *os_heap = nullptr; // This is a pointer to the heap given to us by the OS
    size_t os_heap_size = 0; // The actual size of the os_heap
    size_t pagesize;         // Storing the actual page size

    StackHeap general_stack_heap;

    std::vector<SageRecord> sage_lifo;

public:
    Allocator(size_t total_heap_size); // Constructor for the allocator with a total size argument where we shall request memory
    ~Allocator();                      // Destructor for the allocator

    static Allocator *global_allocator; // Global allocator (singleton)

    // STACK HEAP METHODS
    void sage_free();   // This method will pop a stack frame from the stack heap
    void *sage_alloc(size_t component_size, size_t alignment); // This will add a stack frame to the stack heap

    // RAW HEAP METHODS
    void heap_free(void *ptr, size_t object_size); // When the object was last used this method shall free it
    void *heap_alloc(size_t object_size);          // When a user promotes an object to the raw heap this method shall be called
};