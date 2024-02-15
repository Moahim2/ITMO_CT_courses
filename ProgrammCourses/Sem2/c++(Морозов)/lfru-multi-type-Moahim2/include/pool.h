#pragma once

#include <initializer_list>
#include <new>
#include <vector>

class PoolAllocator
{
    std::size_t m_block_size;
    std::vector<std::size_t> m_sizes;
    std::vector<std::byte> m_storage;
    std::vector<std::vector<bool>> m_used_map;
    void * find_empty_place(std::size_t size);

public:
    PoolAllocator(const std::size_t block_size, std::initializer_list<std::size_t> sizes);

    void * allocate(const std::size_t n);

    void deallocate(const void * ptr);
};
