#include "allocator.h"

PoolAllocator::PoolAllocator(const std::size_t block_size, std::initializer_list<std::size_t> sizes)
    : m_block_size(block_size)
    , m_sizes(sizes)
    , m_storage(sizes.size() * block_size)
    , m_used_map(sizes.size(), std::vector<bool>(block_size, false))
{
}

void * PoolAllocator::allocate(const std::size_t n)
{
    void * ptr = find_empty_place(n);
    if (ptr == nullptr) {
        throw std::bad_alloc{};
    }
    return ptr;
}

void PoolAllocator::deallocate(const void * ptr)
{
    if (ptr == nullptr) {
        return;
    }
    const std::byte * b_ptr = static_cast<const std::byte *>(ptr);
    std::size_t pos = b_ptr - &m_storage[0];
    std::size_t number_slab = pos / m_block_size;
    if ((pos - number_slab * m_block_size) % m_sizes[number_slab] == 0 &&
        (&m_storage.front() <= b_ptr && b_ptr <= &m_storage.back())) {
        m_used_map[number_slab][pos - number_slab * m_block_size] = false;
    }
}

void * PoolAllocator::find_empty_place(size_t size)
{
    for (std::size_t i = 0; i < m_sizes.size(); ++i) {
        if (size == m_sizes[i]) {
            for (std::size_t j = 0; j < (m_block_size / size) * size; j += size) {
                if (!m_used_map[i][j]) {
                    m_used_map[i][j] = true;
                    return &(m_storage[i * m_block_size + j]);
                }
            }
        }
    }
    return nullptr;
}
