#pragma once

#include <algorithm>
#include <cstddef>
#include <list>
#include <new>
#include <ostream>

template <class Key, class KeyProvider, class Allocator>
class Cache
{
public:
    template <class... AllocArgs>
    Cache(const std::size_t cache_size, AllocArgs &&... alloc_args)
        : m_max_privileged_size(cache_size)
        , m_max_unprivileged_size(cache_size)
        , m_alloc(std::forward<AllocArgs>(alloc_args)...)
    {
    }

    std::size_t size() const
    {
        return privileged_queue.size() + unprivileged_queue.size();
    }

    bool empty() const
    {
        return unprivileged_queue.empty();
    }

    template <class T>
    T & get(const Key & key);

    std::ostream & print(std::ostream & strm) const;

    friend std::ostream & operator<<(std::ostream & strm, const Cache & cache)
    {
        return cache.print(strm);
    }

private:
    std::list<KeyProvider *> privileged_queue;
    std::list<KeyProvider *> unprivileged_queue;
    const std::size_t m_max_privileged_size;
    const std::size_t m_max_unprivileged_size;
    Allocator m_alloc;

private:
    void freeing_unprivileged_queue();
    void freeing_privileged_queue();
};

template <class Key, class KeyProvider, class Allocator>
template <class T>
inline T & Cache<Key, KeyProvider, Allocator>::get(const Key & key)
{
    const auto keyCmp = [&key](const KeyProvider * ptr) {
        return *ptr == key;
    };

    auto it = std::find_if(privileged_queue.begin(), privileged_queue.end(), keyCmp);
    if (it != privileged_queue.end()) {
        privileged_queue.splice(privileged_queue.begin(), privileged_queue, it);
    }
    else {
        it = std::find_if(unprivileged_queue.begin(), unprivileged_queue.end(), keyCmp);
        if (it != unprivileged_queue.end()) {
            privileged_queue.splice(privileged_queue.begin(), unprivileged_queue, it);
            freeing_privileged_queue();
        }
        else {
            freeing_unprivileged_queue();
            unprivileged_queue.push_front(m_alloc.template create<T>(key));
            return *static_cast<T *>(unprivileged_queue.front());
        }
    }
    return *static_cast<T *>(privileged_queue.front());
}

template <class Key, class KeyProvider, class Allocator>
inline std::ostream & Cache<Key, KeyProvider, Allocator>::print(std::ostream & strm) const
{
    return strm << "Priority: <empty>"
                << "\nRegular: <empty>"
                << "\n";
}

template <class Key, class KeyProvider, class Allocator>
inline void Cache<Key, KeyProvider, Allocator>::freeing_unprivileged_queue()
{
    if (unprivileged_queue.size() == m_max_unprivileged_size) {
        m_alloc.template destroy<Key>(unprivileged_queue.back());
        unprivileged_queue.pop_back();
    }
}

template <class Key, class KeyProvider, class Allocator>
inline void Cache<Key, KeyProvider, Allocator>::freeing_privileged_queue()
{
    if (privileged_queue.size() > m_max_privileged_size) {
        freeing_unprivileged_queue();
        unprivileged_queue.push_front(privileged_queue.back());
        privileged_queue.pop_back();
    }
}
