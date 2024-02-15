#pragma once

#include "policy.h"

template <
        class Key,
        class CollisionPolicy = LinearProbing,
        class Hash = std::hash<Key>,
        class Equal = std::equal_to<Key>>
class HashSet
{
public:
    // types
    using key_type = ...;
    using value_type = ...;
    using size_type = ...;
    using difference_type = ...;
    using hasher = ...;
    using key_equal = ...;
    using reference = ...;
    using const_reference = ...;
    using pointer = ...;
    using const_pointer = ...;

    using iterator = ...;
    using const_iterator = ...;

    explicit HashSet(size_type expected_max_size = 0,
                     const hasher & hash = hasher(),
                     const key_equal & equal = key_equal());

    template <class InputIt>
    HashSet(InputIt first, InputIt last, size_type expected_max_size = 0, const hasher & hash = hasher(), const key_equal & equal = key_equal());

    HashSet(const HashSet &);
    HashSet(HashSet &&);

    HashSet(std::initializer_list<value_type> init,
            size_type expected_max_size = 0,
            const hasher & hash = hasher(),
            const key_equal & equal = key_equal());

    HashSet & operator=(const HashSet &);
    HashSet & operator=(HashSet &&) noexcept;
    HashSet & operator=(std::initializer_list<value_type> init);

    iterator begin() noexcept;
    const_iterator begin() const noexcept;
    const_iterator cbegin() const noexcept;

    iterator end() noexcept;
    const_iterator end() const noexcept;
    const_iterator cend() const noexcept;

    bool empty() const;
    size_type size() const;
    size_type max_size() const;

    void clear();
    std::pair<iterator, bool> insert(const value_type & key);
    std::pair<iterator, bool> insert(value_type && key);
    iterator insert(const_iterator hint, const value_type & key);
    iterator insert(const_iterator hint, value_type && key);
    template <class InputIt>
    void insert(InputIt first, InputIt last);
    void insert(std::initializer_list<value_type> init);

    // construct element in-place, no copy or move operations are performed;
    // element's constructor is called with exact same arguments as `emplace` method
    // (using `std::forward<Args>(args)...`)
    template <class... Args>
    std::pair<iterator, bool> emplace(Args &&... args);
    template <class... Args>
    iterator emplace_hint(const_iterator hint, Args &&... args);

    iterator erase(const_iterator pos);
    iterator erase(const_iterator first, const_iterator last);
    size_type erase(const key_type & key);

    // exchanges the contents of the container with those of other;
    // does not invoke any move, copy, or swap operations on individual elements
    void swap(HashSet & other) noexcept;

    size_type count(const key_type & key) const;
    iterator find(const key_type & key);
    const_iterator find(const key_type & key) const;
    bool contains(const key_type & key) const;
    std::pair<iterator, iterator> equal_range(const key_type & key);
    std::pair<const_iterator, const_iterator> equal_range(const key_type & key) const;

    size_type bucket_count() const;
    size_type max_bucket_count() const;
    size_type bucket_size(const size_type) const;
    size_type bucket(const key_type & key) const;

    float load_factor() const;
    float max_load_factor() const;
    void rehash(const size_type count);
    void reserve(size_type count);

    // compare two containers contents
    friend bool operator==(const HashSet & lhs, const HashSet & rhs);
    friend bool operator!=(const HashSet & lhs, const HashSet & rhs);
};
