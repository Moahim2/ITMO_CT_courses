#include "allocator.h"
#include "cache.h"

#include <gtest/gtest.h>

#include <algorithm>
#include <iomanip>
#include <string>
#include <utility>

namespace {

struct WithIntKey
{
    const int key;

    WithIntKey(const int key_)
        : key(key_)
    { }

    virtual ~WithIntKey() = default;

    bool operator == (const int other_key) const
    { return key == other_key; }
};

template <std::size_t Size>
struct StringT : WithIntKey
{
    char dummy_[Size];
    std::string data;

    StringT(const int key_)
        : WithIntKey(key_)
        , data(std::to_string(key_))
    { }

    friend std::ostream & operator << (std::ostream & strm, const StringT & s)
    { return strm << s.key << "[" << s.data << "]"; }
};

struct Point : WithIntKey
{
    unsigned long long data = 0;
    bool marked = false;

    static unsigned long long convert_data(const int key)
    { return key; }

    Point(const int key_)
        : WithIntKey(key_)
        , data(key_)
    {}

    friend std::ostream & operator << (std::ostream & strm, const Point & p)
    { return strm << p.key << "[" << std::hex << std::setw(16) << p.data << "]" << (p.marked ? "{*}" : ""); }
};

template <std::size_t size>
using Size = std::integral_constant<std::size_t, size>;

template <class S>
struct LFRUTest : ::testing::Test
{
    static constexpr std::size_t cache_size = 11;

    Cache<int, WithIntKey, AllocatorWithPool> cache;

    using String = StringT<S::value>;

    static constexpr std::size_t biggest_element = std::max(sizeof(Point), sizeof(String));
    static constexpr std::size_t slab_size = 2 * (cache_size + 1) * biggest_element;

    String & get_string(const int n)
    { return cache.get<String>(n); }

    Point & get_point(const int n)
    { return cache.get<Point>(n); }

    LFRUTest()
        : cache(cache_size, slab_size, std::initializer_list<std::size_t>{sizeof(Point), sizeof(String)})
    {}
};

using TestedTypes = ::testing::Types<Size<1>, Size<9>, Size<500>>;
TYPED_TEST_SUITE(LFRUTest, TestedTypes);

} // anonymous namespace

TYPED_TEST(LFRUTest, empty)
{
    EXPECT_TRUE(this->cache.empty());
    EXPECT_EQ(0, this->cache.size());
}

TYPED_TEST(LFRUTest, add_two_items)
{
    const auto & s1 = this->get_string(1);
    EXPECT_EQ("1", s1.data) << "Wrong item 1: " << s1;
    const auto & s2 = this->get_string(2);
    EXPECT_EQ("2", s2.data) << "Wrong item 2: " << s2;
    EXPECT_FALSE(this->cache.empty());
    EXPECT_EQ(2, this->cache.size());
}

TYPED_TEST(LFRUTest, add_different_items)
{
    auto & s1 = this->get_string(111);
    EXPECT_EQ("111", s1.data) << "Wrong item 1: " << s1;
    s1.data += '@';
    auto & s2 = this->get_point(-111);
    EXPECT_EQ(static_cast<unsigned long long>(-111), s2.data) << "Wrong item 2: " << s2;
    s2.marked = true;

    EXPECT_EQ("111@", this->get_string(111).data);
    EXPECT_TRUE(this->get_point(-111).marked);
}

TYPED_TEST(LFRUTest, add_full)
{
    int n = -5;
    for (std::size_t i = 0; i < this->cache_size; ++i, ++n) {
        auto & s = this->get_string(n);
        const auto expected = std::to_string(n);
        EXPECT_EQ(expected, s.data) << "Wrong item " << n << ": " << s;
        s.data += '@';
    }
    EXPECT_FALSE(this->cache.empty());
    EXPECT_EQ(this->cache_size, this->cache.size());

    n = -5;
    for (std::size_t i = 0; i < this->cache_size; ++i, ++n) {
        const auto & s = this->get_string(n);
        const auto expected = std::to_string(n) + '@';
        EXPECT_EQ(expected, s.data) << "Wrong item " << n << ": " << s;
    }
    EXPECT_EQ(this->cache_size, this->cache.size());
}

TYPED_TEST(LFRUTest, add_full__then_revisit_half__then_add_full)
{
    int n = 10000;
    for (std::size_t i = 0; i < this->cache_size; ++i, --n) {
        auto & s = this->get_string(n);
        const auto expected = std::to_string(n);
        EXPECT_EQ(expected, s.data) << "Wrong item " << n << ": " << s;
        s.data += '@';
    }
    EXPECT_FALSE(this->cache.empty());
    EXPECT_EQ(this->cache_size, this->cache.size());

    n = 10000;
    for (std::size_t i = 0; i < this->cache_size / 2; ++i, n -= 2) {
        auto & s = this->get_string(n);
        const auto expected = std::to_string(n) + '@';
        EXPECT_EQ(expected, s.data) << "Wrong item " << n << ": " << s;
        s.data += '!';
    }
    EXPECT_EQ(this->cache_size, this->cache.size());

    n = -5;
    for (std::size_t i = 0; i < this->cache_size; ++i, ++n) {
        auto & p = this->get_point(n);
        const auto expected = Point::convert_data(n);
        EXPECT_EQ(expected, p.data) << "Wrong item " << n << ": " << p;
        p.marked = true;
    }
    EXPECT_EQ(this->cache_size + this->cache_size / 2, this->cache.size());

    n = 10000;
    for (std::size_t i = 0, end = this->cache_size - this->cache_size % 2; i < end; ++i, --n) {
        const auto & s = this->get_string(n);
        const auto expected = std::to_string(n) + (i % 2 ? "" : "@!");
        EXPECT_EQ(expected, s.data) << "Wrong item " << n << ": " << s;
    }
    for (std::size_t i = 0, end = this->cache_size % 2; i < end; ++i, --n) {
        const auto & s = this->get_string(n);
        const auto expected = std::to_string(n);
        EXPECT_EQ(expected, s.data) << "Wrong item " << n << ": " << s;
    }
    EXPECT_EQ(this->cache_size + this->cache_size / 2, this->cache.size());
}

TYPED_TEST(LFRUTest, single_priority__fill_regular__revisit_regulars)
{
    this->get_string(111).data += '&';
    EXPECT_EQ("111&", this->get_string(111).data);

    int n = -259;
    for (std::size_t i = 0; i < this->cache_size; ++i, --n) {
        this->get_string(n).data += '!';
    }

    EXPECT_EQ(this->cache_size + 1, this->cache.size());
    EXPECT_EQ("111&", this->get_string(111).data);

    for (std::size_t i = 0; i < this->cache_size; ++i, --n) {
        this->get_point(n).marked = true;
    }

    EXPECT_EQ(this->cache_size + 1, this->cache.size());
    EXPECT_EQ("111&", this->get_string(111).data);

    ++n;
    const auto promoted = n;
    EXPECT_TRUE(this->get_point(promoted).marked);
    EXPECT_EQ(this->cache_size + 1, this->cache.size());

    n = -259;
    for (std::size_t i = 0; i < this->cache_size; ++i, --n) {
        const auto expected = std::to_string(n);
        EXPECT_EQ(expected, this->get_string(n).data);
    }

    EXPECT_EQ(this->cache_size + 2, this->cache.size());
    EXPECT_EQ("111&", this->get_string(111).data);
    EXPECT_TRUE(this->get_point(promoted).marked);
}

TYPED_TEST(LFRUTest, fill_priority__flush_regular)
{
    int n = 0;
    for (std::size_t i = 0; i < this->cache_size; ++i, ++n) {
        this->get_string(n).data += '@';
    }
    n = 0;
    for (std::size_t i = 0; i < this->cache_size; ++i, ++n) {
        this->get_string(n).data += '@';
    }
    EXPECT_EQ(this->cache_size, this->cache.size());

    for (std::size_t i = 0; i < this->cache_size * 3; ++i, ++n) {
        this->get_string(n).data += '!';
    }
    EXPECT_EQ(2 * this->cache_size, this->cache.size());

    EXPECT_EQ("0@@", this->get_string(0).data);

    --n;
    for (std::size_t i = 0; i < this->cache_size - 1; ++i, --n) {
        const auto & s = this->get_string(n);
        const auto expected = std::to_string(n) + '!';
        EXPECT_EQ(expected, s.data);
    }
    EXPECT_EQ(2 * this->cache_size, this->cache.size());
    EXPECT_EQ("0@@", this->get_string(0).data);
    EXPECT_EQ("1@@", this->get_string(1).data);

    int m = -1000;
    for (std::size_t i = 0; i < this->cache_size * 5; ++i, --m) {
        auto & p = this->get_point(m);
        const auto expected = static_cast<unsigned long long>(m);
        EXPECT_EQ(expected, p.data);
        p.marked = true;
    }
    EXPECT_EQ(2 * this->cache_size, this->cache.size());
    EXPECT_EQ(std::to_string(n), this->get_string(n).data);

    ++n;
    for (std::size_t i = 0; i < this->cache_size - 2; ++i, ++n) {
        const auto & s = this->get_string(n);
        const auto expected = std::to_string(n) + '!';
        EXPECT_EQ(expected, s.data);
    }
    EXPECT_EQ("0@@", this->get_string(0).data);
    EXPECT_EQ("1@@", this->get_string(1).data);

    ++m;
    for (std::size_t i = 0; i < this->cache_size - 2; ++i, ++m) {
        const auto & p = this->get_point(m);
        const auto expected = static_cast<unsigned long long>(m);
        EXPECT_EQ(expected, p.data);
        EXPECT_TRUE(p.marked);
    }
    EXPECT_EQ("0@@", this->get_string(0).data);
    EXPECT_EQ("1@@", this->get_string(1).data);
}
