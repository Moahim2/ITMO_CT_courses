#include "hash_map.h"
#include "utils.h"
#include "test_iterator.h"

#include <gtest/gtest.h>

#include <algorithm>
#include <list>
#include <mutex>
#include <random>
#include <set>
#include <thread>
#include <type_traits>

using namespace test_utils;

namespace {

struct CountingHashMapTest : ::testing::Test
{
    HashMap<int, ConstructionAware, LinearProbing> map;
};

template <class Policy>
struct BadHashingHashMapTest : ::testing::Test
{
    HashMap<std::string, std::size_t, Policy, BadHash> map;
};

struct ComplexConstructionHashMapTest : ::testing::Test
{
    HashMap<std::string, std::string> map;
};

template <class T, class Key>
struct HashMapTestT : ::testing::Test
{
    Converter<Key> keys;
    Converter<T> values;

    using Value = std::pair<Key, T>;
    Value create(const int n) const
    {
        return {keys.create(n), values.create(n)};
    }

    template <class P>
    Value copy(P && p) const
    {
        return {keys.copy(p.first), values.copy(p.second)};
    }

    auto copy_emplace(const Value & v)
    {
        return map.emplace(keys.copy(v.first), values.copy(v.second));
    }

    auto emplace(const int n)
    {
        return map.emplace(keys.create(n), values.create(n));
    }

    using Map = HashMap<Key, T>;
    Map map;

    // Initialize sample data for generic iterator tests
    Map & not_empty_container()
    {
        if (map.empty()) {
            for (int i = 0; i < 991; ++i) {
                emplace(i);
            }
        }
        return map;
    }
};

template <class Key>
using HashMapTest = HashMapTestT<NonCopyable, Key>;
template <class Key>
using HashMapTest_CopyableElems = HashMapTestT<int, Key>;

using TestedTypes = ::testing::Types<int, std::string, NonCopyable>;
using CopyableTestedTypes = ::testing::Types<int, std::string>;
using BadHashPolicies = ::testing::Types<LinearProbing, QuadraticProbing>;
TYPED_TEST_SUITE(HashMapTest, TestedTypes);
TYPED_TEST_SUITE(HashMapTest_CopyableElems, CopyableTestedTypes);
TYPED_TEST_SUITE(BadHashingHashMapTest, BadHashPolicies);

} // anonymous namespace

TYPED_TEST(HashMapTest, check_types)
{
    using Map = typename std::remove_pointer_t<decltype(this)>::Map;
    using Value = std::remove_reference_t<decltype(*std::declval<Map>().begin())>;
    using ConstValue = std::remove_reference_t<decltype(*std::declval<Map>().cbegin())>;
    using ConstValue2 = std::remove_reference_t<decltype(*std::declval<const Map>().begin())>;
    using ExpectedValue = std::pair<const TypeParam, NonCopyable>;
    static_assert(std::is_same_v<TypeParam, typename Map::key_type>,
            "key_type should be equal to key type parameter");
    static_assert(std::is_same_v<NonCopyable, typename Map::mapped_type>,
            "mapped_type should be equal to NonCopyable");
    static_assert(std::is_same_v<ExpectedValue, typename Map::value_type>,
            "value_type should be equal to a pair of const key type parameter and NonCopyable");
    static_assert(std::is_same_v<ExpectedValue *, typename Map::pointer>,
            "pointer should be equal to pointer to a pair of const key type parameter and NonCopyable");
    static_assert(std::is_same_v<ExpectedValue &, typename Map::reference>,
            "reference should be equal to reference to a pair of const key type parameter and NonCopyable");
    static_assert(std::is_constructible_v<typename Map::const_iterator, typename Map::iterator>,
            "const_iterator should be constructible from iterator");
    static_assert(std::is_same_v<ExpectedValue, Value>,
            "iteration should give a pair of const key type parameter and NonCopyable");
    static_assert(std::is_same_v<const ExpectedValue, ConstValue>,
            "const iteration should give a const pair of const key type parameter and NonCopyable");
    static_assert(std::is_same_v<const ExpectedValue, ConstValue2>,
            "const iteration should give a const pair of const key type parameter and NonCopyable");
}

TYPED_TEST(HashMapTest, empty)
{
    EXPECT_TRUE(this->map.empty());
    EXPECT_EQ(0, this->map.size());
    {
        std::size_t count = 0;
        for ([[maybe_unused]] const auto & x : this->map) {
            ++count;
        }
        EXPECT_EQ(0, count);
    }
    EXPECT_FALSE(this->map.contains(this->keys.create(0)));
    EXPECT_FALSE(this->map.contains(this->keys.create(1)));
    EXPECT_FALSE(this->map.contains(this->keys.create(101)));
    EXPECT_EQ(this->map.end(), this->map.find(this->keys.create(3)));
    {
        const auto [begin, end] = this->map.equal_range(this->keys.create(5));
        EXPECT_EQ(end, begin);
        EXPECT_EQ(this->map.end(), begin);
    }
}

TYPED_TEST(HashMapTest, max_size)
{
    EXPECT_LE(this->map.max_size(), this->map.max_bucket_count());
}

TYPED_TEST(HashMapTest, reserve_does_not_change_size)
{
    this->map.reserve(907);
    EXPECT_TRUE(this->map.empty());
    EXPECT_EQ(0, this->map.size());
}

TYPED_TEST(HashMapTest, singleton)
{
    const auto key = this->keys.create(0);
    const auto value = this->values.create(0);
    this->map.emplace(this->keys.copy(key), this->values.copy(value));
    EXPECT_FALSE(this->map.empty());
    EXPECT_EQ(1, this->map.size());
    {
        std::size_t count = 0;
        for (const auto & [k, v] : this->map) {
            EXPECT_EQ(k, key);
            EXPECT_EQ(v, value);
            ++count;
        }
        EXPECT_EQ(1, count);
    }
    EXPECT_TRUE(this->map.contains(key));
    EXPECT_NE(this->map.end(), this->map.find(key));
    EXPECT_EQ(this->map.begin(), this->map.find(key));
    {
        auto [begin, end] = this->map.equal_range(key);
        EXPECT_EQ(this->map.begin(), begin);
        EXPECT_NE(begin, end);
        ++begin;
        EXPECT_EQ(begin, end);
    }
}

TYPED_TEST(HashMapTest, many)
{
    const auto present_elements = {this->create(0), this->create(1), this->create(2), this->create(3), this->create(4)};
    const auto missing_elements = {this->create(5), this->create(6), this->create(7), this->create(8), this->create(9)};
    for (const auto & x : present_elements) {
        const auto [it, success] = this->copy_emplace(x);
        EXPECT_TRUE(success);
        EXPECT_EQ(x.first, it->first);
        EXPECT_EQ(x.second, it->second);
    }
    EXPECT_EQ(present_elements.size(), this->map.size());
    for (const auto & x : present_elements) {
        EXPECT_TRUE(this->map.contains(x.first));
        const auto [it, success] = this->copy_emplace(x);
        EXPECT_FALSE(success);
        EXPECT_EQ(x.first, it->first);
        EXPECT_EQ(x.second, it->second);
    }
    EXPECT_EQ(present_elements.size(), this->map.size());
    for (const auto & y : missing_elements) {
        EXPECT_FALSE(this->map.contains(y.first));
        EXPECT_EQ(this->map.end(), this->map.find(y.first));
        {
            const auto [begin, end] = this->map.equal_range(y.first);
            EXPECT_EQ(begin, end);
            EXPECT_EQ(this->map.end(), begin);
        }
    }
}

TYPED_TEST(HashMapTest, move_construct)
{
    using Map = std::remove_reference_t<decltype(this->map)>;
    const int max = 7777;
    for (int i = 0; i < max; ++i) {
        this->emplace(i);
    }
    Map to_map = std::move(this->map);
    this->map = Map{};
    this->emplace(-3);
    this->emplace(3);
    this->emplace(-11);
    this->emplace(15);
    EXPECT_EQ(4, this->map.size());
    EXPECT_EQ(max, to_map.size());
    EXPECT_TRUE(this->map.contains(this->keys.create(3)));
    EXPECT_FALSE(this->map.contains(this->keys.create(4)));
    EXPECT_TRUE(this->map.contains(this->keys.create(-3)));
    EXPECT_FALSE(this->map.contains(this->keys.create(0)));
    EXPECT_TRUE(this->map.contains(this->keys.create(-11)));
    EXPECT_FALSE(this->map.contains(this->keys.create(10)));
    EXPECT_TRUE(this->map.contains(this->keys.create(15)));
    EXPECT_FALSE(this->map.contains(this->keys.create(14)));
    for (int i = max; i > 0; --i) {
        EXPECT_TRUE(to_map.contains(this->keys.create(i-1)));
    }
}

TYPED_TEST(HashMapTest, move_assign)
{
    using Map = std::remove_reference_t<decltype(this->map)>;
    Map from_map;
    const int max = 7777;
    for (int i = 0; i < max; ++i) {
        from_map.emplace(this->create(i));
    }
    this->emplace(-9);
    this->emplace(-1);
    this->emplace(-29);
    this->map = std::move(from_map);
    from_map = Map{};
    from_map.emplace(this->create(-5));
    from_map.emplace(this->create(5));
    EXPECT_EQ(max, this->map.size());
    EXPECT_EQ(2, from_map.size());
    EXPECT_TRUE(from_map.contains(this->keys.create(-5)));
    EXPECT_TRUE(from_map.contains(this->keys.create(5)));
    EXPECT_FALSE(from_map.contains(this->keys.create(15)));
    EXPECT_FALSE(this->map.contains(this->keys.create(-1)));
    EXPECT_FALSE(this->map.contains(this->keys.create(-9)));
    EXPECT_FALSE(this->map.contains(this->keys.create(-29)));
    for (int i = max; i > 0; --i) {
        EXPECT_TRUE(this->map.contains(this->keys.create(i-1)));
    }
}

TYPED_TEST(HashMapTest, iter_through)
{
    const auto max = 797;
    for (int i = 0; i < max; ++i) {
        EXPECT_TRUE(this->emplace(i).second);
    }
    const auto end = this->map.end();
    for (int i = 0; i < max; ++i) {
        EXPECT_NE(end, this->map.find(this->keys.create(i)));
    }
    std::set<int> keys;
    std::size_t count = 0;
    for (const auto & [k, v] : this->map) {
        const int n = this->keys.value(k);
        EXPECT_EQ(n, this->values.value(v));
        EXPECT_LE(0, n);
        EXPECT_GT(max, n);
        keys.insert(n);
        ++count;
    }
    EXPECT_EQ(max, keys.size());
    EXPECT_EQ(max, count);
    EXPECT_EQ(max, this->map.size());
}

TYPED_TEST(HashMapTest, buckets)
{
    const auto max = 773;
    for (int i = 0; i < max; ++i) {
        EXPECT_TRUE(this->emplace(i).second);
    }
    const auto bucket_num = this->map.bucket_count();
    EXPECT_LE(max, bucket_num);
    for (int i = 0; i < max; ++i) {
        const auto bucket_i = this->map.bucket(this->keys.create(i));
        EXPECT_LE(0, bucket_i);
        EXPECT_GT(bucket_num, bucket_i);
        EXPECT_EQ(1, this->map.bucket_size(bucket_i));
    }
    for (std::size_t k = 0; k < this->map.bucket_count(); ++k) {
        EXPECT_GE(1, this->map.bucket_size(k));
    }
}

TYPED_TEST(HashMapTest, load_factor_and_rehash)
{
    EXPECT_GE(1.0, this->map.load_factor());

    this->emplace(1);
    this->emplace(2);
    this->emplace(3);

    float lf = this->map.size();
    lf /= this->map.bucket_count();
    EXPECT_FLOAT_EQ(lf, this->map.load_factor());
    EXPECT_GE(1.0, this->map.load_factor());

    std::size_t buckets = static_cast<std::size_t>(this->map.size() / this->map.max_load_factor());
    this->map.rehash(0);
    EXPECT_LE(buckets, this->map.bucket_count());

    const std::size_t new_size = 1259;
    buckets = static_cast<std::size_t>(new_size / this->map.max_load_factor());
    this->map.reserve(new_size);
    EXPECT_LE(buckets, this->map.bucket_count());
    EXPECT_GE(1.0, this->map.load_factor());
}

TYPED_TEST(HashMapTest, add_and_remove)
{
    const auto elements = {this->create(0), this->create(1), this->create(2), this->create(3), this->create(4), this->create(5), this->create(6), this->create(7)};
    for (const auto & x : elements) {
        this->copy_emplace(x);
    }
    {
        int i = 0;
        for (const auto & x : elements) {
            if (i % 2) {
                EXPECT_EQ(1, this->map.erase(x.first));
            }
            ++i;
        }
    }
    EXPECT_EQ(elements.size() / 2, this->map.size());
    {
        int i = 0;
        for (const auto & x : elements) {
            if (i % 2) {
                EXPECT_FALSE(this->map.contains(x.first));
            }
            else {
                EXPECT_TRUE(this->map.contains(x.first));
            }
            ++i;
        }
    }
    {
        auto first = this->map.begin();
        const auto end = this->map.end();
        ++first;
        auto last = first;
        for (auto next = last; next != end; ++next) {
            last = next;
        }
        const auto first_el = this->copy(*this->map.begin());
        const auto last_el = this->copy(*last);
        EXPECT_EQ(2, std::distance(first, last));
        this->map.erase(first, last);
        EXPECT_EQ(2, this->map.size());
        EXPECT_TRUE(this->map.contains(first_el.first));
        EXPECT_TRUE(this->map.contains(last_el.first));
    }
}

TYPED_TEST(HashMapTest, erase)
{
    for (int i = 0; i < 50; ++i) {
        EXPECT_TRUE(this->emplace(i).second);
    }
    EXPECT_EQ(50, this->map.size());
    for (int i = 0; i < 50; i += 2) {
        const auto it = this->map.find(this->keys.create(i));
        EXPECT_NE(this->map.end(), it);
        this->map.erase(it);
    }
    EXPECT_EQ(25, this->map.size());
    for (int i = 0; i < 50; i += 2) {
        const auto it = this->map.find(this->keys.create(i));
        EXPECT_EQ(this->map.end(), it);
    }
    for (int i = 1; i < 50; i += 2) {
        const auto it = this->map.find(this->keys.create(i));
        EXPECT_NE(this->map.end(), it);
    }
}

TYPED_TEST(HashMapTest, emplace_hint)
{
    auto hint = this->map.end();
    const int max = 1009;
    for (int i = 0; i < max; ++i) {
        hint = this->map.emplace_hint(hint, this->keys.create(i), this->values.create(i));
        hint = this->map.emplace_hint(hint, this->keys.create(i), this->values.create(i));
        hint = this->map.emplace_hint(hint, this->keys.create(i), this->values.create(i));
    }
    for (int i = 0; i < max; ++i) {
        EXPECT_TRUE(this->map.contains(this->keys.create(i)));
    }
}

TYPED_TEST(HashMapTest, insert_or_assign)
{
    auto hint = this->map.end();
    const int max = 29;
    for (int i = 0; i < max; ++i) {
        hint = this->map.insert_or_assign(hint, this->keys.create(i), this->values.create(i));
    }
    for (int i = 0; i < max; ++i) {
        EXPECT_TRUE(this->map.contains(this->keys.create(i)));
        const auto it = this->map.find(this->keys.create(i));
        EXPECT_EQ(i, this->values.value(it->second));
    }
    for (int i = 0; i < max; ++i) {
        const auto [it, success] = this->map.insert_or_assign(this->keys.create(i), this->values.create(i * 100));
        EXPECT_FALSE(success);
        EXPECT_EQ(i * 100, this->values.value(it->second));
    }
}

TYPED_TEST(HashMapTest, try_emplace)
{
    auto hint = this->map.end();
    const int max = 31;
    for (int i = 0; i < max; ++i) {
        hint = this->map.try_emplace(hint, this->keys.create(i), this->values.create(i));
    }
    for (int i = 0; i < max; ++i) {
        EXPECT_TRUE(this->map.contains(this->keys.create(i)));
        const auto it = this->map.find(this->keys.create(i));
        EXPECT_EQ(i, this->values.value(it->second));
    }
    for (int i = 0; i < max; ++i) {
        const auto [it, success] = this->map.try_emplace(this->keys.create(i), this->values.create(i * 100));
        EXPECT_FALSE(success);
        EXPECT_EQ(i, this->values.value(it->second));
    }
}

TYPED_TEST(HashMapTest, no_iterator_invalidation)
{
    this->map.reserve(2000);
    const int max = 1103;
    std::vector<decltype(this->map.cbegin())> first_ten;
    first_ten.reserve(10);
    for (int i = 0; i < 10; ++i) {
        const auto [it, success] = this->emplace(i);
        EXPECT_TRUE(success);
        first_ten.push_back(it);
    }
    {
        int i = 0;
        for (const auto it : first_ten) {
            EXPECT_EQ(i, this->keys.value(it->first));
            EXPECT_EQ(i, this->values.value(it->second));
            ++i;
        }
    }
    for (int i = 0; i < max; ++i) {
        const bool success = this->emplace(i).second;
        if (i < 10) {
            EXPECT_FALSE(success);
        }
        else {
            EXPECT_TRUE(success);
        }
    }
    {
        int i = 0;
        for (const auto it : first_ten) {
            EXPECT_EQ(i, this->keys.value(it->first));
            EXPECT_EQ(i, this->values.value(it->second));
            ++i;
        }
    }
}

TYPED_TEST(HashMapTest, at)
{
    const int max = 79;
    for (int i = 0; i < max; ++i) {
        if (i % 2) {
            this->emplace(i);
        }
    }
    for (int i = 0; i < max; ++i) {
        if (i % 2) {
            EXPECT_EQ(i, this->values.value(this->map.at(this->keys.create(i))));
        }
        else {
            EXPECT_THROW(this->map.at(this->keys.create(i)), std::out_of_range);
        }
    }
}

TYPED_TEST(HashMapTest, at_mutate)
{
    const auto values = {11, 17, 101};
    for (int i : values) {
        this->emplace(i);
    }
    for (int i : values) {
        EXPECT_EQ(i, this->values.value(this->map.at(this->keys.create(i))));
        this->map.at(this->keys.create(i)) = this->values.create(i * i);
    }
    for (int i : values) {
        EXPECT_EQ(i * i, this->values.value(this->map.at(this->keys.create(i))));
    }
}

TYPED_TEST(HashMapTest, indexed_access)
{
    const int max = 71;
    for (int i = 0; i < max; ++i) {
        this->map[this->keys.create(i)] = this->values.create(i * 333);
    }
    for (int i = 0; i < max; ++i) {
        const auto value = this->values.create(i * 333);
        EXPECT_EQ(value, this->map[this->keys.create(i)]);
    }
}

TYPED_TEST(HashMapTest_CopyableElems, parallel_access)
{
    const int max = 1001;
    for (int i = 0; i < max; ++i) {
        this->emplace(i);
    }

    using Key = decltype(this->keys.create(0));
    const std::size_t threads_count = 6;
    std::vector<std::vector<Key>> patterns(threads_count, std::vector<Key>{});
    std::mt19937_64 gen(11111);
    for (std::size_t n = 0; n < threads_count; ++n) {
        auto & p = patterns[n];
        p.reserve(max);
        for (int i = 0; i < max; ++i) {
            p.emplace_back(this->keys.create(i));
        }
        std::shuffle(p.begin(), p.end(), gen);
    }
    std::list<std::size_t> results;
    std::mutex mutex;
    std::vector<std::thread> threads;
    threads.reserve(threads_count);
    for (std::size_t i = 0; i < threads_count; ++i) {
        threads.emplace_back([&map = this->map, &keys = patterns[i], &results, &mutex] {
                auto copy = map; // NOLINT
                std::size_t equal = 0;
                for (const auto key : keys) {
                    const auto & value = copy[key];
                    const auto it = map.find(key);
                    if (it != map.end() && it->second == value) {
                        ++equal;
                    }
                }
                std::lock_guard lock(mutex);
                results.push_back(equal);
            });
    }
    for (auto & t : threads) {
        t.join();
    }
    EXPECT_EQ(threads_count, results.size());
    for (const auto x : results) {
        EXPECT_EQ(max, x) << "expected to have all elements in both the original and copied maps";
    }
}

TYPED_TEST(HashMapTest_CopyableElems, load_test)
{
    const int max = 1299827;
    long long sum = 0;
    std::vector<TypeParam> keys;
    keys.reserve(max);
    for (int i = 0; i < max; ++i) {
        this->emplace(i);
        sum += i;
        keys.emplace_back(this->keys.create(i));
    }

    const std::size_t N = 239;
    sum *= N;
    long long check_sum = 0;
    for (std::size_t n = 0; n < N; ++n) {
        auto copy = this->map; // NOLINT
        for (auto it = keys.rbegin(), end = keys.rend(); it != end; ++it) {
            // operator[]
            check_sum += copy[*it];

            // find
            ++it;
            if (it == end) {
                break;
            }
            const auto m_it = copy.find(*it);
            ASSERT_NE(m_it, copy.end());
            check_sum += m_it->second;

            // at
            ++it;
            if (it == end) {
                break;
            }
            check_sum += copy.at(*it);
        }
    }
    EXPECT_EQ(sum, check_sum);
}

TYPED_TEST(HashMapTest_CopyableElems, insert_range)
{
    using Value = typename std::remove_pointer_t<decltype(this)>::Value;
    std::vector<Value> elements;
    const int max = 9999;
    elements.reserve(max);
    for (int i = 0; i < max; ++i) {
        elements.push_back(this->create(i));
    }
    this->map.insert(elements.begin(), elements.end());
    EXPECT_EQ(elements.size(), this->map.size());
}

TYPED_TEST(HashMapTest_CopyableElems, assign_from_initializer_list)
{
    {
        const int max = 1111;
        for (int i = -max/2; i < max/2; ++i) {
            this->emplace(i);
        }
    }
    this->map = {this->create(9999), this->create(11111), this->create(33333), this->create(55555)};
    EXPECT_FALSE(this->map.contains(this->keys.create(0)));
    EXPECT_FALSE(this->map.contains(this->keys.create(-11)));
    EXPECT_FALSE(this->map.contains(this->keys.create(-111)));
    EXPECT_FALSE(this->map.contains(this->keys.create(111)));
    EXPECT_FALSE(this->map.contains(this->keys.create(533)));
    EXPECT_TRUE(this->map.contains(this->keys.create(9999)));
    EXPECT_TRUE(this->map.contains(this->keys.create(11111)));
    EXPECT_TRUE(this->map.contains(this->keys.create(33333)));
    EXPECT_TRUE(this->map.contains(this->keys.create(55555)));
    EXPECT_EQ(4, this->map.size());
}

TYPED_TEST(BadHashingHashMapTest, insert_with_collisions)
{
    const auto create = [] (const std::size_t x) {
        const std::size_t n = x / 100 + 1;
        std::string s(n, ' ');
        const unsigned char ch = 32 + x % 100;
        for (std::size_t i = 0; i < n; ++i) {
            s[i] = ch;
        }
        return s;
    };
    const std::size_t max = 1039;
    this->map.reserve(max / 3);
    for (std::size_t i = 0; i < max; ++i) {
        const auto s = create(i);
        const bool success = this->map.emplace(s, i).second;
        EXPECT_TRUE(success) << s << " wasn't inserted";
    }
    EXPECT_EQ(max, this->map.size());
    EXPECT_TRUE(this->map.contains(create(333)));
    EXPECT_FALSE(this->map.contains(create(max)));
    EXPECT_FALSE(this->map.contains(create(max + 1)));
    EXPECT_FALSE(this->map.contains(create(max + 7)));
    EXPECT_FALSE(this->map.contains(create(max + 100)));
    EXPECT_FALSE(this->map.contains(create(max + 200)));
    EXPECT_GE(1.0, this->map.load_factor());
}

TEST_F(CountingHashMapTest, insert)
{
    ConstructionAware::reset_counters();
    const int max = 571;
    map.reserve(max);
    for (int i = 0; i < max; ++i) {
        map.insert(std::make_pair(i, i));
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    map.clear();
    ConstructionAware::reset_counters();
    using Value = decltype(map)::value_type;
    for (int i = 0; i < max; ++i) {
        map.insert(Value(i, i));
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(max, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, insert_or_assign)
{
    ConstructionAware::reset_counters();
    const int max = 379;
    map.reserve(max);
    for (int i = 0; i < max; ++i) {
        map.insert_or_assign(i, i);
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    ConstructionAware::reset_counters();
    for (int i = 0; i < max; ++i) {
        map.insert_or_assign(i, i);
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(max, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, emplace)
{
    ConstructionAware::reset_counters();
    const int max = 1453;
    map.reserve(max);
    for (int i = 0; i < max; ++i) {
        map.emplace(i, i);
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_GE(max, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    ConstructionAware::reset_counters();
    for (int i = 0; i < max; ++i) {
        map.emplace(i, i);
    }
    EXPECT_GE(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, emplace_hint)
{
    ConstructionAware::reset_counters();
    map.emplace_hint(map.end(), 1, 1);
    EXPECT_EQ(1, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_GE(1, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, try_emplace)
{
    ConstructionAware::reset_counters();
    const int max = 17;
    map.reserve(max);
    for (int i = 0; i < max; ++i) {
        map.try_emplace(i, i);
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    ConstructionAware::reset_counters();
    for (int i = 0; i < max; ++i) {
        map.try_emplace(i, i);
    }
    EXPECT_EQ(0, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, indexed_access)
{
    ConstructionAware::reset_counters();
    map.reserve(2);
    const auto & x = map[1];
    EXPECT_EQ(0, x.value());
    EXPECT_EQ(1, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    map[2] = ConstructionAware(3);
    EXPECT_EQ(3, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(1, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, copy)
{
    ConstructionAware::reset_counters();
    const int max = 19;
    map.reserve(max);
    for (int i = 0; i < max; ++i) {
        map.emplace(i, i);
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    decltype(map) another = map;
    EXPECT_EQ(map, another);
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(max, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, move)
{
    ConstructionAware::reset_counters();
    const int max = 13;
    map.reserve(max);
    for (int i = 0; i < max; ++i) {
        map.emplace(i, i);
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    decltype(map) another = std::move(map);
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
}

TEST_F(CountingHashMapTest, swap)
{
    ConstructionAware::reset_counters();
    const int max = 23;
    map.reserve(max);
    for (int i = 0; i < max; ++i) {
        map.emplace(i, i);
    }
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
    decltype(map) another;
    another.swap(map);
    EXPECT_EQ(max, ConstructionAware::constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_constructor_calls_count());
    EXPECT_EQ(0, ConstructionAware::copy_assignment_calls_count());
    EXPECT_EQ(0, ConstructionAware::move_assignment_calls_count());
}

TEST_F(ComplexConstructionHashMapTest, emplace)
{
    map.emplace("Hello", "world");
    map.emplace(std::make_pair("Have a", "nice day"));
    map.emplace(std::piecewise_construct,
            std::forward_as_tuple("Give me", 4),
            std::forward_as_tuple("a hand", 7));
    EXPECT_FALSE(map.contains("Give me"));
    EXPECT_EQ("world", map.at("Hello"));
    EXPECT_EQ(std::string("a hand\0", 7), map.at("Give"));
    EXPECT_EQ("nice day", map.at("Have a"));
}

TEST_F(ComplexConstructionHashMapTest, try_emplace)
{
    map.try_emplace("Nice", "weather");
    map.try_emplace("Nice", "evening", 8);
    map.try_emplace("Summernight", "city", 4);
    map.try_emplace(std::string("Clavicula", 5), std::string("nox"));
    map.try_emplace(map.end(), "The", "eldest cosmonaut", 6);
    EXPECT_EQ("weather", map.at("Nice"));
    EXPECT_EQ("city", map.at("Summernight"));
    EXPECT_EQ("nox", map.at("Clavi"));
    EXPECT_EQ("eldest", map.at("The"));
}

using TypesToTest = ::testing::Types<HashMapTestT<std::string, std::string>>;
INSTANTIATE_TYPED_TEST_SUITE_P(HashMap, IteratorTest, TypesToTest);
