#include <thread>
#include <functional>

#include <gtest/gtest.h>

namespace iterator_test {

template<typename Iterator>
void test_multipass(Iterator begin, Iterator end)
{
    // Check iterator allows multipass
    size_t count = 10;
    std::vector<typename Iterator::value_type> expected; 
    std::copy(begin, end, std::back_inserter(expected));

    for (size_t c = 0; c < count; ++c) {
        size_t exp_idx = 0;
        for (auto it = begin; it != end; ++it) {
            ASSERT_TRUE(exp_idx < expected.size());
            ASSERT_TRUE(expected[exp_idx++] == *it);
        }
        ASSERT_TRUE(exp_idx == expected.size());
    }
}

template<typename Iterator>
void forward(Iterator begin, Iterator end)
{
    ASSERT_TRUE(begin != end);

    // Check iterator traits
    static_assert(std::is_copy_constructible_v<Iterator>);
    static_assert(std::is_copy_assignable_v<Iterator>);
    static_assert(std::is_destructible_v<Iterator>);
    static_assert(std::is_swappable_v<Iterator>);
    static_assert(std::is_default_constructible_v<Iterator>);

    static_assert(std::is_lvalue_reference_v<typename Iterator::reference>);
    static_assert(std::is_convertible_v<std::remove_reference_t<typename Iterator::reference>, typename Iterator::value_type>);

    static_assert(std::is_pointer_v<typename Iterator::pointer>);
    static_assert(std::is_convertible_v<std::remove_pointer_t<typename Iterator::pointer>, typename Iterator::value_type>);

    static_assert(std::is_same_v<std::remove_const_t<std::remove_reference_t<decltype(*begin)>>, typename Iterator::value_type>);

    {
        auto v = begin;
        static_assert(std::is_lvalue_reference_v<decltype(++v)>);
        v = begin;
        static_assert(std::is_same_v<std::remove_reference_t<decltype(++v)>, Iterator>);
        v = begin;
        static_assert(!std::is_reference_v<decltype(v++)>);
        v = begin;
        static_assert(std::is_same_v<decltype(v++), Iterator>);
    }

    {
        // Equality comparable
        Iterator a = begin, b = begin, c = begin;
        auto v = (a == a);
        static_assert(std::is_convertible_v<decltype(v), bool>);
        ASSERT_TRUE(v);
        ASSERT_TRUE((a == b) && (b == a));
        ASSERT_TRUE((a == b) && (b == c) && (c == a));
        ASSERT_FALSE(a != a);
        ASSERT_FALSE(a != b);
        ASSERT_FALSE(b != a);
        ASSERT_FALSE(b != c);
        ASSERT_FALSE(c != a);
        ASSERT_EQ(std::distance(a, b), 0);
    }

    {
        // Check iterator operations
        auto a = begin;
        auto the_a = a++;
        static_assert(std::is_same_v<Iterator, decltype(the_a)>);

        auto b = begin;
        auto next_b = ++b;
        static_assert(std::is_same_v<Iterator, decltype(next_b)>);

        ASSERT_TRUE(a == b);
        ASSERT_TRUE((a != begin) && (b != begin));

        if ((a != end) && (b != end)) {
            ASSERT_TRUE(*a == *b);
        }
    }

    test_multipass(begin, end);
}

template<typename Iterator>
void bidirectional(Iterator begin, Iterator end)
{
    ASSERT_TRUE(begin != end);

    {
        auto v = end;
        static_assert(std::is_lvalue_reference_v<decltype(--v)>);
        v = end;
        static_assert(std::is_same_v<std::remove_reference_t<decltype(--v)>, Iterator>);
        v = end;
        static_assert(!std::is_reference_v<decltype(v--)>);
        v = end;
        static_assert(std::is_same_v<decltype(v--), Iterator>);
    }

    auto a = begin, b = begin;
    ASSERT_TRUE(--(++a) == b);
    a++; b++;
    auto prev_a = --a, prev_b = --b;
    ASSERT_TRUE(prev_a == prev_b);

    std::vector<Iterator> reference;
    for (auto it = begin; it != end; ++it) {
        reference.emplace_back(it);
    }

    auto r_it = reference.rbegin(), r_end = reference.rend();
    auto back = end;
    std::size_t count = 0;
    do {
        --back;
        EXPECT_EQ(*r_it, back);
        ++r_it;
        ++count;
    } while (begin != back && r_it != r_end);
    EXPECT_EQ(reference.size(), count);
}

template<typename Iterator>
void random_access(Iterator begin, Iterator end)
{
    ASSERT_TRUE(begin != end);
    auto /* available_distance */ l = std::distance(begin, end);
    static_assert(std::is_same_v<typename Iterator::difference_type, decltype(l)>);
    static_assert(std::is_same_v<typename Iterator::difference_type, decltype(end - begin)>);
    ASSERT_GE(l, 2);
    auto n = l - 1;

    {
        auto v = begin;
        static_assert(std::is_same_v<Iterator&, decltype(v += n)>);
    }

    auto b = begin;
    auto f = (b += n);

    auto other_b = begin;
    auto j = n;
    for (j = 0; j < n; ++j) {
        ++other_b;
    }
    ASSERT_TRUE(other_b == b);

    auto f1 = (begin + n), f2 = (n + begin);
    static_assert(std::is_same_v<Iterator, decltype(f1)>);
    EXPECT_TRUE(f == f1);
    EXPECT_TRUE(f == f2);

    auto r = (f -= n);
    ASSERT_TRUE(r == begin);

    auto i = begin;
    static_assert(std::is_convertible_v<typename Iterator::reference, decltype(i[n])>);
    ASSERT_TRUE(i[n] == *f1);
    ASSERT_TRUE(begin < f1);
    ASSERT_TRUE(begin < end);
    ASSERT_FALSE(f1 < begin);
    ASSERT_FALSE(end < begin);
    ASSERT_TRUE(begin <= f1);
    ASSERT_TRUE(begin <= end);
    ASSERT_TRUE(f1 >= begin);
    ASSERT_TRUE(end >= begin);
}

template<typename Iterator>
void traits(Iterator begin, Iterator end, std::forward_iterator_tag)
{
    forward(begin, end);
}

template<typename Iterator>
void traits(Iterator begin, Iterator end, std::bidirectional_iterator_tag)
{
    forward(begin, end);
    bidirectional(begin, end);
}

template<typename Iterator>
void traits(Iterator begin, Iterator end, std::random_access_iterator_tag)
{
    forward(begin, end);
    bidirectional(begin, end);
    random_access(begin, end);
}


template<typename Iterator>
struct Job
{
    using data_feed_t = std::function< std::pair<Iterator, Iterator> () >;
    using test_t = std::function< void(Iterator, Iterator) >;

    data_feed_t range;
    test_t test;
    Job(data_feed_t r, test_t t):
        range{r}, test{t}
    {}
};

template<typename Iterator>
void run_multithread(std::vector<Job<Iterator>> jobs, std::size_t load_factor = 1234)
{
    std::cout << "Start " << jobs.size() << " threads\n";
    std::vector<std::thread> v;
    for (const auto & j : jobs) {
        v.emplace_back([load_factor, &j] ()
                {
                    std::vector<std::pair<Iterator, Iterator>> results;
                    for (std::size_t i = 0; i < load_factor; ++i ) {
                        results.emplace_back(j.range());
                    }
                    for (const auto & [b, e] : results) {
                        j.test(b, e);
                    }
                 });
    }

    for (auto & t : v) {
        t.join();
    }
    std::cout << "All threads have been joined\n";
}

template<typename Iterator>
void test_basic(Iterator begin, Iterator end)
{
    static_assert(!std::is_same_v<typename std::iterator_traits<Iterator>::iterator_category(), std::input_iterator_tag>);
    static_assert(!std::is_same_v<typename std::iterator_traits<Iterator>::iterator_category(), std::output_iterator_tag>);
    // FIXME: uncomment this as soon as C++20 is available
    // static_assert(!std::is_same_v<typename std::iterator_traits<Iterator>::iterator_category(), std::contiguous_iterator_tag>);

    traits(begin, end, typename std::iterator_traits<Iterator>::iterator_category());
}

}

template <typename SpecificTestFixture>
struct IteratorTest: public SpecificTestFixture
{
    // NB! SpecificTestFixture has to provide sample data through 'not_empty_container()'
};

TYPED_TEST_SUITE_P(IteratorTest);

TYPED_TEST_P(IteratorTest, basic)
{
    auto & data = this->not_empty_container();
    iterator_test::test_basic(data.begin(), data.end());
}

REGISTER_TYPED_TEST_SUITE_P(IteratorTest, basic);
