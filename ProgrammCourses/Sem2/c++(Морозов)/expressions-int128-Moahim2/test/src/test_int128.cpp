#include "int128.h"

#include <gtest/gtest.h>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>

namespace {
/// 2 ** 127 - 1
constexpr std::string_view max_value_str = "170141183460469231731687303715884105727";

/// - 2 ** 127
constexpr std::string_view min_value_str = "-170141183460469231731687303715884105728";

/// Returns string representation of the same number, multiplied by 1e30
std::string to_big_int128_string(const auto num)
{
    return std::to_string(num) + std::string(30, '0');
}
} // namespace

TEST(Int128Test, traits)
{
    EXPECT_EQ(sizeof(Int128) * 8, 128);
    EXPECT_TRUE(std::is_trivially_copyable_v<Int128>);
}

TEST(Int128Test, construct_and_cast)
{
    const Int128 zero;
    EXPECT_EQ(static_cast<std::int64_t>(zero), 0);
    EXPECT_EQ(static_cast<double>(zero), 0);

    const int x = 42;
    const Int128 x2 = x;
    EXPECT_EQ(static_cast<std::int64_t>(x2), x);
    EXPECT_EQ(static_cast<double>(x2), 42.0);

    const double y = 42.5;
    const Int128 y2 = y;
    EXPECT_EQ(static_cast<std::int64_t>(y2), 42);
    EXPECT_EQ(static_cast<double>(y2), 42.0);

    const std::int64_t z = 999'999'999'999'999'999;
    const Int128 z2 = z;
    EXPECT_EQ(static_cast<std::int64_t>(z2), z);
    EXPECT_EQ(static_cast<double>(z2), z);
}

TEST(Int128Test, basic_equals)
{
    const Int128 x = 42;
    const Int128 y = 42;
    const Int128 z = x;
    const Int128 distinct = 72;

    EXPECT_EQ(x, x);
    EXPECT_EQ(y, y);
    EXPECT_EQ(z, z);
    EXPECT_EQ(distinct, distinct);

    EXPECT_EQ(x, y);
    EXPECT_EQ(x, z);

    EXPECT_EQ(y, x);
    EXPECT_EQ(y, z);

    EXPECT_EQ(z, x);
    EXPECT_EQ(z, y);

    EXPECT_NE(x, distinct);
    EXPECT_NE(y, distinct);
    EXPECT_NE(z, distinct);

    EXPECT_NE(distinct, x);
    EXPECT_NE(distinct, y);
    EXPECT_NE(distinct, z);
}

TEST(Int128Test, basic_negate)
{
    const Int128 x = 42;
    const Int128 y = -42;
    const Int128 z = 0;

    EXPECT_EQ(z, -z);
    EXPECT_EQ(x, -y);
    EXPECT_EQ(-x, y);

    EXPECT_EQ(-(-x), x);
    EXPECT_EQ(-(-y), y);
    EXPECT_EQ(-(-z), z);
}

TEST(Int128Test, basic_add)
{
    const Int128 id = 0;
    const Int128 x = 42;
    Int128 y = 21;

    EXPECT_EQ(x + 0, x);
    EXPECT_EQ(x + id, x);

    EXPECT_EQ(x + 1, Int128(43));
    EXPECT_EQ(x + y, Int128(63));

    y += 0;
    EXPECT_EQ(y, Int128(21));

    y += x;
    EXPECT_EQ(y, Int128(63));
}

TEST(Int128Test, basic_subtract)
{
    const Int128 x = 42;
    Int128 y = 63;
    const Int128 z = 21;

    EXPECT_EQ(x - 0, x);
    EXPECT_EQ(y - 0, y);
    EXPECT_EQ(z - 0, z);

    EXPECT_EQ(0 - x, -x);
    EXPECT_EQ(0 - y, -y);
    EXPECT_EQ(0 - z, -z);

    EXPECT_EQ(y - z, x);
    EXPECT_EQ(z - y, -x);

    y -= 0;
    EXPECT_EQ(y, Int128(63));

    y -= z;
    EXPECT_EQ(y, x);
}

TEST(Int128Test, basic_multiply)
{
    const Int128 x = 42;
    Int128 y = 2;
    const Int128 z = 84;
    const Int128 id = 1;
    const Int128 zero;

    EXPECT_EQ(id * id, id);
    EXPECT_EQ(id * x, x);

    EXPECT_EQ(zero * 42, zero);
    EXPECT_EQ(zero * id, zero);
    EXPECT_EQ(id * zero, zero);

    EXPECT_EQ(x * y, z);
    EXPECT_EQ(y * x, z);

    y *= id;
    EXPECT_EQ(y, Int128(2));

    y *= x;
    EXPECT_EQ(y, z);
}

TEST(Int128Test, basic_multiply_negative)
{
    const Int128 x = 42;
    const Int128 y = -42;
    const Int128 z = 0;
    const Int128 m_one = -1;

    EXPECT_EQ(z, z * m_one);
    EXPECT_EQ(x, y * m_one);
    EXPECT_EQ(m_one * x, y);

    EXPECT_EQ(x * m_one * m_one, x);
    EXPECT_EQ(m_one * x * m_one, x);
    EXPECT_EQ(m_one * m_one * x, x);
}

TEST(Int128Test, basic_divide)
{
    const Int128 x = 42;
    const Int128 y = 2;
    Int128 z = 84;
    const Int128 id = 1;
    const Int128 zero;

    EXPECT_EQ(id / id, id);
    EXPECT_EQ(x / id, x);

    EXPECT_EQ(zero / x, zero);
    EXPECT_EQ(zero / id, zero);

    EXPECT_EQ(z / y, x);
    EXPECT_EQ(y / z, zero);
    EXPECT_EQ(y / id, y);

    EXPECT_EQ(z /= id, Int128(84));
    EXPECT_EQ(z /= y, x);
}

TEST(Int128Test, equals)
{
    const Int128 x(to_big_int128_string(42));
    const Int128 y(to_big_int128_string(42));
    const Int128 z(x);
    const Int128 distinct = 72;

    EXPECT_EQ(x, x);
    EXPECT_EQ(y, y);
    EXPECT_EQ(z, z);
    EXPECT_EQ(distinct, distinct);

    EXPECT_EQ(x, y);
    EXPECT_EQ(x, z);

    EXPECT_EQ(y, x);
    EXPECT_EQ(y, z);

    EXPECT_EQ(z, x);
    EXPECT_EQ(z, y);

    EXPECT_NE(x, distinct);
    EXPECT_NE(y, distinct);
    EXPECT_NE(z, distinct);

    EXPECT_NE(distinct, x);
    EXPECT_NE(distinct, y);
    EXPECT_NE(distinct, z);
}

TEST(Int128Test, negate)
{
    const Int128 x(to_big_int128_string(42));
    const Int128 y(to_big_int128_string(-42));
    const Int128 z = 0;

    EXPECT_EQ(z, -z);
    EXPECT_EQ(x, -y);
    EXPECT_EQ(-x, y);

    EXPECT_EQ(-(-x), x);
    EXPECT_EQ(-(-y), y);
    EXPECT_EQ(-(-z), z);
}

TEST(Int128Test, add)
{
    const Int128 id = 0;
    const Int128 x(to_big_int128_string(42));
    Int128 y(to_big_int128_string(21));

    EXPECT_EQ(x + 0, x);
    EXPECT_EQ(x + id, x);

    auto x_next = to_big_int128_string(42);
    x_next.back() = '1';

    EXPECT_EQ(x + 1, Int128(x_next));
    EXPECT_EQ(x + y, Int128(to_big_int128_string(63)));

    y += 0;
    EXPECT_EQ(y, Int128(to_big_int128_string(21)));

    y += x;
    EXPECT_EQ(y, Int128(to_big_int128_string(63)));
}

TEST(Int128Test, subtract)
{
    const Int128 x(to_big_int128_string(42));
    Int128 y(to_big_int128_string(63));
    const Int128 z(to_big_int128_string(21));

    EXPECT_EQ(x - 0, x);
    EXPECT_EQ(y - 0, y);
    EXPECT_EQ(z - 0, z);

    EXPECT_EQ(0 - x, -x);
    EXPECT_EQ(0 - y, -y);
    EXPECT_EQ(0 - z, -z);

    EXPECT_EQ(y - z, x);
    EXPECT_EQ(z - y, -x);

    y -= 0;
    EXPECT_EQ(y, Int128(to_big_int128_string(63)));

    y -= z;
    EXPECT_EQ(y, x);
}

TEST(Int128Test, multiply)
{
    const Int128 x(to_big_int128_string(42));
    Int128 y(2);
    const Int128 z(to_big_int128_string(84));

    const Int128 id = 1;
    EXPECT_EQ(id * id, id);
    EXPECT_EQ(id * x, x);

    const Int128 zero;
    EXPECT_EQ(zero * x, zero);
    EXPECT_EQ(zero * id, zero);
    EXPECT_EQ(id * zero, zero);

    EXPECT_EQ(x * y, z);
    EXPECT_EQ(y * x, z);

    y *= id;
    EXPECT_EQ(y, Int128(2));

    y *= x;
    EXPECT_EQ(y, z);
}

TEST(Int128Test, multiply_negative)
{
    const Int128 x(to_big_int128_string(42));
    const Int128 y(to_big_int128_string(-42));
    const Int128 z = 0;
    const Int128 m_one = -1;

    EXPECT_EQ(z, z * m_one);
    EXPECT_EQ(x, y * m_one);
    EXPECT_EQ(m_one * x, y);

    EXPECT_EQ(x * m_one * m_one, x);
    EXPECT_EQ(m_one * x * m_one, x);
    EXPECT_EQ(m_one * m_one * x, x);
}

TEST(Int128Test, divide)
{
    const Int128 x(to_big_int128_string(42));
    const Int128 y = 2;
    Int128 z(to_big_int128_string(84));
    const Int128 id = 1;
    const Int128 zero;

    EXPECT_EQ(id / id, id);
    EXPECT_EQ(x / id, x);
    EXPECT_EQ(zero / x, zero);
    EXPECT_EQ(zero / id, zero);

    EXPECT_EQ(z / y, x);
    EXPECT_EQ(y / z, zero);
    EXPECT_EQ(y / id, y);

    EXPECT_EQ(z /= id, Int128(to_big_int128_string(84)));
    EXPECT_EQ(z /= y, x);
}

TEST(Int128Test, string_zero)
{
    const Int128 zero;
    EXPECT_EQ(zero.str(), "0");
    EXPECT_EQ(zero.str(), (-zero).str());
}

TEST(Int128Test, basic_string)
{
    Int128 x = 42;
    Int128 y = 63;
    Int128 z = 21;

    constexpr auto * x_const_char_value = "42";
    std::string y_str_value = "63";
    std::string_view z_str_view_value = "21";

    const Int128 x2(x_const_char_value);
    const Int128 y2(y_str_value);
    const Int128 z2(z_str_view_value);

    std::string minus_sign = "-";

    EXPECT_EQ(x, x2);
    EXPECT_EQ(y, y2);
    EXPECT_EQ(z, z2);

    EXPECT_EQ(x.str(), x2.str());
    EXPECT_EQ(y.str(), y2.str());
    EXPECT_EQ(z.str(), z2.str());

    EXPECT_EQ(x.str(), x_const_char_value);
    EXPECT_EQ(y.str(), y_str_value);
    EXPECT_EQ(z.str(), z_str_view_value);

    x *= -1;
    y *= -1;
    z *= -1;

    EXPECT_EQ(x.str(), minus_sign + x_const_char_value);
    EXPECT_EQ(y.str(), minus_sign + y_str_value);
    EXPECT_EQ(z.str(), minus_sign + std::string(z_str_view_value));

    const Int128 ten_thousand = static_cast<std::int64_t>(1e4);
    const Int128 x_mul = x * ten_thousand;
    const Int128 y_mul = x * ten_thousand;
    const Int128 z_mul = x * ten_thousand;

    EXPECT_EQ(x_mul.str(), x.str() + "0000");
    EXPECT_EQ(y_mul.str(), x.str() + "0000");
    EXPECT_EQ(z_mul.str(), x.str() + "0000");
}

TEST(Int128Test, print)
{
    constexpr std::string_view expected_result = "42 21 63 0";
    constexpr std::string_view expected_negated_result = "-42 -21 -63 0";
    const Int128 x("42");
    const Int128 y("21");
    const Int128 z("63");
    const Int128 zero;

    std::stringstream current_stream;
    current_stream << x << " " << y << " " << z << " " << zero;
    EXPECT_EQ(current_stream.str(), expected_result);

    std::stringstream current_stream_negated;
    current_stream_negated << -x << " " << -y << " " << -z << " " << -zero;
    EXPECT_EQ(current_stream_negated.str(), expected_negated_result);
}

TEST(Int128Test, big_string)
{
    const Int128 max_val(max_value_str);
    EXPECT_EQ(max_val.str(), max_value_str);
    const Int128 min_val(min_value_str);
    EXPECT_EQ(min_val.str(), min_value_str);
}

TEST(Int128Test, operations_on_limits)
{
    const Int128 max_val(max_value_str);
    const Int128 min_val(min_value_str);
    const Int128 reverse_max_val = min_val + 1;

    EXPECT_EQ(-max_val, reverse_max_val);
    EXPECT_EQ(max_val * -1, reverse_max_val);
    EXPECT_EQ(reverse_max_val / max_val, -1);
    EXPECT_EQ(max_val / reverse_max_val, -1);
    EXPECT_EQ(reverse_max_val + max_val, 0);
    EXPECT_EQ(max_val + reverse_max_val, 0);

    EXPECT_EQ(min_val * 1, min_val);
    EXPECT_EQ(min_val / 1, min_val);
    EXPECT_EQ(min_val * 0, 0);
    EXPECT_EQ(0 / min_val, 0);
}

TEST(Int128Test, multiply_positive_limits)
{
    Int128 current = 1;
    // going to max value
    for (std::size_t i = 0; i < 126; i++) {
        current *= 2;
    }
    current += (current - 1);
    EXPECT_EQ(current.str(), max_value_str);
}

TEST(Int128Test, multiply_negative_limits)
{
    Int128 current = -1;
    // going to min value
    for (std::size_t i = 0; i < 127; i++) {
        current *= 2;
    }
    EXPECT_EQ(current.str(), min_value_str);
}

TEST(Int128Test, divide_positive_limits)
{
    Int128 current(max_value_str);
    for (std::size_t i = 0; i < 126; i++) {
        current /= 2;
    }

    EXPECT_EQ(current.str(), "1");
    EXPECT_EQ(current, Int128(1));
}

TEST(Int128Test, divide_negative_limits)
{
    Int128 current(min_value_str);
    for (std::size_t i = 0; i < 127; i++) {
        current /= 2;
    }

    EXPECT_EQ(current.str(), "-1");
    EXPECT_EQ(current, -1);
}

TEST(Int128Test, divide_negative_limits_step_10)
{
    Int128 current(min_value_str);
    std::string_view current_str_value = min_value_str;

    while (current != 0) {
        EXPECT_EQ(current.str(), current_str_value);
        current /= 10;
        current_str_value = current_str_value.substr(0, current_str_value.size() - 1);
    }

    EXPECT_EQ(current.str(), "0");
    EXPECT_EQ(current, 0);
    EXPECT_EQ(current_str_value.size(), 1);
}
