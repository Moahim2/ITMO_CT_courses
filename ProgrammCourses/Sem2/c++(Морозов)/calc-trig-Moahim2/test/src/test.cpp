#include "calc.h"

#include <gtest/gtest.h>

#include <cmath>

namespace {

class CalcTest : public ::testing::TestWithParam<bool>
{
};

INSTANTIATE_TEST_SUITE_P(Calc, CalcTest, ::testing::Values(true, false));

}

TEST_P(CalcTest, err)
{
    auto param = GetParam();
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "fix"));
    EXPECT_EQ("Unknown operation fix\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, param, "sqrt"));
    EXPECT_EQ("Unknown operation sqrt\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(17, process_line(17, param, "\\ 11"));
    EXPECT_EQ("Unknown operation \\ 11\n", testing::internal::GetCapturedStderr());
}

TEST_P(CalcTest, set)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "0"));
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "0000"));
    EXPECT_DOUBLE_EQ(0, process_line(101, param, "0"));
    EXPECT_DOUBLE_EQ(13, process_line(0, param, "13"));
    EXPECT_DOUBLE_EQ(5, process_line(99, param, "5."));
    EXPECT_DOUBLE_EQ(0.05625, process_line(1113, param, "0.05625"));
    EXPECT_DOUBLE_EQ(1234567890.0, process_line(1, param, "1234567890"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, param, "12345678900000"));
    EXPECT_EQ("Argument isn't fully parsed, suffix left: '0000'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(99, process_line(99, param, "5 "));
    EXPECT_EQ("Argument parsing error at 1: ' '\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "0.001 11"));
    EXPECT_EQ("Argument parsing error at 5: ' 11'\n", testing::internal::GetCapturedStderr());
}

TEST_P(CalcTest, add)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(7, process_line(0, param, "+7"));
    EXPECT_DOUBLE_EQ(7, process_line(5, param, "+ 2"));
    EXPECT_DOUBLE_EQ(7, process_line(5, param, "+ \t\t   2"));
    EXPECT_DOUBLE_EQ(2.34, process_line(1.5, param, "+ 0.84"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(9, process_line(9, param, "+    12345678900000"));
    EXPECT_EQ("Argument isn't fully parsed, suffix left: '0000'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(99, process_line(99, param, "+ 1 "));
    EXPECT_EQ("Argument parsing error at 3: ' '\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(5, process_line(5, param, "+ 1 2"));
    EXPECT_EQ("Argument parsing error at 3: ' 2'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "+0.001 11"));
    EXPECT_EQ("Argument parsing error at 6: ' 11'\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(99, process_line(99, param, "+ -"));
    EXPECT_EQ("Argument parsing error at 2: '-'\nNo argument for a binary operation\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(113, process_line(113, param, "+"));
    EXPECT_EQ("No argument for a binary operation\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(113, process_line(113, param, "+) 10"));
    EXPECT_EQ("Argument parsing error at 1: ') 10'\nNo argument for a binary operation\n", testing::internal::GetCapturedStderr());
}

TEST_P(CalcTest, sub)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(-11, process_line(0, param, "- 11"));
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "-0"));
    EXPECT_DOUBLE_EQ(0, process_line(3, param, "-3"));
    EXPECT_DOUBLE_EQ(-3, process_line(7, param, "-10"));
    EXPECT_DOUBLE_EQ(-12344.6789, process_line(1, param, "- 12345.67890"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(77, process_line(77, param, "- 123a3"));
    EXPECT_EQ("Argument parsing error at 5: 'a3'\n", testing::internal::GetCapturedStderr());
}

TEST_P(CalcTest, mul)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "* 0"));
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "*131"));
    EXPECT_DOUBLE_EQ(0, process_line(99, param, "* 0"));
    EXPECT_DOUBLE_EQ(8, process_line(2, param, "* 4"));
    EXPECT_DOUBLE_EQ(-16, process_line(-4, param, "*4"));
}

TEST_P(CalcTest, div)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "/ 11"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(11, process_line(11, param, "/ 0"));
    EXPECT_EQ("Bad right argument for division: 0\n", testing::internal::GetCapturedStderr());
    EXPECT_DOUBLE_EQ(3, process_line(6, param, "/ 2"));
    EXPECT_DOUBLE_EQ(0.7, process_line(7, param, "/ 10"));
    EXPECT_DOUBLE_EQ(0.3333333333333333, process_line(1, param, "/ 3"));
    EXPECT_DOUBLE_EQ(-0.5, process_line(-2, param, "/ 4"));
    EXPECT_DOUBLE_EQ(100, process_line(10, param, "/ 0.1"));
}

TEST_P(CalcTest, rem)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "% 3"));
    EXPECT_DOUBLE_EQ(0, process_line(4, param, "%4"));
    EXPECT_DOUBLE_EQ(0, process_line(-24, param, "%4"));
    EXPECT_DOUBLE_EQ(-3, process_line(-13, param, "%5"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(-1, process_line(-1, param, "%0"));
    EXPECT_EQ("Bad right argument for remainder: 0\n", testing::internal::GetCapturedStderr());
}

TEST_P(CalcTest, neg)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "_"));
    EXPECT_DOUBLE_EQ(1, process_line(-1, param, "_"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, param, "_ "));
    EXPECT_EQ("Unexpected suffix for a unary operation: ' '\n", testing::internal::GetCapturedStderr());
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(1, process_line(1, param, "_1"));
    EXPECT_EQ("Unexpected suffix for a unary operation: '1'\n", testing::internal::GetCapturedStderr());
}

TEST_P(CalcTest, pow)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "^1"));
    EXPECT_DOUBLE_EQ(0, process_line(0, param, "^2"));
    EXPECT_DOUBLE_EQ(1, process_line(119, param, "^0"));
    EXPECT_DOUBLE_EQ(37, process_line(37, param, "^1"));
    EXPECT_DOUBLE_EQ(25, process_line(-5, param, "^2"));
    EXPECT_DOUBLE_EQ(-27, process_line(-3, param, "^3"));
    EXPECT_DOUBLE_EQ(5, process_line(25, param, "^0.5"));
}

TEST_P(CalcTest, sqrt)
{
    auto param = GetParam();
    EXPECT_DOUBLE_EQ(1, process_line(1, param, "SQRT"));
    EXPECT_DOUBLE_EQ(0.7, process_line(0.49, param, "SQRT"));
    EXPECT_DOUBLE_EQ(5, process_line(25, param, "SQRT"));
    testing::internal::CaptureStderr();
    EXPECT_DOUBLE_EQ(-1, process_line(-1, param, "SQRT"));
    EXPECT_EQ("Bad argument for SQRT: -1\n", testing::internal::GetCapturedStderr());
}

namespace {

const double eps = 1e-15;

const double near_inf = 1.633123935319537e+16;

const double sqrt_3 = 1.7320508075688772935274463;
const double sqrt_3_over_3 = 1.7320508075688772935274463l / 3.0;
const double half_sqrt_3 = 1.7320508075688772935274463l / 2.0;
const double half_sqrt_2 = 0.707106781186547524400844;

const double pi = 3.141592653589793115997963468544185161590576171875;
const double half_pi = 1.5707963267948965579989817342720925807952880859375;
const double quarter_pi = 0.78539816339744827899949086713604629039764404296875;

} // anonymous namespace

TEST(Calc, sin)
{
    bool rad_on = false;
    EXPECT_NEAR(0.5, process_line(30, rad_on, "SIN"), eps);
    EXPECT_NEAR(1, process_line(90, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(-90, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(270, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "SIN"), eps);
    EXPECT_NEAR(-half_sqrt_2, process_line(225, rad_on, "SIN"), eps);
    EXPECT_NEAR(-0.5, process_line(330, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "SIN"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(480, rad_on, "SIN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(1, process_line(half_pi, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(-half_pi, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "SIN"), eps);
    EXPECT_NEAR(1e-1, process_line(1e-1, rad_on, "SIN"), 1e-2);
    EXPECT_NEAR(1e-2, process_line(1e-2, rad_on, "SIN"), 1e-3);
    EXPECT_NEAR(0, process_line(pi * 2, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(pi * 4, rad_on, "SIN"), eps);
    EXPECT_NEAR(0.5, process_line(pi * 5 / 6, rad_on, "SIN"), eps);
    EXPECT_NEAR(-half_sqrt_2, process_line(pi * 7 / 4, rad_on, "SIN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(1, process_line(90, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(-90, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "SIN"), eps);
    EXPECT_NEAR(-0.5, process_line(210, rad_on, "SIN"), eps);
    EXPECT_NEAR(-1, process_line(270, rad_on, "SIN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "SIN"), eps);
}

TEST(Calc, cos)
{
    bool rad_on = false;
    EXPECT_NEAR(0, process_line(90, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(0, rad_on, "COS"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(30, rad_on, "COS"), eps);
    EXPECT_NEAR(0.5, process_line(60, rad_on, "COS"), eps);
    EXPECT_NEAR(-1, process_line(180, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(360, rad_on, "COS"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(0, process_line(half_pi, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(0, rad_on, "COS"), eps);
    EXPECT_NEAR(-1, process_line(pi, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(pi * 2, rad_on, "COS"), eps);
    EXPECT_NEAR(-half_sqrt_2, process_line(pi * 3 / 4, rad_on, "COS"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(pi * 11 / 6, rad_on, "COS"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(0, process_line(90, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(0, rad_on, "COS"), eps);
    EXPECT_NEAR(-0.5, process_line(120, rad_on, "COS"), eps);
    EXPECT_NEAR(-1, process_line(180, rad_on, "COS"), eps);
    EXPECT_NEAR(-half_sqrt_3, process_line(210, rad_on, "COS"), eps);
    EXPECT_NEAR(-0.5, process_line(240, rad_on, "COS"), eps);
    EXPECT_NEAR(0, process_line(270, rad_on, "COS"), eps);
    EXPECT_NEAR(0.5, process_line(300, rad_on, "COS"), eps);
    EXPECT_NEAR(half_sqrt_3, process_line(330, rad_on, "COS"), eps);
    EXPECT_NEAR(1, process_line(360, rad_on, "COS"), eps);
}

TEST(Calc, tan)
{
    bool rad_on = false;
    EXPECT_NEAR(1, process_line(45, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(-45, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "TAN"), eps);
    EXPECT_NEAR(sqrt_3_over_3, process_line(30, rad_on, "TAN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(60, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "TAN"), eps);
    EXPECT_DOUBLE_EQ(near_inf, process_line(90, rad_on, "TAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(1, process_line(quarter_pi, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(-quarter_pi, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(pi, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(pi * 2, rad_on, "TAN"), eps);
    EXPECT_NEAR(sqrt_3_over_3, process_line(pi * 7 / 6, rad_on, "TAN"), eps);
    EXPECT_DOUBLE_EQ(near_inf, process_line(half_pi, rad_on, "TAN"));
    EXPECT_DOUBLE_EQ(-near_inf, process_line(-half_pi, rad_on, "TAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(1, process_line(45, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(-45, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(0, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(135, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(180, rad_on, "TAN"), eps);
    EXPECT_NEAR(1, process_line(225, rad_on, "TAN"), eps);
    EXPECT_NEAR(-1, process_line(315, rad_on, "TAN"), eps);
    EXPECT_NEAR(0, process_line(360, rad_on, "TAN"), eps);
}

TEST(Calc, ctn)
{
    bool rad_on = false;
    EXPECT_NEAR(1, process_line(45, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(30, rad_on, "CTN"), eps);
    EXPECT_TRUE(std::isinf(process_line(0, rad_on, "CTN")));
    EXPECT_NEAR(0, process_line(90, rad_on, "CTN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_NEAR(1, process_line(quarter_pi, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(pi / 6, rad_on, "CTN"), eps);
    EXPECT_TRUE(std::isinf(process_line(0, rad_on, "CTN")));
    EXPECT_NEAR(0, process_line(half_pi, rad_on, "CTN"), eps);
    EXPECT_NEAR(1, process_line(quarter_pi * 5, rad_on, "CTN"), eps);
    EXPECT_NEAR(-sqrt_3_over_3, process_line(pi * 5 / 3, rad_on, "CTN"), eps);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_NEAR(1, process_line(45, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3, process_line(30, rad_on, "CTN"), eps);
    EXPECT_NEAR(sqrt_3_over_3, process_line(60, rad_on, "CTN"), eps);
    EXPECT_TRUE(std::isinf(process_line(0, rad_on, "CTN")));
    EXPECT_NEAR(0, process_line(90, rad_on, "CTN"), eps);
}

TEST(Calc, asin)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(-90, process_line(-1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(-30, process_line(-0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(30, process_line(0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(90, process_line(1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(-half_pi, process_line(-1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(-pi / 6, process_line(-0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(pi / 6, process_line(0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(half_pi, process_line(1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(-90, process_line(-1, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(-30, process_line(-0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(30, process_line(0.5, rad_on, "ASIN"));
    EXPECT_DOUBLE_EQ(90, process_line(1, rad_on, "ASIN"));
}

TEST(Calc, acos)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(180, process_line(-1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(90, process_line(0, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(pi, process_line(-1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(half_pi, process_line(0, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(180, process_line(-1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(0, process_line(1, rad_on, "ACOS"));
    EXPECT_DOUBLE_EQ(90, process_line(0, rad_on, "ACOS"));
}

TEST(Calc, atan)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(quarter_pi, process_line(1, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "ATAN"));
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ATAN"));
}

TEST(Calc, actn)
{
    bool rad_on = false;
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(30, process_line(sqrt_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "RAD"));
    ASSERT_TRUE(rad_on);
    EXPECT_DOUBLE_EQ(quarter_pi, process_line(1, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(pi / 6, process_line(sqrt_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(pi / 3, process_line(sqrt_3_over_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(0, process_line(0, rad_on, "DEG"));
    ASSERT_FALSE(rad_on);
    EXPECT_DOUBLE_EQ(45, process_line(1, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(30, process_line(sqrt_3, rad_on, "ACTN"));
    EXPECT_DOUBLE_EQ(120, process_line(-sqrt_3_over_3, rad_on, "ACTN"));
}
