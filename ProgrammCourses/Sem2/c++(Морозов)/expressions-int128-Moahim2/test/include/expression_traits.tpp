#pragma once

#include "expression_traits.h"
#include "int128.h"

#include <limits>
#include <random>

template <>
struct ExpressionTraits<Int128> : BaseExpressionTraits<Int128>
{
private:
    static bool checkInt128Op(const Int128 & left, const Int128 & right, std::function<double(double, double)> const & op)
    {
        double n = op(static_cast<double>(left), static_cast<double>(right));
        return n >= -1e38 && n <= 1e38;
    }

public:
    static Int128 randomNumber(std::mt19937 & rng)
    {
        std::uniform_int_distribution<int> dist(-100, 100);
        return Int128(dist(rng));
    }

    static inline const UnaryOperation UNARY_OPERATIONS[]{
            UnaryOperation(std::negate{}, operator-),
    };

    static inline const BinaryOperation BINARY_OPERATIONS[]{
            BinaryOperation(std::plus{}, operator+, [](const Int128 & left, const Int128 & right) {
                return checkInt128Op(left, right, std::plus{});
            }),
            BinaryOperation(std::minus{}, operator-, [](const Int128 & left, const Int128 & right) {
                return checkInt128Op(left, right, std::minus{});
            }),
            BinaryOperation(std::multiplies{}, operator*, [](const Int128 & left, const Int128 & right) {
                return checkInt128Op(left, right, std::multiplies{});
            }),
            BinaryOperation(std::divides{}, operator/, [](const Int128 & left, const Int128 & right) {
                return right != 0 && checkInt128Op(left, right, std::divides{});
            }),
    };
};

using Number = Int128;
