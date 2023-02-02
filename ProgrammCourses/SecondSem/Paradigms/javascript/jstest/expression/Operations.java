package jstest.expression;

import java.util.Arrays;
import java.util.OptionalDouble;
import java.util.function.*;
import java.util.stream.DoubleStream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Operations {
    Operation ARITH = checker -> {
        checker.unary("negate", "Negate", a -> -a, null);

        checker.any("+", "Add", 0, 2, arith(0, Double::sum));
        checker.any("-", "Subtract", 1, 2, arith(0, (a, b) -> a - b));
        checker.any("*", "Multiply", 0, 2, arith(1, (a, b) -> a * b));
        checker.any("/", "Divide", 1, 2, arith(1, (a, b) -> a / b));
    };

    Operation INFIX_POW = infix("**", "IPow", -300, Math::pow);
    Operation INFIX_LOG = infix("//", "ILog", -300, (a, b) -> Math.log(Math.abs(b)) / Math.log(Math.abs(a)));

    Operation INFIX_BIT_AND     = infix("&",   "BitAnd",  90,  bitwise((a, b) -> a & b));
    Operation INFIX_BIT_OR      = infix("|",   "BitOr",   80,  bitwise((a, b) -> a | b));
    Operation INFIX_BIT_XOR     = infix("^",   "BitXor",  70,  bitwise((a, b) -> a ^ b));
    Operation INFIX_BIT_IMPL    = infix("=>",   "BitImpl", -60, bitwise((a, b) -> ~a | b));
    Operation INFIX_BIT_IFF     = infix("<=>",  "BitIff",  50,  bitwise((a, b) -> ~(a ^ b)));

    private static DoubleBinaryOperator bitwise(final LongBinaryOperator op) {
        return (a, b) -> Double.longBitsToDouble(op.applyAsLong(Double.doubleToLongBits(a), Double.doubleToLongBits(b)));
    }


    Operation PI = constant("pi", Math.PI);
    Operation E = constant("e", Math.E);

    Operation ABS = unary("abs", "Abs", Math::abs, null);
    Operation IFF = fixed("iff", "Iff", 3, args -> args[0] >= 0 ? args[1] : args[2], null);

    Operation SINH = unary("sinh", "Sinh", Math::sinh,
            new int[][]{{1, 1, 1}, {6, 1, 1}, {10, 15, 1}, {10, 10, 1}, {51, 51, 40}, {30, 21, 21}});
    Operation COSH = unary("cosh", "Cosh", Math::cosh,
            new int[][]{{1, 1, 1}, {6, 1, 1}, {10, 15, 1}, {10, 10, 1}, {51, 51, 40}, {30, 22, 22}});

    Operation POW = binary("pow", "Pow", Math::pow,
            new int[][]{{1, 1, 1}, {1, 28, 1}, {11, 1, 1}, {15, 33, 1}, {51, 51, 35}, {53, 38, 33}, {71, 89, 39}, {53, 71, 57}});
    Operation LOG = binary("log", "Log", (a, b) -> Math.log(Math.abs(b)) / Math.log(Math.abs(a)),
            new int[][]{{1, 1, 1}, {1, 22, 1}, {44, 1, 1}, {44, 27, 1}, {38, 38, 74}, {43, 68, 63}, {87, 70, 76}, {116, 99, 45}});

    Operation GAUSS = fixed("gauss", "Gauss", 4, args -> gauss(args[0], args[1], args[2], args[3]),
            new int[][]{{1, 1, 1}, {1, 1, 1}, {1, 1, 1}, {34, 57, 64}, {31, 51, 58}, {47, 1, 1}, {247, 129, 185}, {1007, 693, 763}});

    Operation EXP = unary("exp", "Exp", Math::exp,
            new int[][]{{1, 1, 1}, {5, 1, 1}, {9, 14, 1}, {9, 9, 1}, {48, 48, 37}, {27, 22, 22}});
    Operation LN = unary("ln", "Ln", Math::log,
            new int[][]{{1, 1, 1}, {5, 1, 1}, {9, 14, 1}, {9, 9, 1}, {46, 46, 36}, {26, 22, 22}});


    private static double gauss(final double a, final double b, final double c, final double x) {
        final double q = (x - b) / c;
        return a * Math.exp(-q * q / 2);
    }

    static Operation avg(final int arity) {
        return fix("avg", "Avg", arity, DoubleStream::average);
    }

    static Operation med(final int arity) {
        return fix("med", "Med", arity, args -> {
            final double[] sorted = args.sorted().toArray();
            return OptionalDouble.of(sorted[sorted.length / 2]);
        });
    }

    static Operation min(final int arity) {
        return fix("min", "Min", arity, DoubleStream::min);
    }

    static Operation max(final int arity) {
        return fix("max", "Max", arity, DoubleStream::max);
    }

    Operation MEAN = any("mean", "Mean", 1, 5, Operations::mean);
    Operation VAR = any("var", "Var", 1, 5, Operations::var);
    Operation VARN = any("varn", "Varn", 1, 5, Operations::var);

    Operation SUMEXP = any("sumexp", "Sumexp", 0, 3, Operations::sumexp);
    Operation SOFTMAX = any("softmax", "Softmax", 1, 5, args -> Math.exp(args[0]) / sumexp(args));

    private static double sumexp(final double[] args) {
        return Arrays.stream(args).map(Math::exp).sum();
    }

    private static double mean(final double[] args) {
        return Arrays.stream(args).sum() / args.length;
    }

    private static double var(final double[] args) {
        final double mean = mean(args);
        return Arrays.stream(args).map(a -> a - mean).map(a -> a * a).sum() / args.length;
    }

    private static Operation fix(final String name, final String alias, final int arity, final Function<DoubleStream, OptionalDouble> f) {
        final BaseTester.Func wf = args -> f.apply(Arrays.stream(args)).orElseThrow();
        return arity >= 0
               ? fixed(name + arity, alias + arity, arity, wf, null)
               : any(name, alias, -arity - 1, -arity - 1, wf);
    }

    private static Operation any(final String name, final String alias, final int minArity, final int fixedArity, final BaseTester.Func f) {
        return checker -> checker.any(name, alias, minArity, fixedArity, f);
    }

    private static Operation constant(final String name, final double value) {
        return checker -> checker.constant(name, value);
    }

    private static Operation unary(final String name, final String alias, final DoubleUnaryOperator op, final int[][] simplifications) {
        return checker -> checker.unary(name, alias, op, simplifications);
    }

    private static Operation binary(final String name, final String alias, final DoubleBinaryOperator op, final int[][] simplifications) {
        return checker -> checker.binary(name, alias, op, simplifications);
    }

    private static Operation fixed(final String name, final String alias, final int arity, final BaseTester.Func f, final int[][] simplifications) {
        return checker -> checker.fixed(name, alias, arity, f, simplifications);
    }

    private static BaseTester.Func arith(final double zero, final DoubleBinaryOperator f) {
        return args -> args.length == 0 ? zero
                : args.length == 1 ? f.applyAsDouble(zero, args[0])
                : Arrays.stream(args).reduce(f).orElseThrow();
    }

    private static Operation infix(final String name, final String alias, final int priority, final DoubleBinaryOperator op) {
        return checker -> checker.infix(name, alias, priority, op);
    }

}
