package jstest.expression;

import base.Selector;
import base.TestCounter;
import base.Tester;
import jstest.ArithmeticTests;

import java.util.HashMap;
import java.util.Map;
import java.util.function.*;
import java.util.stream.Stream;

import static jstest.expression.AbstractTests.c;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class Builder {
    private final ArithmeticTests tests = new ArithmeticTests();
    private final boolean testMulti;
    private final Map<String, String> aliases = new HashMap<>();

    private final AbstractTests.TestExpression vx = tests.vx, vy = tests.vy, vz = tests.vz;
    private final Supplier<AbstractTests.TestExpression> constGenerator = () -> c(tests.random().nextInt(10));
    private final Supplier<AbstractTests.TestExpression> variableGenerator = () -> tests.random().randomItem(tests.getVariables());
    private final Supplier<AbstractTests.TestExpression> generator = () -> tests.random().nextBoolean()
            ? variableGenerator.get()
            : constGenerator.get();

    public Builder(final boolean testMulti) {
        this.testMulti = testMulti;
    }

    public static base.Selector.Composite<Builder> selector(
            final Class<?> owner,
            final IntPredicate testMulti,
            final BiFunction<Builder, TestCounter, Tester> tester,
            final String... modes
    ) {
        return Selector.composite(
                owner,
                counter -> new Builder(testMulti.test(counter.mode())),
                (builder, counter) -> tester.apply(builder, counter).test(),
                modes
        );
    }

    private AbstractTests.TestExpression f(final String name, final AbstractTests.TestExpression... args) {
        return tests.f(name, args);
    }

    private void tests(final int[][] simplifications, final AbstractTests.TestExpression... tests) {
        this.tests.tests(simplifications, tests);
    }

    public void constant(final String name, final double value) {
        final BaseTester.Func expr = vars -> value;
        tests.nullary(name, expr);
        final AbstractTests.TestExpression constant = (parsed, unparsed) -> new BaseTester.Expr(name, name, expr);
        tests(null,
                f("+", constant, vx),
                f("-", vy, constant),
                f("*", vz, constant),
                f("/", constant, vx)
        );
    }

    public void unary(final String name, final String alias, final DoubleUnaryOperator op, final int[][] simplifications) {
        tests.unary(name, op);
        aliases.put(name, alias);
        unaryTests(name, simplifications);
    }

    private void unaryTests(final String name, final int[][] simplifications) {
        tests(simplifications,
                f(name, c(2)),
                f(name, vx),
                f(name, f("-", vx, vy)),
                f(name, f("+", vx, vy)),
                f(name, f("/", f(name, vz), f("+", vx, vy))),
                f("+", f(name, f(name, f("+", vx, c(10)))), f("*", vz, f("*", vy, f(name, c(4)))))
        );
    }

    public void binary(final String name, final String alias, final DoubleBinaryOperator op, final int[][] simplifications) {
        tests.binary(name, op);
        aliases.put(name, alias);
        binaryTests(name, simplifications);
    }

    public void infix(final String name, final String alias, final int priority, final DoubleBinaryOperator op) {
        tests.infix(name, priority, op);
        aliases.put(name, alias);
        binaryTests(name, null);
    }

    private void binaryTests(final String name, final int[][] simplifications) {
        tests(simplifications,
                f(name, c(2), c(3)),
                f(name, c(2), vy),
                f(name, vx, c(3)),
                f(name, vx, vy),
                f(name, f("negate", vz), f("+", vx, vy)),
                f(name, f("-", vz, vy), f("negate", vx)),
                f(name, f("negate", vz), f(name, vx, vy)),
                f(name, f(name, vx, vy), f("negate", vz))
        );
    }

    public void fixed(final String name, final String alias, final int arity, final BaseTester.Func f, final int[][] simplifications) {
        tests.fixed(name, arity, f);
        aliases.put(name, alias);

        if (arity == 1) {
            unaryTests(name, simplifications);
        } else if (arity == 2) {
            binaryTests(name, simplifications);
        } else if (arity == 4) {
            final AbstractTests.TestExpression e1 = f(name, vx, vy, vz, c(0));
            final AbstractTests.TestExpression e2 = f(name, vx, vy, vz, c(1));
            final AbstractTests.TestExpression e3 = f(name, c(1), c(2), c(3), vx);
            final AbstractTests.TestExpression e4 = f(name, f("+", vx, vy), f("-", vy, vz), f("*", vz, vx), f("/", vx, c(3)));
            tests(
                    simplifications,
                    f(name, c(1), c(2), c(3), c(4)),
                    f(name, c(0), vx, vy, vz),
                    f(name, c(0), c(0), c(1), vz),
                    e1,
                    e2,
                    e3,
                    e4,
                    f(name, e1, e2, e3, e4)
            );
        } else {
            f(name, arity, constGenerator);
            f(name, arity, variableGenerator);
            for (int i = 1; i < 10; i++) {
                f(name, arity, generator);
            }
        }
    }

    public AbstractTests.TestExpression f(final String name, final int arity, final Supplier<AbstractTests.TestExpression> generator) {
        return f(name, Stream.generate(generator).limit(arity).toArray(AbstractTests.TestExpression[]::new));
    }

    public void any(final String name, final String alias, final int minArity, final int fixedArity, final BaseTester.Func f) {
        tests.any(name, minArity, 5, f);
        aliases.put(name, alias);
        if (testMulti) {
            tests.any(name, minArity, 5, f);
        } else {
            tests.fixed(name, fixedArity, f);
        }

        if (testMulti) {
            tests.tests(
                    f(name, vx),
                    f(name, vx, vy, vz),
                    f(name, vx, vy, vz, c(3), c(5)),
                    f(name, f("+", vx, c(2))),
                    f(name, f("+", vx, vy))
            );
        }

        for (int i = 1; i < 10; i++) {
            tests.tests(f(name, testMulti ? i : fixedArity, generator));
        }
    }

    public AbstractTests getTests() {
        return tests;
    }

    public Language language(final Dialect parsed, final Dialect unparsed) {
        return new Language(parsed, unparsed, tests);
    }

    public Language aliased(final Dialect parsed, final Dialect unparsed) {
        return language(dialect(parsed, s -> true), unparsed);
    }

    public Dialect dialect(final Dialect dialect, final Predicate<String> filter) {
        return dialect.renamed(aliases.entrySet().stream()
                .filter(e -> filter.test(e.getValue()))
                .flatMap(e -> Stream.of(e.getKey(), e.getValue()))
                .toArray(String[]::new));
    }
}
