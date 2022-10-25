package expression.parser;

import base.ExtendedRandom;
import base.TestCounter;
import base.Unit;
import expression.ToMiniString;
import expression.common.BaseTester;
import expression.common.ExpressionKind;
import expression.common.Renderer;
import expression.common.TestGenerator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.LongBinaryOperator;
import java.util.function.LongUnaryOperator;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ParserTester extends BaseTester {
    /* package-private */ final TestGenerator<Integer> generator;
    /* package-private */ final Renderer<Integer, Unit, ParserTestSet.TExpression> renderer;
    private final List<ParserTestSet.ParsedKind<?, ?>> kinds = new ArrayList<>();

    public ParserTester(final TestCounter counter, final int mode) {
        super(counter, mode);
        renderer = new Renderer<>(c -> vars -> c);
        final ExtendedRandom random = new ExtendedRandom();
        generator = new TestGenerator<>(counter, random, random::nextInt, ParserTestSet.CONSTS, true);
    }

    public void unary(final String name, final LongUnaryOperator op) {
        generator.unary(name);
        renderer.unary(name, (unit, a) -> vars -> cast(op.applyAsLong(a.evaluate(vars))));
    }

    public void binary(final String name, final int priority, final LongBinaryOperator op) {
        generator.binary(name, priority);
        renderer.binary(name, (unit, a, b) -> vars -> cast(op.applyAsLong(a.evaluate(vars), b.evaluate(vars))));
    }

    <E extends ToMiniString, C> void kind(final ExpressionKind<E, C> kind, final ParserTestSet.Parser<E> parser) {
        kinds.add(new ParserTestSet.ParsedKind<>(kind, parser));
    }

    @Override
    protected void test() {
        for (final ParserTestSet.ParsedKind<?, ?> kind : kinds) {
            test(kind);
        }
    }

    protected void test(final ParserTestSet.ParsedKind<?, ?> kind) {
        new ParserTestSet<>(this, kind).test();
    }

    public TestCounter getCounter() {
        return counter;
    }

    public ExtendedRandom getRandom() {
        return random;
    }

    protected int cast(final long value) {
        return (int) value;
    }
}
