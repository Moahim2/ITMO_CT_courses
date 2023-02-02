package cljtest.object;

import base.ExtendedRandom;
import base.Selector;
import base.TestCounter;
import cljtest.functional.FunctionalTester;
import jstest.Engine;
import jstest.expression.Builder;
import jstest.expression.Dialect;
import jstest.expression.Diff;
import jstest.expression.Language;

import java.util.Optional;
import java.util.function.BiFunction;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ObjectTester extends FunctionalTester {
    public static final Dialect PARSED = new Dialect("(Variable \"%s\")", "(Constant %s.0)", "({op} {args})", " ");
    private static final Diff DIFF = new Diff(1, N, new Dialect("\"%s\"", "%s", "({op} {args})", " "));

    /* package-private*/ static Selector.Composite<Builder> builder() {
        return Builder.selector(
                ObjectTester.class,
                mode -> mode >= 1,
                (builder, counter) -> {
                    final Language language = builder.aliased(PARSED, UNPARSED);
                    return new ObjectTester(
                            counter,
                            language,
                            true,
                            "parseObject", "toString", (a, b) -> b
                    );
                },
                "easy", "hard"
        );
    }

    public ObjectTester(
            final TestCounter counter,
            final Language language,
            final boolean testDiff,
            final String parse, final String toString,
            final BiFunction<ExtendedRandom, String, String> spoiler
    ) {
        super(counter, language, Optional.of("evaluate"), parse, toString, spoiler);
        if (testDiff) {
            DIFF.diff(this);
        }
    }

    @Override
    protected void test(final Engine.Result<Object> prepared, final String unparsed) {
        counter.test(() -> engine.toString(prepared).assertEquals(unparsed));
    }
}
