package cljtest.functional;

import base.ExtendedRandom;
import base.Selector;
import base.TestCounter;
import cljtest.ClojureEngine;
import jstest.Engine;
import jstest.expression.BaseTester;
import jstest.expression.Builder;
import jstest.expression.Dialect;
import jstest.expression.Language;

import java.util.Optional;
import java.util.function.BiFunction;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class FunctionalTester extends BaseTester<Object, ClojureEngine> {
    public static final Dialect PARSED = new Dialect("(variable \"%s\")", "(constant %s.0)", "({op} {args})", " ")
            .renamed("+", "add", "-", "subtract", "*", "multiply", "/", "divide");
    public static final Dialect UNPARSED = new Dialect("%s", "%s.0", "({op} {args})", " ");

    /* package-private*/ static Selector.Composite<Builder> builder() {
        return Builder.selector(
                FunctionalTester.class,
                mode -> mode >= 1,
                (builder, counter) -> new FunctionalTester(
                        counter,
                        builder.language(PARSED, UNPARSED),
                        Optional.empty(),
                        "parseFunction", "", (a, b) -> b
                ),
                "easy", "hard"
        );
    }

    private final BiFunction<ExtendedRandom, String, String> spoiler;

    protected FunctionalTester(
            final TestCounter counter,
            final Language language,
            final Optional<String> evaluate,
            final String parse,
            final String toString,
            final BiFunction<ExtendedRandom, String, String> spoiler
    ) {
        super(counter, new ClojureEngine("expression.clj", evaluate, parse, toString), language, true);
        this.spoiler = spoiler;
    }

    @Override
    public Engine.Result<Object> parse(final String expression) {
        return super.parse(spoiler.apply(random(), expression));
    }
}
