package jstest.functional;

import base.Selector;
import base.TestCounter;
import jstest.JSTester;
import jstest.expression.AbstractTests;
import jstest.expression.Dialect;
import jstest.expression.Language;

import java.nio.file.Path;
import java.util.function.Consumer;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class ExpressionTest {
    public static final Dialect ARITHMETIC = new Dialect("variable('%s')", "cnst(%s)", "{op}({args})", ", ")
                    .renamed("+", "add", "-", "subtract", "/", "divide", "*", "multiply");

    public static final Dialect POLISH = new Dialect("%s", "%s", "{args} {op}", " ");
    private static final Path SCRIPT = Path.of("functionalExpression.js");

    private ExpressionTest() {
    }

    public static JSTester tester(final TestCounter counter, final Language language) {
        return tester(counter, language, counter.mode() >= 1, SCRIPT);
    }

    /* package-private */ static JSTester tester(
            final TestCounter counter,
            final Language language,
            final boolean testParsing,
            final Path script
    ) {
        return new JSTester(counter, language, testParsing, script, "", "toString", "parse");
    }

    private static Consumer<TestCounter> v(final Path script, final AbstractTests tests) {
        return counter -> tester(counter, new Language(ARITHMETIC, POLISH, tests), counter.mode() == 1, script).test();
    }

    public static final Selector SELECTOR = new Selector(ExpressionTest.class, "easy", "hard")
            .variant("Mini", v(Path.of("functionalMiniExpression.js"), new AbstractTests() {{
                tests(c(10), variable("x", 0));
            }}))
            .variant("base", v(SCRIPT, new AbstractTests() {{
                final TestExpression vx = variable("x", 0);

                //noinspection Convert2MethodRef
                binary("+", (a, b) -> a + b);
                binary("-", (a, b) -> a - b);
                binary("*", (a, b) -> a * b);
                binary("/", (a, b) -> a / b);

                tests(
                        c(10),
                        vx,
                        f("+", vx, c(2)),
                        f("-", c(3), vx),
                        f("*", c(4), vx),
                        f("/", c(5), vx),
                        f("/", vx, f("*", f("+", vx, c(1)), vx)),
                        f("+", f("+", f("*", vx, vx), f("*", vx, vx)), f("*", vx, vx)),
                        f("-", f("+", f("*", vx, vx), f("*", c(5), f("*", vx, f("*", vx, vx)))), f("*", vx, c(8)))
                );
            }}));

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
