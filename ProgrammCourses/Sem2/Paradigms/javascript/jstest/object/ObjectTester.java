package jstest.object;

import base.TestCounter;
import jstest.Engine;
import jstest.JSTester;
import jstest.expression.Dialect;
import jstest.expression.Diff;
import jstest.expression.Language;

import java.nio.file.Path;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ObjectTester extends JSTester {
    public static final Dialect OBJECT = new Dialect("new Variable('%s')", "new Const(%s)", "new {op}({args})", ", ")
            .renamed("+", "Add", "-", "Subtract", "*", "Multiply", "/", "Divide", "negate", "Negate");

    private static final Diff DIFF = new Diff(2, N, new Dialect(
            "'%s'", "%s",
            (name, args) -> String.format("%s.%s(%s)", args.get(0), name, String.join(", ", args.subList(1, args.size())))
    ));

    protected ObjectTester(final TestCounter counter, final Language language, final String toString, final String parse) {
        super(counter, language, true, Path.of("objectExpression.js"), ".evaluate", toString, parse);

        if (counter.mode() >= 2) {
            DIFF.diff(this);
        }
        if (counter.mode() >= 3) {
            DIFF.simplify(this);
        }
    }

    @Override
    protected void test(final Engine.Result<Object> prepared, final String unparsed) {
        counter.test(() -> engine.toString(prepared).assertEquals(unparsed));
    }
}
