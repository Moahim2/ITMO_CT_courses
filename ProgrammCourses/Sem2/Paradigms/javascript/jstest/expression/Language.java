package jstest.expression;

import jstest.expression.BaseTester.Expr;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Language {
    private final Dialect parsed;
    private final Dialect unparsed;
    private final AbstractTests template;
    private final List<Expr> tests;

    public Language(final Dialect parsed, final Dialect unparsed, final AbstractTests template) {
        this.parsed = parsed;
        this.unparsed = unparsed;

        this.template = template;
        tests = template.renderTests(parsed, unparsed);
    }

    public Expr randomTest(final int size) {
        return template.randomTest(size, parsed, unparsed);
    }

    public List<int[]> getSimplifications() {
        return template.getSimplifications();
    }

    public List<AbstractTests.TestExpression> getVariables() {
        return template.getVariables();
    }

    public List<Expr> getTests() {
        return tests;
    }
}
