package expression.exceptions;

import expression.TripleExpression;
import expression.common.Selector;

import static expression.parser.Operations.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class ExceptionsTest {
    private static final ExpressionParser PARSER = new ExpressionParser();
    private static final Operation TRIPLE = kind(TripleExpression.KIND, (expr, variables) -> PARSER.parse(expr));

    public static final Selector<?> SELECTOR = Selector.create(ExceptionsTest.class, ExceptionsTester::new, "easy", "hard")
            .variant("Base", TRIPLE, ADD, SUBTRACT, MULTIPLY, DIVIDE, NEGATE)
            .variant("Shifts", SHIFT_L, SHIFT_R, SHIFT_A)
            .variant("MinMax", MIN, MAX)
            .variant("PowLog", POW, LOG)
            .variant("Abs", ABS)
            .variant("Zeroes", L_ZEROES, T_ZEROES)
            ;

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
