package cljtest.parsing;

import base.Selector;
import jstest.expression.AbstractTests;
import jstest.expression.Operation;

import java.util.function.BiConsumer;

import static jstest.expression.Operations.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class ParserTest {
    private static final Operation VARIABLES = checker -> {
        final AbstractTests t = checker.getTests();
        final BiConsumer<Character, Integer> var = (first, index) -> {
            final char prefix = t.random().nextBoolean() ? first : Character.toUpperCase(first);
            t.variable(prefix + t.random().randomString("xyzXYZ"), index);
        };
        for (int i = 0; i < 10; i++) {
            var.accept('x', 0);
            var.accept('y', 1);
            var.accept('z', 2);
        }
    };

    private static final Selector SELECTOR = ParserTester.builder()
            .variant("Base",                       ARITH)
            .variant("Variables",       VARIABLES, ARITH)
            .variant("Bitwise",         VARIABLES, ARITH, INFIX_BIT_AND,    INFIX_BIT_OR,   INFIX_BIT_XOR)
            .variant("PowLog",          VARIABLES, ARITH, INFIX_POW,   INFIX_LOG)
            .variant("BitImplIff",      VARIABLES, ARITH, INFIX_BIT_AND,    INFIX_BIT_OR,   INFIX_BIT_XOR, INFIX_BIT_IMPL, INFIX_BIT_IFF)
            .selector();

    private ParserTest() {
    }

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
