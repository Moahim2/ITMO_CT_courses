package cljtest.functional;

import base.Selector;

import static jstest.expression.Operations.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class FunctionalTest {
    private static final Selector SELECTOR = FunctionalTester.builder()
            .variant("Base",            ARITH)
            .variant("MeanVarn",        ARITH, MEAN,        VARN)
            .variant("SumexpSoftmax",   ARITH, SUMEXP,      SOFTMAX)
            .variant("PowLog",          ARITH, POW,         LOG)
            .variant("ExpLn",           ARITH, EXP,         LN)
            .selector();

    private FunctionalTest() {
    }

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
