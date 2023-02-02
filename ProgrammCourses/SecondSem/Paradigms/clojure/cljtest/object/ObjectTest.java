package cljtest.object;

import base.Selector;

import static jstest.expression.Operations.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class ObjectTest {
    public static final Selector SELECTOR = ObjectTester.builder()
            .variant("Base",            ARITH)
            .variant("MeanVarn",        ARITH, MEAN,        VARN)
            .variant("SumexpSoftmax",   ARITH, SUMEXP,      SOFTMAX)
            .variant("PowLog",          ARITH, POW,         LOG)
            .variant("ExpLn",           ARITH, EXP,         LN)
            .selector();

    private ObjectTest() {
    }

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
