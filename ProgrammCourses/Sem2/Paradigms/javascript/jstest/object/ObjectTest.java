package jstest.object;

import base.Selector;
import jstest.expression.Builder;
import jstest.functional.ExpressionTest;

import static jstest.expression.Operations.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class ObjectTest {
    /* package-private */
    static Selector.Composite<Builder> selector() {
         return Builder.selector(
                ObjectTest.class,
                mode -> false,
                (builder, counter) -> new ObjectTester(
                        counter,
                        builder.aliased(ObjectTester.OBJECT, ExpressionTest.POLISH),
                        "toString", "parse"
                ),
                "easy", "", "hard", "bonus"
        );
    }

    public static final Selector SELECTOR = selector()
            .variant("Base")
            .variant("PowLog", POW, LOG)
            .variant("Gauss", GAUSS)
            .variant("SinhCosh", SINH, COSH)
            .variant("MinMax", min(3), max(5))
            .selector();

    private ObjectTest() {
    }

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
