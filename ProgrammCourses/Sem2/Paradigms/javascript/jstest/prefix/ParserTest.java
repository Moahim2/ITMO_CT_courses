package jstest.prefix;

import base.Selector;
import jstest.ArithmeticTests;
import jstest.expression.Language;
import jstest.object.ObjectTester;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class ParserTest {
    public static final Selector SELECTOR = Selector.composite(
            ParserTest.class,
            counter -> new ParserTester(
                    counter,
                    new Language(ObjectTester.OBJECT, ParserTester.PREFIX, new ArithmeticTests()),
                    "prefix",
                    "parsePrefix",
                    "xyz()+*/@ABC"
            ),
            "", "easy", "hard"
    )
            .variant("Base")
            .selector();

    private ParserTest() {
    }

    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
