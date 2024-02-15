package info.kgeorgiy.java.advanced.student;

import info.kgeorgiy.java.advanced.base.BaseTester;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class Tester {
    private Tester() {
    }

    public static void main(final String... args) {
        new BaseTester()
                .add("StudentQuery", StudentQueryTest.class)
                .add("GroupQuery", GroupQueryTest.class)
                .add("AdvancedQuery", AdvancedQueryTest.class)
                .run(args);
    }
}
