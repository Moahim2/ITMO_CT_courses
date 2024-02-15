package info.kgeorgiy.java.advanced.arrayset;

import info.kgeorgiy.java.advanced.base.BaseTester;

/**
 * Test runner
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-arrayset">ArraySet</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class Tester {
    private Tester() {
    }

    public static void main(final String... args) {
        new BaseTester()
                .add("SortedSet", SortedSetTest.class)
                .add("NavigableSet", NavigableSetTest.class)
                .add("AdvancedSet", AdvancedSetTest.class)
                .run(args);
    }
}
