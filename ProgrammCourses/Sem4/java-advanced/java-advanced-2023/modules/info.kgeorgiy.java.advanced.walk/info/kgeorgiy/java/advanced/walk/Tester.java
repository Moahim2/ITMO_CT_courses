package info.kgeorgiy.java.advanced.walk;

import info.kgeorgiy.java.advanced.base.BaseTester;

/**
 * Test runner
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-walk">Walk</a> homework
 * if <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class Tester {
    private Tester() {
    }

    public static void main(final String... args) {
        new BaseTester()
                .add("Walk", WalkTest.class)
                .add("RecursiveWalk", RecursiveWalkTest.class)
                .add("AdvancedWalk", (tester, cut) -> {
                    tester.test("Walk", cut.replace(".RecursiveWalk", ".Walk"));
                    return tester.test("RecursiveWalk", cut);
                })
                .run(args);
    }
}
