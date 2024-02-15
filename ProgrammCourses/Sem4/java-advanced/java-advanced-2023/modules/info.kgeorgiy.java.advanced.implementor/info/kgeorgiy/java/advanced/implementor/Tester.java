package info.kgeorgiy.java.advanced.implementor;

import info.kgeorgiy.java.advanced.base.BaseTester;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class Tester {
    private Tester() {
    }

    public static void main(final String... args) {
        new BaseTester()
                .add("interface", InterfaceImplementorTest.class)
                .add("class", ClassImplementorTest.class)
                .add("advanced", AdvancedImplementorTest.class)
                .add("covariant", CovariantImplementorTest.class)
                .add("jar-interface", InterfaceJarImplementorTest.class)
                .add("jar-class", ClassJarImplementorTest.class)
                .add("jar-advanced", AdvancedJarImplementorTest.class)
                .run(args);
    }
}
