package info.kgeorgiy.ja.levitskii.i18n.statisitics;

import org.junit.runner.JUnitCore;

public class Tester {
    public static void main(String[] args) {
        //by kgeorgiy's Tester
        final long start = System.currentTimeMillis();


        new JUnitCore().run(TestTextAnalyze.class);

        System.out.println("============================");
        final long time = System.currentTimeMillis() - start;
        System.out.printf("OK %s in %dms %n", TestTextAnalyze.class.getName(), time);
    }
}
