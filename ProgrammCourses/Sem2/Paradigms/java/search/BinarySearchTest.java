package search;

import base.MainChecker;
import base.Runner;
import base.Selector;
import base.TestCounter;

import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.IntBinaryOperator;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class BinarySearchTest {
    public static final int[] SIZES = {5, 4, 2, 1, 10, 100, 300};
    public static final int[] VALUES = new int[]{5, 4, 2, 1, 0, 10, 100, Integer.MAX_VALUE / 2};

    private BinarySearchTest() {
    }

    /* package-private */ static long[] base(final int c, final int x, final int[] a) {
        for (int i = 0; i < a.length; i++) {
            if (Integer.compare(a[i], x) != c) {
                return longs(i);
            }
        }
        return longs(a.length);
    }

    private static Sorted uni(final BiFunction<Integer, int[], Integer> answer) {
        return (tester, a, b) -> {
            tester.test(answer.apply(a.length, a), a);
            for (int k = 0, i = a.length - 1; k < b.length && i >= 0; k++, i--) {
                a[i] = b[k];
                tester.test(answer.apply(i, a), a);
            }
        };
    }

    private static long[] missing(final int c, final int x, final int[] a) {
        for (int i = 0; i < a.length; i++) {
            if (a[i] == x) {
                return longs(i);
            }
            if (Integer.compare(x, a[i]) == c) {
                return longs(-1 - i);
            }
        }
        return longs(-1 - a.length);
    }


    private static Sorted cycle(final BiFunction<Integer, int[], Integer> answer) {
        return (tester, a, b) -> {
            for (int k = 0; k < a.length; k++) {
                tester.test(answer.apply(k, a), a);

                final int last = a[a.length - 1];
                System.arraycopy(a, 0, a, 1, a.length - 1);
                a[0] = last;
            }
        };
    }

    private static final Sorted CYCLE = cycle((k, a) -> k);


    private static Sorted uniE(final IntBinaryOperator op) {
        return uni((k, a) -> k < 1 ? a[k] : k == a.length ? a[k - 1] : op.applyAsInt(a[k], a[k - 1]));
    }

    private static Sorted uniI(final IntBinaryOperator op) {
        return uni((k, a) -> k != 0 && (k == a.length || op.applyAsInt(a[k], a[k - 1]) == a[k - 1] && a[k] != a[k - 1]) ? k - 1 : k);
    }

    public static final Selector SELECTOR = new Selector(BinarySearchTest.class)
            .variant("Base", Solver.variant("", false, BinarySearchTest::base))
            .variant("Missing",   Solver.variant("Missing", true,   BinarySearchTest::missing))
            .variant("Shift",     sorted("Shift",   true,   CYCLE))
            .variant("Min",  sorted("Min",     false,  uniE(Math::min)))
            .variant("Uni",  sorted("Uni",     false,  uniI(Math::min)))
            ;

    public static long[] longs(final long... longs) {
        return longs;
    }

    public static void main(final String... args) {
        SELECTOR.main(args);
    }

    /* package-private */ static Consumer<TestCounter> variant(final String name, final Consumer<Variant> variant) {
        final String className = "search.BinarySearch" + name;
        return counter -> variant.accept(new Variant(counter, new MainChecker(Runner.args(className))));
    }

    /* package-private */ interface Solver {
        static Consumer<TestCounter> variant(final String name, final boolean asc, final Solver solver) {
            final Sampler sampler = new Sampler(asc, true, true);
            return BinarySearchTest.variant(name, vrt -> {
                final int c = asc ? - 1 : 1;
                solver.test(vrt, c, 0);
                solver.test(vrt, c, 0, 0);
                for (final int s1 : SIZES) {
                    final int size = s1 > 3 * TestCounter.DENOMINATOR ? s1 / TestCounter.DENOMINATOR : s1;
                    for (final int max : VALUES) {
                        final int[] a = sampler.sample(vrt, size, max);
                        for (int i = 0; i < size; i++) {
                            solver.test(vrt, c, a[i], a);
                            if (i != 0) {
                                solver.test(vrt, c, (a[i - 1] + a[i]) / 2, a);
                            }
                        }
                        solver.test(vrt, c, Integer.MIN_VALUE, a);
                        solver.test(vrt, c, Integer.MAX_VALUE, a);
                    }
                }
            });
        }

        long[] solve(final int c, final int x, final int... a);

        default void test(final Variant variant, final int c, final int x, final int... a) {
            final String expected = Arrays.stream(solve(c, x, a))
                    .mapToObj(Long::toString)
                    .collect(Collectors.joining(" "));
            variant.test(IntStream.concat(IntStream.of(x), IntStream.of(a)), expected);
        }
    }

    /* package-private */ static Consumer<TestCounter> sorted(final String name, final boolean asc, final Sorted solver) {
        final Sampler sampler = new Sampler(asc, false, false);
        return BinarySearchTest.variant(name, variant -> {
            variant.test(0, 0);
            for (final int s : SIZES) {
                final int size = s > 3 * TestCounter.DENOMINATOR ? s / TestCounter.DENOMINATOR : s;
                for (final int max : VALUES) {
                    solver.test(variant, sampler.sample(variant, size, max), sampler.sample(variant, size, max));
                }
            }
        });
    }

    /* package-private */ interface Sorted {
        void test(final Variant variant, final int[] a, final int[] b);
    }

    public static class Variant {
        private final TestCounter counter;
        private final MainChecker checker;

        public Variant(final TestCounter counter, final MainChecker checker) {
            this.counter = counter;
            this.checker = checker;
        }

        void test(final IntStream ints, final String expected) {
            final List<String> input = ints.mapToObj(Integer::toString).collect(Collectors.toUnmodifiableList());
            checker.testEquals(counter, input, List.of(expected));
        }

        public void test(final int expected, final int... a) {
            test(Arrays.stream(a), Integer.toString(expected));
        }
    }

    public static class Sampler {
        private final boolean asc;
        private final boolean dups;
        private final boolean zero;

        public Sampler(final boolean asc, final boolean dups, final boolean zero) {
            this.asc = asc;
            this.dups = dups;
            this.zero = zero;
        }

        public int[] sample(final Variant variant, final int size, final int max) {
            final IntStream sorted = variant.counter.random().getRandom().ints(zero ? size : Math.max(size, 1), -max, max + 1).sorted();
            final int[] ints = (dups ? sorted : sorted.distinct()).toArray();
            if (!asc) {
                final int sz = ints.length;
                for (int i = 0; i < sz / 2; i++) {
                    final int t = ints[i];
                    ints[i] = ints[sz - i - 1];
                    ints[sz - i - 1] = t;
                }
            }
            return ints;
        }
    }
}
