package cljtest.linear;

import base.ExtendedRandom;
import base.Selector;
import base.TestCounter;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import java.util.Arrays;
import java.util.stream.IntStream;


/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class LinearTest {
    // Cuboid
    private static final List<Item.Fun> CUBOID = Item.functions("c");

    private static void cuboid(final Test test) {
        for (int complexity = 1; complexity < 10 / TestCounter.DENOMINATOR2; complexity++) {
            for (int size1 = 1; size1 < complexity; size1++) {
                for (int size2 = 1; size1 + size2 < complexity; size2++) {
                    test.test(Item.generator(size1, size2, complexity - size1 - size2));
                }
            }
        }

        if (test.args == 2 && test.isHard()) {
            test.expectException(new int[]{3, 3, 3}, new int[][]{{}, {3}, {3, 3}, {3, 3, 3, 3}});
        }
    }


    // Shapeless
    private static final List<Item.Fun> SHAPELESS = Item.functions("s");

    private static Item genShapeless(final ExtendedRandom random, final int complexity) {
        if (complexity == 0) {
            return Item.ZERO;
        }
        final int[] parts = new int[1 + random.nextInt(Math.min(complexity, 5))];
        for (int i = parts.length; i < complexity; i++) {
            parts[random.nextInt(parts.length)]++;
        }
        return Item.vector(Arrays.stream(parts).mapToObj(c -> genShapeless(random, c)));
    }

    private static void shapeless(final Test test) {
        IntStream.range(0, 100 / TestCounter.DENOMINATOR2)
                .forEachOrdered(complexity -> test.test(() -> genShapeless(test.random(), complexity)));
    }


    // Selector
    public static final Selector SELECTOR = new Selector(LinearTester.class, "easy", "hard")
            .variant("Base", v(LinearTester::new))
            .variant("Cuboid", variant(CUBOID, LinearTest::cuboid))
            .variant("Shapeless", variant(SHAPELESS, LinearTest::shapeless))
            .variant("Simplex", v(SimplexTester::new))
            .variant("Broadcast", v(BroadcastTester::new))
            ;

    private LinearTest() {
    }

    /* package-private*/ static Consumer<TestCounter> v(final Function<TestCounter, LinearTester> variant) {
        return counter -> variant.apply(counter).test();
    }

    /* package-private*/ static Consumer<TestCounter> variant(final List<Item.Fun> functions, final Consumer<Test> variant) {
        return v(counter -> new LinearTester(counter) {
            @Override
            protected void test(final int args) {
                variant.accept(new Test(this, functions, args));
            }
        });
    }

    /* package-private */ static class Test {
        private final LinearTester test;
        private final List<Item.Fun> functions;
        /* package-private */ final int args;

        public Test(final LinearTester test, final List<Item.Fun> functions, final int args) {
            this.test = test;
            this.functions = functions;
            this.args = args;
        }

        public void test(final Supplier<Item> generator) {
            test.test(args, functions, generator);
        }

        public boolean isHard() {
            return test.isHard();
        }

        public void expectException(final int[] okDims, final int[][] failDims) {
            test.expectException(functions, okDims, failDims);
        }

        public ExtendedRandom random() {
            return test.random();
        }
    }



    public static void main(final String... args) {
        SELECTOR.main(args);
    }
}
