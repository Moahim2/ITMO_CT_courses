package cljtest.linear;

import base.TestCounter;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class SimplexTester extends LinearTester {
    public static final List<Item.Fun> SIMPLEX = Item.functions("x");

    public SimplexTester(final TestCounter counter) {
        super(counter);
    }

    @Override
    protected void test(final int argc) {
        super.test(argc);

        for (int complexity = 1; complexity <= 12 / TestCounter.DENOMINATOR2; complexity++) {
            for (int size = 1; size < complexity; size++) {
                final int dim = complexity - size;

                final int sz = size;
                test(argc, SIMPLEX, () -> generate(sz, dim));

                for (final Item.Fun fun : SIMPLEX) {
                    fun.test(counter, args(argc, size, dim));

                    if (isHard()) {
                        if (argc > 1) {
                            final List<Item> args = args(argc, size, dim).collect(Collectors.toList());
                            final int index = random().nextInt(args.size());
                            args.set(index, corrupt(args.get(index)));
                            fun.expectException(counter, args.stream());
                        }
                        if (dim > 1) {
                            fun.expectException(counter, args(argc, corrupt(generate(size, dim))));
                        }
                    }
                }
            }
        }
    }

    private Item corrupt(final Object arg) {
        final Item.Vector vector = (Item.Vector) arg;
        final int size = vector.size();
        final int index = random().nextInt(size);
        if (vector.get(0) instanceof Item.Value) {
            return Item.vector(Stream.generate(() -> Item.ZERO).limit(size + (random().nextBoolean() ? 1 : -1)));
        } else if (random().nextInt(5) > 0) {
            return Item.vector(IntStream.range(0, size).mapToObj(i -> i == index ? corrupt(vector.get(index)) : vector.get(index)));
        } else if (random().nextBoolean() || index == 0) {
            return Item.vector(IntStream.range(0, size)
                    .flatMap(i -> i == index ? IntStream.of(i, i) : IntStream.of(i))
                    .mapToObj(vector::get));
        } else {
            return Item.vector(IntStream.range(0, size).filter(i -> i != index).mapToObj(vector::get));
        }
    }

    private Stream<Item> args(final int argc, final int size, final int dim) {
        return args(argc, generate(size, dim));
    }

    private Stream<Item> args(final int argc, final Item shape) {
        return Item.args(argc, shape, random());
    }

    private static Item.Vector generate(final int size, final int dim) {
        final Stream<Item> items = dim == 1
                ? Stream.generate(() -> Item.ZERO).limit(size)
                : IntStream.rangeClosed(0, size - 1).mapToObj(i -> generate(size - i, dim - 1));
        return Item.vector(items);
    }
}
