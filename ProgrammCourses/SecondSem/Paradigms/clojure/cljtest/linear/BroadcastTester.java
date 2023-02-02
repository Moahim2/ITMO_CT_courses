package cljtest.linear;

import base.TestCounter;

import java.util.Arrays;
import java.util.List;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BroadcastTester extends LinearTester {
    public static final List<Item.Fun> BROADCAST = Item.functions("hb");

    public BroadcastTester(final TestCounter counter) {
        super(counter);
    }

    @Override
    protected void test(final int args) {
        super.test(args);

        for (int complexity = 0; complexity <= 20 / TestCounter.DENOMINATOR2; complexity++) {
            final int[] dims = tensor(complexity).toArray();
            final Supplier<Item> generator = Item.generator(dims);

            test(args, BROADCAST, generator);

            final List<Supplier<Item>> slices = slice(0, dims).map(Item::generator).collect(Collectors.toUnmodifiableList());
            test(dims, Stream.generate(() -> random().randomItem(slices)).limit(args));

            if (isHard() && args == 2) {
                expectException(BROADCAST, dims, new int[][]{});
            }
        }
    }

    private void test(final int[] dims, final Stream<Supplier<Item>> generators) {
        final List<Item> args = generators.map(Supplier::get).map(item -> item.refill(random())).collect(Collectors.toUnmodifiableList());
        @SuppressWarnings("OptionalGetWithoutIsPresent")
        final int maxDim = args.stream().mapToInt(Item::dim).max().getAsInt();
        final int[] newDims = Arrays.copyOfRange(dims, dims.length - maxDim, dims.length);
        final List<Item> fakeArgs = args.stream().map(arg -> extend(0, arg, newDims)).collect(Collectors.toUnmodifiableList());

        for (final Item.Fun fun : BROADCAST) {
            fun.test(counter, args, fakeArgs);
        }
    }

    private static Item extend(final int depth, final Item item, final int[] dims) {
        if (depth + item.dim() == dims.length) {
            return item;
        }
        final Item prev = extend(depth + 1, item, dims);
        return Item.vector(Stream.generate(() -> prev).limit(dims[depth]));
    }

    private static Stream<int[]> slice(final int index, final int[] dims) {
        final Stream<int[]> self = Stream.of(Arrays.copyOfRange(dims, index, dims.length));
        return index == dims.length ? self : Stream.concat(self, slice(index + 1, dims));
    }

    private IntStream tensor(final int complexity) {
        if (complexity <= 0) {
            return IntStream.of();
        }
        final int dim = 1 + random().nextInt(Math.min(complexity, 4));
        return IntStream.concat(IntStream.of(dim), tensor(complexity - dim));
    }
}
