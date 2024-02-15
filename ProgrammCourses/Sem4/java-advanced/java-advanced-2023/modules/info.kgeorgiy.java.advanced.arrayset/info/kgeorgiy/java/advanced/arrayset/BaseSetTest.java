package info.kgeorgiy.java.advanced.arrayset;

import info.kgeorgiy.java.advanced.base.BaseTest;
import org.junit.Assert;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import static net.java.quickcheck.generator.CombinedGenerators.lists;
import static net.java.quickcheck.generator.CombinedGeneratorsIterables.someExcludeValues;
import static net.java.quickcheck.generator.CombinedGeneratorsIterables.somePairs;
import static net.java.quickcheck.generator.PrimitiveGenerators.fixedValues;
import static net.java.quickcheck.generator.PrimitiveGenerators.integers;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public abstract class BaseSetTest extends BaseTest {
    protected static final List<NamedComparator> NAMED_COMPARATORS = new ArrayList<>(List.of(
            comparator("Natural order", Integer::compare),
            comparator("Reverse order", Comparator.comparingInt(Integer::intValue).reversed()),
            comparator("Div 128", Comparator.comparingInt(i -> i >> 7)),
            comparator("Mod 1024", Comparator.comparingInt(i -> i & 1023)),
            comparator("Even first", Comparator.<Integer>comparingInt(i -> i % 2).thenComparing(Integer::intValue)),
            comparator("All equal", Comparator.comparingInt(i -> 0))
    ));

    protected BaseSetTest() {
    }

    protected static <E> List<E> toList(final SortedSet<E> set) {
        return new ArrayList<>(set);
    }

    protected static <E extends Number> List<Number> toArray(final SortedSet<E> set) {
        return Arrays.asList(set.toArray(new Number[0]));
    }

    private static <T, S extends SortedSet<T>> S treeSet(final List<T> elements, final Comparator<T> comparator) {
        final TreeSet<T> set = new TreeSet<>(comparator);
        set.addAll(elements);
        return cast(set);
    }

    protected static <T, S extends SortedSet<T>> S set(final List<T> elements, final Comparator<T> comparator) {
        return create(new Object[]{elements, comparator}, Collection.class, Comparator.class);
    }

    protected static <T, S extends SortedSet<T>> S treeSet(final List<T> elements) {
        return cast(new TreeSet<>(elements));
    }

    protected static <T, S extends SortedSet<T>> S set(final List<Integer> elements) {
        return create(new Object[]{elements}, Collection.class);
    }

    protected static <E extends Number, S extends SortedSet<E>> void assertEq(final SetPair<E, S> pair) {
        assertEq(pair.context, pair.model, pair.tested);
    }

    protected static <E extends Number, S extends SortedSet<E>> void assertEq(final String context, final S model, final S tested) {
        Assert.assertEquals("invalid element order " + context, toList(model), toList(tested));
        Assert.assertEquals("invalid toArray " + context, toArray(model), toArray(tested));
        Assert.assertEquals("invalid set size " + context, model.size(), (Object) tested.size());
        Assert.assertEquals("invalid isEmpty " + context, model.isEmpty(), tested.isEmpty());
        Assert.assertSame("invalid comparator " + context, model.comparator(), tested.comparator());
    }

    protected static <T, S extends SortedSet<T>> S set() {
        return create(new Object[]{});
    }

    @SuppressWarnings("unchecked")
    private static <T, S extends SortedSet<T>> S cast(final Object set) {
        return (S) set;
    }

    private static <T, S extends SortedSet<T>> S create(final Object[] params, final Class<?>... types) {
        try {
            return cast(loadClass().getConstructor(types).newInstance(params));
        } catch (final Exception e) {
            throw new AssertionError("Instantiation error", e);
        }
    }

    protected List<Integer> values(final Comparator<? super Integer> comparator, final Collection<Integer> elements) {
        return Stream.of(
                elements.stream(),
                Stream.of(0, Integer.MAX_VALUE, Integer.MIN_VALUE),
                StreamSupport.stream(someExcludeValues(integers(), elements).spliterator(), false)
        ).flatMap(Function.identity()).collect(Collectors.toList());
    }

    protected interface TestCase<T, S extends SortedSet<T>> {
        void test(SetPair<T, S> pair);
    }

    protected interface TestPair<T, S extends SortedSet<T>> {
        void test(SetPair<T, S> pair, T first, T second);
    }

    protected <S extends SortedSet<Integer>> void test(final TestCase<Integer, S> testCase) {
        somePairs(lists(integers()), fixedValues(NAMED_COMPARATORS)).forEach(
                pair -> testCase.test(pair(pair.getFirst(), pair.getSecond())));
    }

    protected <S extends SortedSet<Integer>> SetPair<Integer, S> pair(final List<Integer> elements, final NamedComparator comparator) {
        return new SetPair<Integer, S>(elements, comparator, this::values);
    }

    protected <S extends SortedSet<Integer>> void testPairs(final TestPair<Integer, S> testCase) {
        this.<S>test(pair -> pair.testPairs(testCase));
    }

    static NamedComparator comparator(final String name, final Comparator<Integer> comparator) {
        final Comparator<Integer> cmp = (name.hashCode() & 1) == 0
                ? Comparator.nullsFirst(comparator)
                : Comparator.nullsLast(comparator);
        return new NamedComparator(name, cmp);
    }

    protected <S extends SortedSet<Integer>, R> void testGet(final String name, final BiFunction<S, Integer, R> method) {
        this.<S>test(pair -> pair.testGet(name, method));
    }

    protected <S extends SortedSet<Integer>, R> void testGet(final String name, final Function<S, R> method) {
        this.<S>test(pair -> pair.testGet(name, method));
    }

    /**
     * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
     */
    protected static final class NamedComparator implements Comparator<Integer> {
        private final String name;
        private final Comparator<Integer> comparator;

        private NamedComparator(final String name, final Comparator<Integer> comparator) {
            this.name = Objects.requireNonNull(name);
            this.comparator = Objects.requireNonNull(comparator);
        }

        @Override
        public int compare(final Integer o1, final Integer o2) {
            return comparator.compare(o1, o2);
        }

        @Override
        public String toString() {
            return name;
        }
    }

    protected static class SetPair<T, S extends SortedSet<T>> {
        protected final List<T> elements;
        final Comparator<T> comparator;
        final S model;
        final S tested;
        final String context;
        private final BiFunction<Comparator<? super T>, Collection<T>, Collection<T>> values;

        private SetPair(final List<T> elements, final Comparator<T> comparator, final S model, final S tested, final String context, final BiFunction<Comparator<? super T>, Collection<T>, Collection<T>> values) {
            this.elements = elements;
            this.comparator = comparator;
            this.model = model;
            this.tested = tested;
            this.context = context;
            this.values = values;
        }

        public SetPair(final List<T> elements, final Comparator<T> comparator, final BiFunction<Comparator<? super T>, Collection<T>, Collection<T>> values) {
            this(
                    elements,
                    comparator,
                    treeSet(elements, comparator),
                    set(elements, comparator),
                    String.format("[comparator = %s, elements = %s]", comparator, elements),
                    values
            );
        }

        protected <R> void testGet(final String format, final BiFunction<S, T, R> method) {
            values().forEach(element -> Assert.assertEquals(
                    String.format(format, element) + " " + context,
                    method.apply(model, element),
                    method.apply(tested, element))
            );
        }

        protected Collection<T> values() {
            return values.apply(comparator, elements);
        }

        protected <R> void testGet(final String description, final Function<S, R> method) {
            try {
                final R expected = method.apply(model);
                try {
                    Assert.assertEquals(description + context, expected, method.apply(tested));
                } catch (final RuntimeException e) {
                    Assert.fail(description + context + ": Unexpected exception " + e.getClass().getName() + ": " + e.getMessage());
                }
            } catch (final Exception ee) {
                try {
                    method.apply(tested);
                    Assert.fail(description + context + ": " + ee.getClass().getName() + " expected " + ee.getMessage());
                } catch (final Exception ae) {
                    Assert.assertSame(description, ee.getClass(), ae.getClass());
                }
            }
        }

        public <R extends SortedSet<T>> SetPair<T, R> transform(final Function<S, R> f) {
            final R model = f.apply(this.model);
            final R tested = f.apply(this.tested);
            return updated(model, tested);
        }

        private <R extends SortedSet<T>> SetPair<T, R> updated(final R model, final R tested) {
            return new SetPair<>(elements, comparator, model, tested, context, values);
        }

        public SetPair<T, S> transformModel(final Function<S, S> f) {
            return updated(model, f.apply(tested));
        }

        private void testPairs(final TestPair<T, S> testCase) {
            somePairs(fixedValues(values()), fixedValues(values()))
                    .forEach(p -> testCase.test(this, p.getFirst(), p.getSecond()));
        }
    }
}
