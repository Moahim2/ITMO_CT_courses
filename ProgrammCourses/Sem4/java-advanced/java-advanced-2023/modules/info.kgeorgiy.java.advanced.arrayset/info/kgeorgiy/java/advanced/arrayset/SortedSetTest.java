package info.kgeorgiy.java.advanced.arrayset;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static net.java.quickcheck.generator.CombinedGeneratorsIterables.someLists;
import static net.java.quickcheck.generator.PrimitiveGenerators.integers;

/**
 * Tests for easy version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-arrayset">ArraySet</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class SortedSetTest extends BaseSetTest {
    public static final int PERFORMANCE_SIZE = 500_000;
    public static final int PERFORMANCE_TIME = 10_000;

    public SortedSetTest() {
    }

    @Test
    public void test01_constructors() {
        final Class<?> token = loadClass();
        Assert.assertTrue(token.getName() + " should implement SortedSet interface", SortedSet.class.isAssignableFrom(token));

        checkConstructor("default constructor", token);
        checkConstructor("constructor out of Collection", token, Collection.class);
        checkConstructor("constructor out of Collection and Comparator", token, Collection.class, Comparator.class);
    }

    @Test
    public void test02_empty() {
        testEmpty(set());
        testEmpty(set(List.of()));
        testEmpty(set(List.of(), Comparator.<Integer>naturalOrder()));
        testEmpty(set(List.of(), null));
    }

    private static void testEmpty(final SortedSet<?> set) {
        Assert.assertEquals("Empty set size should be zero", 0, set.size());
        Assert.assertTrue("Empty set should be empty", set.isEmpty());
        Assert.assertEquals("toArray for empty set should return empty array", 0, (Object) set.toArray().length);
    }

    @Test
    public void test03_naturalOrder() {
        for (final List<Integer> elements : someLists(integers())) {
            assertEq("elements = " + elements, treeSet(elements), set(elements));
        }
    }

    @Test
    public void test04_externalOrder() {
        test(BaseSetTest::assertEq);
    }

    @Test
    public void test05_constructorPerformance() {
        performance("constructor", SortedSetTest::performanceSet);
    }

    @Test
    public void test10_contains() {
        testGet("contains(%s)", SortedSet::contains);
    }

    @Test
    public void test11_containsPerformance() {
        performance("contains", set -> {
            for (final Integer element : set) {
                Assert.assertTrue(set.contains(element));
            }
        });
    }

    @Test
    public void test12_containsAll() {
        test(pair -> Assert.assertTrue("set should contains() all elements " + " " + pair.context, pair.tested.containsAll(pair.elements)));
        testGet("containsAll(List.of(%s))", (set, element) -> set.containsAll(Arrays.asList(element, element)));
    }

    protected static void performance(final String description, final Runnable runnable) {
        runnable.run();

        final long start = System.currentTimeMillis();
        runnable.run();
        final long time = System.currentTimeMillis() - start;
        System.err.println("    " + description + " done in " + time + "ms");
        Assert.assertTrue(description + " works too slow", time < PERFORMANCE_TIME);
    }

    protected static void performance(final String description, final Consumer<SortedSet<Integer>> action) {
        performance(description, () -> action.accept(performanceSet()));
    }

    protected static SortedSet<Integer> performanceSet() {
        return set(performanceList(PERFORMANCE_SIZE));
    }

    protected static List<Integer> performanceList(final int size) {
        return new Random().ints().limit(size).boxed().distinct().collect(Collectors.toList());
    }

    @Test
    public void test21_comparator() {
        testGet("comparator()", SortedSet::comparator);
        for (final List<Integer> elements : someLists(integers())) {
            Assert.assertNull("comparator() should return null for default order", set(elements).comparator());
        }
    }

    @Test
    public void test22_headSet() {
        testGet("headSet(%s)", SortedSet::headSet);
    }

    @Test
    public void test23_tailSet() {
        testGet("tailSet(%s)", SortedSet::tailSet);
    }

    @Test
    public void test24_subSet() {
        testPairs((pair, from, to) -> pair.testGet("subSet(" + from + ", " + to + ") ", s -> s.subSet(from, to)));
    }

    @Test
    public void test25_first() {
        testGet("first()", SortedSet::first);
    }

    @Test
    public void test26_last() {
        testGet("last()", SortedSet::last);
    }

    @Test
    public void test27_iterator() {
        test(pair -> {
            testIterator(pair);
            testIterator(pair);
        });
    }

    private void testIterator(final SetPair<Integer, SortedSet<Integer>> pair) {
        final Iterator<Integer> model = pair.model.iterator();
        final Iterator<Integer> tested = pair.tested.iterator();
        while (model.hasNext()) {
            Assert.assertTrue("hasNext()", tested.hasNext());
            Assert.assertSame("next()", model.next(), tested.next());
        }
        Assert.assertFalse("hasNext()", tested.hasNext());
    }

    @Override
    protected List<Integer> values(final Comparator<? super Integer> comparator, final Collection<Integer> elements) {
        return super.values(comparator, elements.stream().flatMap(e -> e == null ? Stream.of((Integer) null) : Stream.of(e, e - 1, e + 1)).collect(Collectors.toList()));
    }
}
