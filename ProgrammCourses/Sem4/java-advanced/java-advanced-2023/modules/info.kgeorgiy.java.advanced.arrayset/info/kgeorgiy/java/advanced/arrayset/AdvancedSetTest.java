package info.kgeorgiy.java.advanced.arrayset;

import org.junit.*;
import org.junit.runners.MethodSorters;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Tests for advanced version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-arrayset">ArraySet</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class AdvancedSetTest extends NavigableSetTest {
    public static final List<NamedComparator> MORE_COMPARATORS = Arrays.asList(
            null,
            comparator("AllEqual.first", Comparator.nullsFirst(Comparator.comparingInt(v -> 0))),
            comparator("Mod1000.last", Comparator.nullsLast(Comparator.comparingInt(v -> v % 1000)))
    );

    public AdvancedSetTest() {
    }

    @Test
    public void test51_immutable() {
        final SortedSet<Integer> set = set(List.of(1));
        checkUnsupported("add", () -> set.add(1));
        checkUnsupported("addAll", () -> set.addAll(List.of(1, 2)));
        checkUnsupported("clear", set::clear);
        checkUnsupported("iterator.remove", () -> {
            final Iterator<Integer> iterator = set.iterator();
            iterator.next();
            iterator.remove();
        });
        checkUnsupported("remove", () -> set.remove(1));
        checkUnsupported("removeAll", () -> set.removeAll(List.of(1)));
        checkUnsupported("retainAll", () -> set.retainAll(List.of(0)));
    }

    private static void checkUnsupported(final String method, final Runnable command) {
        try {
            command.run();
            Assert.fail("Method '" + method + "' should throw UnsupportedOperationException");
        } catch (final UnsupportedOperationException ignore) {
        }
    }

    @Test
    public void test52_containsAllPerformance() {
        performance("containsAll", set -> Assert.assertTrue(set.containsAll(toList(set))));
    }

    @Test
    public void test53_tailSetPerformance() {
        performance("tailSet", set -> {
            for (final Integer element : set) {
                Assert.assertTrue(set.tailSet(element).contains(element));
            }
        });
    }

    @Test
    public void test54_copySource() {
        final List<Integer> data = List.of(1, 10, 50);

        @SuppressWarnings("MismatchedQueryAndUpdateOfCollection")
        final List<Integer> list = new ArrayList<>(data);

        final TreeSet<Integer> set = treeSet(data);
        final SortedSet<Integer> integers = set(data);
        assertEq("initial", integers, set);
        list.set(1, 20);
        assertEq("mutated", integers, set);
    }

    @Test
    public void test55_immutableSource() {
        final List<Integer> data = List.of(1, 100, 10);
        assertEq("initial", treeSet(data), set(data));
    }

    @Test
    public void test56_mutators() {
        final NavigableSet<Integer> set = set(List.of(1, 2, 3), Integer::compareTo);
        checkUnsupported("pollFirst", set::pollFirst);
        checkUnsupported("pollLast", set::pollLast);
    }

    @Test
    public void test57_descendingSet() {
        testDescendingSet(TEST_DESCENDING_SET_DATA, null);
    }

    @Test
    public void test58_descendingSetPerformance() {
        testDescendingSetPerformance(10, comparator("unsigned", Integer::compareUnsigned), 300);
        testDescendingSetPerformance(10, null, 300);
        testDescendingSetPerformance(PERFORMANCE_SIZE / 250, comparator("plane", Integer::compare), 2);
        testDescendingSetPerformance(PERFORMANCE_SIZE / 250, null, 2);
    }

    private void testDescendingSetPerformance(final int size, final NamedComparator comparator, final int iterations) {
        final SetPair<Integer, NavigableSet<Integer>> pair = this.<NavigableSet<Integer>>pair(performanceList(size), comparator)
                .transformModel((NavigableSet<Integer> model) -> Stream.iterate(model, NavigableSet::descendingSet)
                        .skip(PERFORMANCE_SIZE & -2).findFirst().orElseThrow());
        performance(
                "descendingSet",
                () -> {
                    for (int i = 0; i < iterations; i++) {
                        testDescendingSet(pair);
                    }
                }
        );
    }

    @Override
    protected List<Integer> values(final Comparator<? super Integer> comparator, final Collection<Integer> elements) {
        return super.values(comparator, comparator == null ? elements : Stream.concat(elements.stream(), Stream.of(null, null)).collect(Collectors.toList()));
    }

    @BeforeClass
    public static void beforeClass() {
        NAMED_COMPARATORS.addAll(MORE_COMPARATORS);
    }

    @AfterClass
    public static void afterClass() {
        NAMED_COMPARATORS.removeAll(MORE_COMPARATORS);
    }
}
