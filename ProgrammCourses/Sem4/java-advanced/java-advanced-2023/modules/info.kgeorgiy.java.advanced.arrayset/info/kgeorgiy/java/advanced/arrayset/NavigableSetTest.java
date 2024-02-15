package info.kgeorgiy.java.advanced.arrayset;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.Comparator;
import java.util.List;
import java.util.NavigableSet;
import java.util.SortedSet;
import java.util.function.BiFunction;
import java.util.stream.IntStream;

/**
 * Tests for hard version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-arrayset">ArraySet</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class NavigableSetTest extends SortedSetTest {
    public static final List<Integer> TEST_DESCENDING_SET_DATA = List.of(10, 20, 30, 40);

    public NavigableSetTest() {
    }

    @Test
    public void test31_lower() {
        testGetN("lower(%s)", NavigableSet::lower);
    }

    @Test
    public void test32_ceiling() {
        testGetN("ceiling(%s)", NavigableSet::ceiling);
    }

    @Test
    public void test33_higher() {
        testGetN("higher(%s)", NavigableSet::higher);
    }

    @Test
    public void test34_floor() {
        testGetN("floor(%s)", NavigableSet::floor);
    }

    @Test
    public void test35_navigableTailSet() {
        testGetN("tailSet(%s, true)", (s, e) -> s.tailSet(e, true));
        testGetN("tailSet(%s, false)", (s, e) -> s.tailSet(e, false));
    }

    @Test
    public void test36_navigableHeadSet() {
        testGetN("headSet(%s, true)", (s, e) -> s.headSet(e, true));
        testGetN("headSet(%s, false)", (s, e) -> s.headSet(e, false));
    }

    @Test
    public void test37_navigableSubSet() {
        testPairsN((pair, from, to) -> IntStream.range(0, 4).forEach(i -> pair.testGet(
                String.format("subSet(%d, %b, %d, %b)", from, i % 2 == 1, to, i / 2 == 1),
                set -> set.subSet(from, i % 2 == 1, to, i / 2 == 1)
        )));
    }

    @Test
    public void test38_descendingSet() {
        testDescendingSet(TEST_DESCENDING_SET_DATA, comparator("unsigned", Integer::compareUnsigned));
    }

    protected void testDescendingSet(final List<Integer> data, final Comparator<Integer> comparator) {
        testDescendingSet(new SetPair<Integer, NavigableSet<Integer>>(data, comparator, this::values));
    }

    protected static void testDescendingSet(final SetPair<Integer, NavigableSet<Integer>> basePair) {
        final SetPair<Integer, NavigableSet<Integer>> pair = basePair.transform(NavigableSet::descendingSet);

        pair.testGet("toArray()", NavigableSetTest::toArray);
        pair.testGet("size()", NavigableSet::size);
        pair.testGet("first()", SortedSet::first);
        pair.testGet("last()", SortedSet::last);
        pair.testGet("descendingIterator().next()", s -> s.descendingIterator().next());

        pair.testGet("floor(%s)", NavigableSet::floor);
        pair.testGet("lower(%s)", NavigableSet::lower);
        pair.testGet("ceiling(%s)", NavigableSet::ceiling);
        pair.testGet("higher(%s)", NavigableSet::higher);

        pair.testGet("headSet(%s).size()", (s, e) -> s.headSet(e).size());
        pair.testGet("tailSet(%s).size()", (s, e) -> s.tailSet(e).size());

        pair.testGet("descendingSet().toArray()", s -> toArray(s.descendingSet()));
    }

    private void testPairsN(final TestPair<Integer, NavigableSet<Integer>> testCase) {
        testPairs(testCase);
    }

    protected <R> void testGetN(final String name, final BiFunction<NavigableSet<Integer>, Integer, R> testCase) {
        testGet(name, testCase);
    }
}
