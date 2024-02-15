package info.kgeorgiy.java.advanced.student;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.IntFunction;

/**
 * Tests for advanced version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-student">Student</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class AdvancedQueryTest extends GroupQueryTest implements AdvancedQuery {
    private static final Map<Integer, Student> IDS = new HashMap<>();
    static {
        for (final Student student : STUDENTS) {
            IDS.put(student.getId(), student);
        }
    }

    private final AdvancedQuery db = createCUT();

    public AdvancedQueryTest() {
    }

    @Test
    public void test31_testGetMostPopularName() {
        test(this::getMostPopularName, db::getMostPopularName);
    }

    @Test
    public void test32_testGetFirstNames() {
        testGet(this::getFirstNames, db::getFirstNames);
    }

    @Test
    public void test33_testGetLastNames() {
        testGet(this::getLastNames, db::getLastNames);
    }

    @Test
    public void test34_testGetGroups() {
        testGet(this::getGroups, db::getGroups);
    }

    @Test
    public void test35_testGetFullNames() {
        testGet(this::getFullNames, db::getFullNames);
    }

    private static <T> void testGet(
            final BiFunction<Collection<Student>, int[], List<T>> reference,
            final BiFunction<Collection<Student>, int[], List<T>> tested
    ) {
        testGet(reference, tested, n -> {
            final List<Student> students = new ArrayList<>(STUDENTS).subList(0, n);
            Collections.shuffle(students, RANDOM);
            return students.stream().mapToInt(Student::getId).toArray();
        });
    }

    private static <T> void testGet(
            final BiFunction<Collection<Student>, int[], List<T>> reference,
            final BiFunction<Collection<Student>, int[], List<T>> tested,
            final IntFunction<int[]> argsF
    ) {
        for (int i = 0; i < STUDENTS.size(); i++) {
            final int[] args = argsF.apply(i);
            Assert.assertEquals(reference.apply(STUDENTS, args), tested.apply(STUDENTS, args));
        }
    }

    // Reference implementation follows
    // This implementation is intentionally poorly-written and contains a lot of copy-and-paste

    @Override
    public String getMostPopularName(final Collection<Student> students) {
        final NavigableMap<String, Integer> counts = new TreeMap<>();
        for (final Group group : getGroupsByName(students)) {
            for (final String name : Set.copyOf(getFirstNames(group.getStudents()))) {
                counts.merge(name, 1, Integer::sum);
            }
        }

        if (counts.isEmpty()) {
            return "";
        }

        final int max = Collections.max(counts.values());
        for (final Map.Entry<String, Integer> entry : counts.entrySet()) {
            if (entry.getValue() == max) {
                return entry.getKey();
            }
        }
        return "";
    }

    @Override
    public List<String> getFirstNames(final Collection<Student> students, final int[] ids) {
        final List<String> result = new ArrayList<>();
        for (final int id : ids) {
            result.add(IDS.get(id).getFirstName());
        }
        return result;
    }

    @Override
    public List<String> getLastNames(final Collection<Student> students, final int[] ids) {
        final List<String> result = new ArrayList<>();
        for (final int id : ids) {
            result.add(IDS.get(id).getLastName());
        }
        return result;
    }

    @Override
    public List<GroupName> getGroups(final Collection<Student> students, final int[] ids) {
        final List<GroupName> result = new ArrayList<>();
        for (final int id : ids) {
            result.add(IDS.get(id).getGroup());
        }
        return result;
    }

    @Override
    public List<String> getFullNames(final Collection<Student> students, final int[] ids) {
        final List<String> result = new ArrayList<>();
        for (final int id : ids) {
            result.add(getFullName(IDS.get(id)));
        }
        return result;
    }
}
