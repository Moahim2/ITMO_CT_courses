package info.kgeorgiy.java.advanced.walk;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Tests for hard version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-walk">Walk</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class RecursiveWalkTest extends WalkTest {
    public RecursiveWalkTest() {
    }

    @Test @Override
    public void test46_filesAndDirs() throws IOException {
        new TestCase()
                .randomFiles(10, 10)
                .randomDirs(10)
                .testRecursive();
    }

    @Test
    @Override
    public void test53_dirsHash() throws IOException {
        new TestCase().randomDirs(10).testRecursive();
        new TestCase().testRecursive(testDir().toString());
    }

    @Test
    public void test70_singleRecursion() throws IOException {
        final Path root = testDir();
        new TestCase().files(randomDirs(3, 4, 100, root)).testRecursive(root.toString());
    }

    @Test
    public void test80_doubleRecursion() throws IOException {
        final Path root = testDir();
        final Path dir1 = root.resolve(randomFileName());
        final Path dir2 = root.resolve(randomFileName());
        final String from = dir1.toString();
        final String to = dir2.resolve("..").resolve(dir1.getFileName()).toString();

        final Map<String, String> files = randomDirs(3, 4, 100, dir1);
        new TestCase()
                .files(files)
                .files(files.entrySet().stream().collect(Collectors.toMap(e -> e.getKey().replace(from, to), Map.Entry::getValue)))
                .files(randomDirs(3, 4, 100, dir2))
                .testRecursive(from, dir2.toString(), to);
    }

    private Map<String, String> randomDirs(final int n, final int d, final int maxL, final Path dir) throws IOException {
        final Map<String, String> result = randomFiles(random.nextInt(n + 1), maxL, dir);
        if (d > 0) {
            for (int i = random.nextInt(n + 1); i < n; i++) {
                result.putAll(randomDirs(n, d - 1, maxL, dir.resolve(randomFileName())));
            }
        }
        return result;
    }
}
