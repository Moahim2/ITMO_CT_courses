package info.kgeorgiy.java.advanced.walk;

import info.kgeorgiy.java.advanced.base.BaseTest;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Tests for easy version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-walk">Walk</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class WalkTest extends BaseTest {
    private static final Path DIR = Path.of("__Test__Walk__");
    private static final List<String> ENGLISH_DIGITS = codePoints("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789");

    private static final SimpleFileVisitor<Path> DELETE = new SimpleFileVisitor<>() {
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };
    protected static final String ERROR_HASH = "0".repeat(64);
    protected static final String DIR_HASH = "dir";

    private List<String> alphabet = ENGLISH_DIGITS;
    protected final Random random = new Random(3495823423458708L);

    public WalkTest() {
    }

    private static List<String> codePoints(final String string) {
        return string.codePoints().mapToObj(Character::toString).collect(Collectors.toUnmodifiableList());
    }


    @BeforeClass
    public static void beforeClass() throws IOException {
        if (Files.exists(DIR)) {
            Files.walkFileTree(DIR, DELETE);
        }
    }

    protected Path testDir() {
        return DIR.resolve(testMethodName);
    }

    @Test
    public void test10_oneEmptyFile() throws IOException {
        testRandomFiles(1, 0);
    }

    @Test
    public void test15_tenEmptyFiles() throws IOException {
        testRandomFiles(10, 0);
    }

    @Test
    public void test20_smallRandomFiles() throws IOException {
        testRandomFiles(10, 100);
    }

    @Test
    public void test21_mediumRandomFiles() throws IOException {
        testRandomFiles(10, 10_000);
    }

    @Test
    public void test22_largeRandomFiles() throws IOException {
        testRandomFiles(10, 1_000_000);
    }

    @Test
    public void test23_veryLargeFile() throws IOException {
        testRandomFiles(1, 10_000_000);
    }

    private void testRandomFiles(final int n, final int maxSize) throws IOException {
        new TestCase().randomFiles(n, maxSize).test();
    }

    @Test
    public void test30_missingFiles() throws IOException {
        new TestCase().randomFiles(3, 10)
                .error(randomFileName())
                .error(randomFileName())
                .error(randomFileName())
                .test();
    }

    @Test
    public void test31_invalidFiles() throws IOException {
        final String upperPath = System.getProperty("os.name").toLowerCase(Locale.ENGLISH).contains("win")
                ? "//.."
                : "/../\0/...";
        new TestCase().randomFiles(3, 10)
                .error(upperPath)
                .error(upperPath + "2")
                .test();
    }

    @Test
    public void test40_errorReading() throws IOException {
        new TestCase().randomFiles(3, 10)
                .error(DIR + "..")
                .error(DIR + "@")
                .test();
    }

    @Test
    public void test45_partiallyMissingFiles() throws IOException {
        new TestCase()
                .error("no-such-file-1")
                .randomFiles(10, 100)
                .error("no-such-file-2")
                .randomFiles(10, 100)
                .error("no-such-file-3")
                .randomFiles(10, 100)
                .test();
    }

    @Test
    public void test46_filesAndDirs() throws IOException {
        new TestCase()
                .randomFiles(10, 100)
                .randomDirs(10)
                .test();
    }

    @Test
    public void test50_whitespaceSupport() throws IOException {
        testAlphabet(10, 100, " \u00a0_");
    }

    @Test
    public void test51_dirSupport() throws IOException {
        missingDirs(testMethodName + File.separator + testMethodName + ".out");
    }

    @Test
    public void test52_dirsSupport() throws IOException {
        missingDirs(testMethodName + File.separator + "111" + File.separator + testMethodName + ".out");
    }

    @Test
    public void test53_dirsHash() throws IOException {
        new TestCase().randomDirs(10).test();
    }

    private void missingDirs(final String output) throws IOException {
        final TestCase testCase = new TestCase().randomFiles(10, 100);
        testCase.test(testCase.hashes.keySet(), testCase.hashes, testMethodName + ".in", output);
    }

    @Test
    public void test55_chineseSupport() throws IOException {
        testAlphabet(10, 100, "\u8acb\u554f\u4f60\u7684\u7a0b\u5e8f\u652f\u6301\u4e2d\u570b");
    }

    @Test
    public void test56_emojiSupport() throws IOException {
        testAlphabet(10, 100, "\uD83D\uDC81\uD83D\uDC4C\uD83C\uDF8D\uD83D\uDE0D\uD83D\uDE08");
    }

    private void testAlphabet(final int n, final int maxSize, final String alphabet) throws IOException {
        this.alphabet = codePoints(alphabet);
        testRandomFiles(n, maxSize);
        this.alphabet = ENGLISH_DIGITS;
    }

    @Test
    public void test60_noInput() {
        runRaw(randomFileName(), randomFileName());
    }

    @Test
    public void test61_invalidInput() {
        runRaw("/", randomFileName());
        runRaw("\0*", randomFileName());
    }

    @Test
    public void test62_invalidOutput() throws IOException {
        final String input = createEmptyFile(testMethodName);
        runRaw(input, DIR.toString());
        runRaw(input, "\0*");
        final String file = createEmptyFile(testMethodName);
        runRaw(input, file + File.separator + randomFileName());
    }

    @Test
    public void test63_invalidFiles() throws IOException {
        testAlphabet(1, 10, "\0\\*");
    }

    @Test
    public void test70_singleArgument() throws IOException {
        runRaw(createEmptyFile(testMethodName));
    }

    @Test
    public void test71_noArguments() {
        runRaw();
    }

    @Test
    public void test72_nullArguments() {
        runRaw((String[]) null);
    }

    @Test
    public void test73_firstArgumentNull() {
        runRaw(null, "");
    }

    @Test
    public void test74_secondArgumentNull() throws IOException {
        runRaw(createEmptyFile(testMethodName), null);
    }

    @Test
    public void test75_threeArguments() throws IOException {
        runRaw(createEmptyFile("a"), createEmptyFile("b"), "c");
    }

    protected String randomDir() throws IOException {
        final Path dir = testDir();
        final Path path = dir.resolve(randomFileName());
        Files.createDirectories(path);
        return path.toString();
    }

    protected Map<String, String> randomDirs(final int n) throws IOException {
        final Map<String, String> result = new HashMap<>();
        for (int i = 0; i < n; i++) {
            result.put(randomDir(), DIR_HASH);
        }
        return result;
    }

    private static String createEmptyFile(final String name) throws IOException {
        final Path input = DIR.resolve(name);
        Files.write(input, new byte[0]);
        return input.toString();
    }

    private static void test(
            final Collection<String> inputs,
            final Map<String, String> files,
            final Path inputFile,
            final Path outputFile
    ) {
        try {
            Files.writeString(inputFile, generateInput(inputs));
        } catch (final IOException e) {
            throw new AssertionError("Cannot write input file " + inputFile);
        }
        run(inputFile, outputFile);
        try {
            for (final String line : Files.readAllLines(outputFile, StandardCharsets.UTF_8)) {
                final String[] parts = line.split(" ", 2);
                Assert.assertEquals("Invalid line format\n" + line, 2, parts.length);
                Assert.assertTrue("Unexpected file " + parts[1], files.containsKey(parts[1]));
                Assert.assertEquals("Wrong hash", files.remove(parts[1]), parts[0]);
            }
        } catch (final IOException e) {
            throw new AssertionError("Cannot read output file " + outputFile);
        }

        Assert.assertTrue("Some files missing: \n    " + String.join("\n    ", files.keySet()), files.isEmpty());
    }

    private static void run(final Path inputFile, final Path outputFile) {
        runRaw(inputFile.toString(), outputFile.toString());
    }

    private static void runRaw(final String... args) {
        final Method method;
        final Class<?> cut = loadClass();
        try {
            method = cut.getMethod("main", String[].class);
        } catch (final NoSuchMethodException e) {
            throw new AssertionError("Cannot find method main(String[]) of " + cut, e);
        }
        try {
            method.invoke(null, (Object) args);
        } catch (final IllegalAccessException e) {
            throw new AssertionError("Cannot call main(String[]) of " + cut, e);
        } catch (final InvocationTargetException e) {
            throw new AssertionError("Error thrown", e.getCause());
        }
    }

    private static String generateInput(final Collection<String> files) {
        final StringWriter stringWriter = new StringWriter();
        final PrintWriter writer = new PrintWriter(stringWriter);
        files.forEach(writer::println);
        writer.close();
        return stringWriter.toString();
    }

    protected Map<String, String> randomFiles(final int n, final int maxSize, final Path dir) throws IOException {
        Files.createDirectories(dir);
        final Map<String, String> result = new HashMap<>();
        for (int i = 0; i < n; i++) {
            final String name = randomFileName();
            try {
                final Path file = dir.resolve(name);
                final byte[] bytes = new byte[random.nextInt(maxSize + 1)];
                random.nextBytes(bytes);
                Files.write(file, bytes);
                result.put(file.toString(), hash(bytes));
            } catch (final InvalidPathException ignore) {
                result.put(dir + File.separator + name, ERROR_HASH);
            }
        }
        return result;
    }

    protected String randomFileName() {
        return random.ints(30, 0, alphabet.size())
                .mapToObj(alphabet::get)
                .collect(Collectors.joining());
    }


    @SuppressWarnings("unused")
    public static long hash(final byte[] bytes, final int size, long start) {
        for (int i = 0; i < size; i++) {
            start = (start << 8) + (bytes[i] & 0xff);
            final long high = start & 0xff00_0000_0000_0000L;
            if (high != 0) {
                start ^= high >> 48;
                start &= ~high;
            }
        }
        return start;
    }

    private static String hash(final byte[] bytes) {
        try {
            final byte[] hash = MessageDigest.getInstance("SHA-256").digest(bytes);
            return String.format("%0" + (hash.length << 1) + "x", new BigInteger(1, hash));
        } catch (final NoSuchAlgorithmException e) {
            throw new AssertionError("Digest error: " + e.getMessage(), e);
        }
    }

    protected final class TestCase {
        private final LinkedHashMap<String, String> hashes = new LinkedHashMap<>();

        TestCase() {
        }

        private TestCase error(final String path) {
            hashes.put(path, ERROR_HASH);
            return this;
        }

        void test() {
            final Map<String, String> resolved = hashes.entrySet().stream().collect(Collectors.toMap(
                    Map.Entry::getKey,
                    e -> DIR_HASH.equals(e.getValue()) ? ERROR_HASH : e.getValue()
            ));
            test(hashes.keySet(), resolved);
        }

        void testRecursive(final String... inputs) {
            final Map<String, String> resolved = hashes.entrySet().stream()
                    .filter(e -> !DIR_HASH.equals(e.getValue()))
                    .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            test(inputs.length == 0 ? hashes.keySet() : List.of(inputs), resolved);
        }

        private void test(final Collection<String> inputs, final Map<String, String> resolved) {
            test(inputs, resolved, testMethodName + ".in", testMethodName + ".out");
        }

        private void test(
                final Collection<String> inputs,
                final Map<String, String> resolved,
                final String inputFile,
                final String outputFile
        ) {
            WalkTest.test(inputs, resolved, DIR.resolve(inputFile), DIR.resolve(outputFile));
        }

        TestCase randomFiles(final int n, final int maxSize) throws IOException {
            hashes.putAll(WalkTest.this.randomFiles(n, maxSize, testDir()));
            return this;
        }

        TestCase randomDirs(final int n) throws IOException {
            hashes.putAll(WalkTest.this.randomDirs(n));
            return this;
        }

        TestCase files(final Map<String, String> files) {
            this.hashes.putAll(files);
            return this;
        }
    }
}
