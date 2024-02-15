package info.kgeorgiy.java.advanced.crawler;

import info.kgeorgiy.java.advanced.base.BaseTest;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * Full tests for easy version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#crawler">Web Crawler</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class EasyCrawlerTest extends BaseTest {

    public static final int UNLIMITED = 100;

    public EasyCrawlerTest() {
    }

    @Test
    public void test01_singlePage() throws IOException {
        test("https://en.itmo.ru/en/page/50/Partnership.htm", 1);
        test("https://bars.itmo.ru", 1);
    }

    @Test
    public void test02_pageAndLinks() throws IOException {
        test("https://itmo.ru", 2);
        test("https://www.itmo.ru", 3);
    }

    @Test
    public void test03_invalid() throws IOException {
        test("https://itmo.ru/ru/educational-activity/voprosi_predlojeniya.htmvoprosy_i_predlozheniya.htmvoprosy_i_predlozheniya.htm", 1);
    }

    @Test
    public void test04_deep() throws IOException {
        for (int i = 1; i <= 5; i++) {
            test("http://www.kgeorgiy.info", i);
        }
    }

    @Test
    public void test05_noLimits() throws IOException {
        test(UNLIMITED, UNLIMITED, 10, 10);
    }

    @Test
    public void test06_limitDownloads() throws IOException {
        test(10, UNLIMITED, 100, 10);
    }

    @Test
    public void test07_limitExtractors() throws IOException {
        test(UNLIMITED, 10, 10, 100);
    }

    @Test
    public void test08_limitBoth() throws IOException {
        test(10, 10, 100, 100);
    }

    @Test
    public void test09_performance() throws IOException {
        checkTime(6500, test(UNLIMITED, UNLIMITED, 1000, 1000));
    }

    @Test
    public void test10_realTimePerformance() throws IOException {
        checkTime(1800, test(UNLIMITED, UNLIMITED, -30, 100));
    }

    protected static void checkTime(final double target, final long time) {
        System.err.println("Time: " + time);
        Assert.assertTrue("Too parallel: " + time, time > 0.8 * target);
        Assert.assertTrue("Not parallel: " + time, time < 1.2 * target);
    }

    private static void test(final String url, final int depth) throws IOException {
        test(url, depth, UNLIMITED, UNLIMITED, UNLIMITED, 10, 10);
    }

    protected static long test(final int downloaders, final int extractors, final int downloadTimeout, final int extractTimeout) throws IOException {
        return test("http://nerc.itmo.ru/subregions/index.html", 3, downloaders, extractors,
                UNLIMITED, downloadTimeout, extractTimeout);
    }

    protected static long test(final String url, final int depth, final int downloaders, final int extractors, final int perHost, final int downloadTimeout, final int extractTimeout) throws IOException {
        final ReplayDownloader replayDownloader = new ReplayDownloader(url, downloadTimeout, extractTimeout);

        final long start = System.currentTimeMillis();
        final Result actual = download(url, depth, replayDownloader, downloaders, extractors, perHost);
        final long time = System.currentTimeMillis() - start;

        final Result expected = replayDownloader.expected(url, depth);
        checkResult(expected, actual);
        return time;
    }

    public static void checkResult(final Result expected, final Result actual) {
        compare("Downloaded OK", Set.copyOf(expected.getDownloaded()), Set.copyOf(actual.getDownloaded()));
        compare("Downloaded with errors", expected.getErrors().keySet(), actual.getErrors().keySet());
        Assert.assertEquals("Errors", expected.getErrors(), actual.getErrors());
    }

    private static void compare(final String context, final Set<String> expected, final Set<String> actual) {
        final Set<String> missing = difference(expected, actual);
        final Set<String> excess = difference(actual, expected);
        final String message = String.format("%s:%n    missing = %s%n    excess = %s%n", context, missing, excess);
        Assert.assertTrue(message, missing.isEmpty() && excess.isEmpty());
    }

    private static Result download(final String url, final int depth, final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        final CheckingDownloader checkingDownloader = new CheckingDownloader(downloader, downloaders, extractors, perHost);
        try (final Crawler crawler = createInstance(checkingDownloader, downloaders, extractors, perHost)) {
            final Result result = crawler.download(url, depth);
            Assert.assertNull(checkingDownloader.getError(), checkingDownloader.getError());
            return result;
        }
    }

    private static Crawler createInstance(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        return createInstance(downloader, downloaders, extractors, perHost, Crawler.class);
    }

    protected static <C extends Crawler> C createInstance(final Downloader downloader, final int downloaders, final int extractors, final int perHost, final Class<C> type) {
        try {
            final Constructor<?> constructor = loadClass().getConstructor(Downloader.class, int.class, int.class, int.class);
            return type.cast(constructor.newInstance(downloader, downloaders, extractors, perHost));
        } catch (final Exception e) {
            throw new AssertionError(e);
        }
    }

    private static Set<String> difference(final Set<String> a, final Set<String> b) {
        return a.stream().filter(Predicate.not(b::contains)).collect(Collectors.toSet());
    }
}
