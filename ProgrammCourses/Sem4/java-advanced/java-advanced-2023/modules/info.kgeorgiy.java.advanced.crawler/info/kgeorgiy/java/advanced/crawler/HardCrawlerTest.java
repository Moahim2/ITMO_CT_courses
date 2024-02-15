package info.kgeorgiy.java.advanced.crawler;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.IOException;

/**
 * Full tests for hard version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#crawler">Web Crawler</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class HardCrawlerTest extends EasyCrawlerTest {
    public HardCrawlerTest() {
    }

    @Test
    public void test10_singleConnectionPerHost() throws IOException {
        test("https://itmo.ru", 2, UNLIMITED, UNLIMITED, 1, 10, 10);
    }

    @Test
    public void test11_limitedConnectionsPerHost() throws IOException {
        test("https://itmo.ru", 2, UNLIMITED, UNLIMITED, 10, 10, 10);
    }

    @Test
    public void test12_limitedConnectionsPerformance() throws IOException {
        testPerformance(3, 7700);
        testPerformance(10, 2760);
    }

    private static void testPerformance(final int perHost, final double target) throws IOException {
        checkTime(target, test("https://itmo.ru", 3, UNLIMITED, UNLIMITED, perHost, 100, 50));
    }
}
