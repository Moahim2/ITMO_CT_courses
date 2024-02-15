package info.kgeorgiy.java.advanced.crawler;

import info.kgeorgiy.java.advanced.base.BaseTester;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class Tester {
    private Tester() {
    }

    public static void main(final String... args) {
        new BaseTester()
                .add("easy", EasyCrawlerTest.class)
                .add("hard", HardCrawlerTest.class)
                .add("advanced", AdvancedCrawlerTest.class)
                .run(args);
    }

    static {
        //noinspection ConstantConditions
        assert Downloader.class.isAssignableFrom(CachingDownloader.class);
    }
}
