package info.kgeorgiy.ja.levitskii.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;


/**
 * WebCrawler class implements {@link Crawler} interface.
 * This class is designed for parallel crawling of sites to the desired depth.
 * This implementation ignores the perHost argument.
 * Contains {@link #main(String[])} and therefore can be run from the command line.
 *
 * @author Ivanl
 */
public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final ExecutorService downloadExecutor;
    private final ExecutorService parseExecutor;

    private final static int DEPTH = 1;
    private final static int DOWNLOADERS = 10;
    private final static int EXTRACTORS = 10;

    private final static double TIME_SCALE = 1.0; //const for cashingDownloader, because it hasn't got empty constructor.


    /**
     * Method for a place to run crawling of sites from the command line.
     * Accepts arguments and expects them in the format {@code url [depth [downloads [extractors [perHost]]]]}.
     * In the absence of any arguments other than the url, it substitutes default values in them:
     * <ul>
     *     <li>
     *         DEPTH = {@value DEPTH}
     *     </li>
     *     <li>
     *         DOWNLOADERS = {@value DOWNLOADERS}
     *     </li>
     *     <li>
     *         EXTRACTORS = {@value EXTRACTORS}
     *     </li>
     * </ul>
     *
     * @param args (usually) array of cmd arguments.
     */
    public static void main(String[] args) {
        if (args == null) {
            System.out.println("Array of args is null.");
            return;
        }

        if (Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.out.println("Array of args contains null");
            return;
        }

        if (args.length == 0 || args.length > 5) {
            System.out.println("Array of args of unacceptable size (must be 0 < size <= 5).");
            return;
        }

        final List<Integer> integerArgs;
        try {
            integerArgs = Arrays.stream(args).skip(1).map(Integer::parseInt).toList();
        } catch (NumberFormatException e) {
            System.out.println("Array of args must contain only numbers after first argument");
            return;
        }

        final int depth = getArgOrDefaultValue(integerArgs, 0, DEPTH);
        final int downloaders = getArgOrDefaultValue(integerArgs, 1, DOWNLOADERS);
        final int extractors = getArgOrDefaultValue(integerArgs, 2, EXTRACTORS);

        try {
            final Downloader dl = new CachingDownloader(TIME_SCALE);
            try (final WebCrawler webCrawler = new WebCrawler(dl, downloaders, extractors, -1)) {
                webCrawler.download(args[0], depth);

                //some actions with the Result
                //...
                //
            } catch (IllegalArgumentException e) {
                System.out.println("Args (downloaders and extractors) must be > 0");
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }

    private static int getArgOrDefaultValue(List<Integer> args, int i, int defaultValue) {
        return args.size() > i ? args.get(i) : defaultValue;
    }


    /**
     * Creates a new Crawler from various variable arguments.
     *
     * @param downloader     website loader.
     * @param downloaders    max count of threads simultaneously loading sites.
     * @param extractors     max count of threads simultaneously extracting links of sites.
     * @param ignoredPerHost ignored parameter.
     * @throws NullPointerException if {@code downloader = null}.
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int ignoredPerHost) {
        if (downloader == null) {
            throw new NullPointerException("Downloader must be not null.");
        }

        this.downloader = downloader;
        this.downloadExecutor = Executors.newFixedThreadPool(downloaders);
        this.parseExecutor = Executors.newFixedThreadPool(extractors);
    }


    private void downloadLinkAndReadChildren(String link, boolean readChildren, Set<String> downloaded,
                                             Map<String, IOException> errors, Set<String> nextLevelLinks,
                                             Phaser phaser) {
        phaser.register();
        downloadExecutor.execute(() -> {
            try {
                final Document document = downloader.download(link);
                downloaded.add(link);

                if (readChildren) {
                    phaser.register();
                    parseExecutor.execute(() -> {
                        try {
                            nextLevelLinks.addAll(document.extractLinks());
                        } catch (IOException ignored) {
                        }
                        phaser.arriveAndDeregister();
                    });
                }
            } catch (IOException e) {
                errors.put(link, e);
            }
            phaser.arriveAndDeregister();
        });
    }

    @Override
    public Result download(String url, int depth) {
        final Set<String> downloaded = ConcurrentHashMap.newKeySet();
        final Map<String, IOException> errors = new ConcurrentHashMap<>();


        final List<String> curLevelLinks = new ArrayList<>();
        final Set<String> nextLevelLinks = ConcurrentHashMap.newKeySet();
        curLevelLinks.add(url);

        final Phaser phaser = new Phaser();

        phaser.register();
        for (int i = depth; i > 0; i--) {
            final boolean flagReadChildren = i > 1;
            curLevelLinks.stream()
                    .filter(link -> !downloaded.contains(link) && !errors.containsKey(link))
                    .forEach(link ->
                            downloadLinkAndReadChildren(link, flagReadChildren, downloaded, errors, nextLevelLinks, phaser));
            phaser.arriveAndAwaitAdvance();

            curLevelLinks.clear();
            curLevelLinks.addAll(nextLevelLinks);
            nextLevelLinks.clear();
        }

        return new Result(new ArrayList<>(downloaded), errors);
    }


    /**
     * {@inheritDoc}
     * Interrupts the execution of all tasks that have not been started
     * and waits for the end of all tasks that have been started,
     * after which it releases resources.
     */
    @Override
    public void close() {
        parseExecutor.shutdownNow();
        downloadExecutor.shutdownNow();
        //java19
        downloadExecutor.close();
        parseExecutor.close();
    }

}
