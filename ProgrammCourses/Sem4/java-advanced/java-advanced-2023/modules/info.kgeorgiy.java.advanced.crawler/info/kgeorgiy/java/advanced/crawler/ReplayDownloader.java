package info.kgeorgiy.java.advanced.crawler;

import java.io.*;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class ReplayDownloader implements Downloader {
    protected final ConcurrentMap<String, Page> pages;
    private final ConcurrentMap<String, Boolean> downloaded = new ConcurrentHashMap<>();
    private final AtomicInteger errors = new AtomicInteger();
    private final int downloadDelay;
    private final int extractDelay;

    public ReplayDownloader(final String url, final int downloadDelay, final int extractDelay) throws IOException {
        pages = load(getFileName(url));
        this.downloadDelay = downloadDelay;
        this.extractDelay = extractDelay;
    }

    Page getPage(final String url) {
        return pages.computeIfAbsent(url, u -> error(1, new IOException("Unknown page " + u)));
    }

    public static String getFileName(final String url) throws MalformedURLException {
        return URLUtils.getHost(url) + ".ser.gz";
    }

    @SuppressWarnings("unchecked")
    private static ConcurrentMap<String, Page> load(final String fileName) throws IOException {
        final InputStream stream = ReplayDownloader.class.getResourceAsStream("sites/" + fileName);
        if (stream == null) {
            throw new AssertionError("Cache file " + fileName + " not found");
        }
        try (final ObjectInput os = new ObjectInputStream(new GZIPInputStream(stream))) {
            try {
                return (ConcurrentMap<String, Page>) os.readObject();
            } catch (final ClassNotFoundException e) {
                throw new AssertionError(e);
            }
        }
    }

    @Override
    public Document download(final String url) throws IOException {
        final Page page = getPage(url);
        if (downloaded.putIfAbsent(url, true) != null) {
            throw new AssertionError("Duplicate download of " + url);
        }
        if (downloaded.size() % 100 == 0) {
            System.out.format("    %d of %d pages downloaded, %d error(s)%n", downloaded.size(), pages.size(), errors.get());
        }
        return page.document(downloadDelay, extractDelay, errors);
    }

    private static void sleepUpTo(final int max) {
        if (max > 0) {
            sleep(ThreadLocalRandom.current().nextInt(max) + 1);
        }
    }

    private static void sleep(final long millis) {
        try {
            Thread.sleep(millis);
        } catch (final InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    public Result expected(final String url, final int depth) {
        return expected(getPages(url, depth));
    }

    protected static Result expected(final Map<String, Page> pages) {
        final Map<Boolean, List<Map.Entry<String, Page>>> results = pages.entrySet().stream()
                .collect(Collectors.partitioningBy(e -> e.getValue().exception == null));
        return new Result(
                results.get(true).stream().map(Map.Entry::getKey).collect(Collectors.toUnmodifiableList()),
                results.get(false).stream().collect(Collectors.toMap(Map.Entry::getKey, e -> e.getValue().exception))
        );
    }

    private Map<String, Page> getPages(final String url, final int depth) {
        final Map<String, Page> level = new HashMap<>(Map.of(url, getPage(url)));
        for (int i = 1; i < depth; i++) {
            final Map<String, Page> next = new HashMap<>();
            level.values().stream()
                    .map(p -> p.links)
                    .filter(Objects::nonNull)
                    .flatMap(Collection::stream)
                    .forEach(link -> next.put(link, getPage(link)));
            level.putAll(next);
        }
        return level;
    }

    public static Page success(final long loadTime, final List<String> links) {
        return new Page(loadTime, links, null);
    }

    public static Page error(final long loadTime, final IOException exception) {
        return new Page(loadTime, null, exception);
    }

    public static final class Page implements Serializable {
        private static final long serialVersionUID = -6132283310711004635L;

        private final long loadTime;
        private List<String> links;
        private final IOException exception;

        private Page(final long loadTime, final List<String> links, final IOException exception) {
            this.loadTime = loadTime;
            this.links = links;
            this.exception = exception;
        }

        @Override
        public String toString() {
            return "Page(" + (links == null ? "" : links.size()) + " " + exception + ")";
        }

        private Document document(
                final int downloadDelay,
                final int extractDelay,
                final AtomicInteger errors
        ) throws IOException {
            if (downloadDelay >= 0) {
                sleepUpTo(downloadDelay);
            } else {
                sleep(loadTime * downloadDelay / -100);
            }
            if (exception != null) {
                errors.incrementAndGet();
                throw exception;
            }
            return () -> {
                sleepUpTo(extractDelay);
                return links;
            };
        }

        public void filterLinks(final Predicate<String> filter) {
            if (links != null) {
                links = links.stream().filter(filter).collect(Collectors.toUnmodifiableList());
            }
        }

        public List<String> links() {
            return links == null ? List.of() : links;
        }
    }
}
