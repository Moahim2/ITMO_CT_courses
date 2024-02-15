package info.kgeorgiy.java.advanced.crawler;

import info.kgeorgiy.java.advanced.base.BaseTest;

import java.io.*;
import java.net.URI;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Downloads document from the Web and stores them in storage directory.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class CachingDownloader implements Downloader {
    private static final byte[] OK_MARKER = {'+'};
    private static final byte[] FAIL_MARKER = {'-'};
    private static final int READ_TIMEOUT = 30_000;

    private final double timeScale;
    private final Path directory;

    /**
     * Creates a new downloader storing documents in temporary directory.
     *
     * @param timeScale loading times simulation scale.
     * @throws IOException if an error occurred.
     */
    public CachingDownloader(final double timeScale) throws IOException {
        this(timeScale, Files.createTempDirectory(CachingDownloader.class.getName()));
    }

    /**
     * Creates a new downloader storing documents in specified directory.
     *
     * @param timeScale loading times simulation scale.
     * @param directory storage directory.
     *
     * @throws IOException if an error occurred.
     */
    public CachingDownloader(final double timeScale, final Path directory) throws IOException {
        this.timeScale = timeScale;
        this.directory = directory;
        if (!Files.exists(directory)) {
            Files.createDirectories(directory);
        }
        if (!Files.isDirectory(directory)) {
            throw new IOException(directory + " is not a directory");
        }
    }

    private static ByteArrayInputStream writeData(
            final BaseTest.ConsumerCommand<DataOutputStream, IOException> consumer
    ) throws IOException {
        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        consumer.run(new DataOutputStream(out));
        return new ByteArrayInputStream(out.toByteArray());
    }

    /**
     * Downloads document and stores it in the storage directory. An error during
     * download may result in incomplete files in storage directory.
     *
     * @param url URL of the document to download.
     *
     * @return downloaded document.
     *
     * @throws IOException if an error occurred.
     */
    @Override
    public Document download(final String url) throws IOException {
        final URI uri = URLUtils.getURI(url);
        final Path file = directory.resolve(URLEncoder.encode(uri.toString(), StandardCharsets.UTF_8).replace('*', '_'));
        final long start = System.currentTimeMillis();
        if (Files.notExists(file)) {
            System.out.println("Downloading " + url);
            try {
                final URLConnection connection = uri.toURL().openConnection();
                connection.setReadTimeout(READ_TIMEOUT);
                try (final InputStream data = connection.getInputStream()) {
                    final ByteArrayInputStream header = writeData(out -> writeHeader(out, OK_MARKER, start));
                    Files.copy(new SequenceInputStream(header, data), file);
                }
            } catch (final IOException e) {
                final ByteArrayInputStream in = writeData(out -> {
                    writeHeader(out, FAIL_MARKER, start);
                    try (final ObjectOutputStream oos = new ObjectOutputStream(out)) {
                        oos.writeObject(e);
                    }
                });
                Files.copy(in, file);
                throw e;
            }
            System.out.println("Downloaded " + uri);
        } else {
            System.out.println("Already downloaded " + url);
            try (final DataInputStream is = new DataInputStream(Files.newInputStream(file))) {
                if (readHeader(is, timeScale)) {
                    try (final ObjectInputStream ois = new ObjectInputStream(is)) {
                        throw (IOException) ois.readObject();
                    } catch (final ClassNotFoundException e) {
                        throw new AssertionError(e);
                    }
                }
            }
        }
        return () -> {
            try (final DataInputStream is = new DataInputStream(Files.newInputStream(file))) {
                return readHeader(is, 0) ? List.of() : URLUtils.extractLinks(uri, is);
            }
        };
    }

    private static boolean readHeader(final DataInputStream is, final double timeScale) throws IOException {
        final boolean fail = is.read() == FAIL_MARKER[0];
        final long loadTime = (long) (is.readLong() * timeScale);
        if (loadTime > 0) {
            try {
                Thread.sleep(loadTime);
            } catch (final InterruptedException e) {
                throw new AssertionError("Thread interrupted", e);
            }
        }
        return fail;
    }

    private static void writeHeader(
            final DataOutputStream out, final byte[] marker, final long start
    ) throws IOException {
        out.write(marker);
        out.writeLong(System.currentTimeMillis() - start);
    }
}
