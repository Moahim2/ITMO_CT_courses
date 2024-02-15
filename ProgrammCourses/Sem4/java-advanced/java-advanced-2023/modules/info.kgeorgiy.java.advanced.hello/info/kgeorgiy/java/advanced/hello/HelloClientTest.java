package info.kgeorgiy.java.advanced.hello;

import info.kgeorgiy.java.advanced.base.BaseTest;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.concurrent.*;

/**
 * Full tests for {@link HelloClient}.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class HelloClientTest extends BaseTest {
    private static final int PORT = 28888;
    public static final String PREFIX = HelloClientTest.class.getName();
    public static final int SOCKET_FREE_TIME = 500;

    public HelloClientTest() {
    }

    @Test
    public void test01_singleRequest() throws SocketException {
        test(1, 1, 1);
    }

    @Test
    public void test02_sequence() throws SocketException {
        test(100, 1, 1);
    }

    @Test
    public void test03_singleWithFailures() throws SocketException {
        test(1, 1, 0.1);
    }

    @Test
    public void test04_sequenceWithFailures() throws SocketException {
        test(20, 1, 0.5);
    }

    @Test
    public void test05_singleMultithreaded() throws SocketException {
        test(1, 10, 1);
    }

    @Test
    public void test06_sequenceMultithreaded() throws SocketException {
        test(10, 10, 1);
    }

    @Test
    public void test07_sequenceMultithreadedWithFails() throws SocketException {
        test(10, 10, 0.5);
    }

    @SuppressWarnings("try")
    private void test(final int requests, final int threads, final double p) throws SocketException {
        try (final DatagramSocket socket = new DatagramSocket(PORT)) {
            final ExecutorService executor = Executors.newFixedThreadPool(2);
            try {
                final CompletionService<int[]> completionService = new ExecutorCompletionService<>(executor);
                final String prefix = PREFIX + "_" + testMethodName.replaceAll("[0-9]+", "") + "_";
                completionService.submit(server(prefix, threads, p, socket));
                completionService.submit(() -> {
                    final HelloClient client = createCUT();
                    client.run("localhost", PORT, prefix, threads, requests);
                    return null;
                });
                for (int i = 0; i < 2; i++) {
                    try {
                        final int[] actual = completionService.take().get();
                        if (actual != null) {
                            for (int j = 0; j < actual.length; j++) {
                                Assert.assertEquals("Invalid number of requests on thread " + j , requests, actual[j]);
                            }
                        } else {
                            socket.close();
                        }
                    } catch (final ExecutionException e) {
                        final Throwable cause = e.getCause();
                        if (cause instanceof RuntimeException) {
                            throw (RuntimeException) cause;
                        } else if (cause instanceof Error) {
                            throw (Error) cause;
                        } else {
                            throw new AssertionError("Unexpected exception", e.getCause());
                        }
                    }
                }
                Thread.sleep(SOCKET_FREE_TIME);
            } finally {
                executor.shutdownNow();
            }
        } catch (final InterruptedException e) {
            throw new AssertionError("Test thread interrupted", e);
        }
    }

    protected Callable<int[]> server(
            final String prefix,
            final int threads,
            final double p,
            final DatagramSocket socket
    ) {
        return Util.server(prefix, threads, p, socket);
    }
}
