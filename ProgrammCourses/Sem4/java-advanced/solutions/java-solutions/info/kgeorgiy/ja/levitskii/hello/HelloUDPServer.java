package info.kgeorgiy.ja.levitskii.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.util.Scanner;
import java.util.concurrent.*;

import static info.kgeorgiy.ja.levitskii.hello.HelloUtil.*;


/**
 * This class is implementation of the server interface {@link HelloServer}.
 * Accept tasks sent by the Hello UdpClient class and respond to them.
 * Contains {@link #main(String[])} and therefore can be run from the command line.
 *
 * @author Ivanl
 */
public class HelloUDPServer implements HelloServer {
    private ExecutorService receiveExecutorService = null;
    private ExecutorService handlerExecutorService = null;
    private DatagramSocket socket = null;


    /**
     * Maximum of count simultaneous requests ({@value}).
     */
    public static final int MAX_REQUESTS = 1000000;


    /**
     * Method for a place to run server from the command line.
     * Accepts arguments and expects them only in the format {@code port threads}.
     * When:
     * <ul>
     *     <li>
     *         port - the number of the port on which requests will be received;
     *     </li>
     *     <li>
     *         threads - the number of worker threads that will process requests.
     *     </li>
     * </ul>
     *
     * @param args (usually) array of cmd arguments.
     */
    public static void main(String[] args) {
        if (checkArgs(args, 2)) {
            return;
        }

        try (final HelloUDPServer server = new HelloUDPServer();
             final Scanner scanner = new Scanner(System.in, CHARSET)
        ) {
            server.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
            while (scanner.hasNext()) {
                scanner.next();
            }
        } catch (NumberFormatException e) {
            System.err.println(e.getMessage());
        }
    }


    /**
     * {@inheritDoc}
     * To a request in any format {@code "string"} returns it in the format {@code "Hello, string"}.
     * Server will be working until method {@link #close()} isn't call.
     *
     * @param port server port.
     * @param threads number of server working threads.
     */
    @Override
    public void start(int port, int threads) {
        if (receiveExecutorService != null) {
            System.err.println("Server has already been started and isn't closed.");
            return;
        }

        final DatagramPacket receivePacket;
        try {
            socket = new DatagramSocket(port);
            receivePacket = createPacket(getValidReceiveBuffer(socket));
        } catch (SocketException e) {
            System.err.println("Server cannot run on this port: " + e.getMessage());
            return;
        }

        receiveExecutorService = Executors.newSingleThreadExecutor();

        // можно использовать только один executor pool
        handlerExecutorService = new ThreadPoolExecutor(
                threads, threads, 0, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(MAX_REQUESTS), Executors.defaultThreadFactory(),
                new ThreadPoolExecutor.CallerRunsPolicy() //DiscardPolicy
        );

        receiveExecutorService.execute(() -> {
            while (!socket.isClosed()) {
                try {
                    socket.receive(receivePacket);

                    final String receiveString = getStringFromDatagramPacket(receivePacket);

                    final SocketAddress reverseAddress = receivePacket.getSocketAddress();

                    handlerExecutorService.execute(() -> {
                        final byte[] response = createResponse(receiveString).getBytes(CHARSET);

                        try {
                            sendData(socket, response, reverseAddress);
                        } catch (IOException e) {
                            logServerMessagesExceptions("Error when sending response", e);
                        }
                    });
                } catch (IOException e) {
                    logServerMessagesExceptions("Error when receiving request:", e);
                }
            }
        });
    }


    private void logServerMessagesExceptions(String metaInformation, IOException e) {
        //anything
        //
        //for example:
        if (!socket.isClosed()) {
            System.err.println(metaInformation + " " + e.getMessage());
        }
    }

    @Override
    public void close() {
        if (receiveExecutorService != null) {
            socket.close();

            receiveExecutorService.shutdownNow();
            receiveExecutorService.close();

            handlerExecutorService.shutdownNow();
            handlerExecutorService.close();

            socket = null;
            receiveExecutorService = null;
            handlerExecutorService = null;
        }
    }

}
