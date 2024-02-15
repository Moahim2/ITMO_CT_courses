package info.kgeorgiy.ja.levitskii.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static info.kgeorgiy.ja.levitskii.hello.HelloUtil.*;


/**
 * This class is implementation of the client interface {@link HelloClient}.
 * It sends requests to the server, accepts the results and outputs them to the console.
 * Contains {@link #main(String[])} and therefore can be run from the command line.
 *
 * @author Ivanl
 */
public class HelloUDPClient implements HelloClient {

    /**
     * Special value {@value} for limit the maximum waiting time for receiving packets
     */
    public static final int TIME_OUT = 100;


    /**
     * Method for a place to run client from the command line.
     * Accepts arguments and expects them only in the format {@code host port prefix threads requests}.
     * When:
     * <ul>
     *     <li>
     *         host - the name or ip address of the computer on which the server is running;
     *     </li>
     *     <li>
     *         port - the port number to send requests to;
     *     </li>
     *     <li>
     *         prefix - request prefix (string);
     *     </li>
     *     <li>
     *         threads - number of parallel request threads;
     *     </li>
     *     <li>
     *         requests - number of requests in each thread.
     *     </li>
     * </ul>
     *
     * @param args (usually) array of cmd arguments.
     */
    public static void main(String[] args) {
        if (checkArgs(args, 5)) {
            return;
        }

        try {
            final HelloUDPClient client = new HelloUDPClient();

            client.run(
                    args[0], Integer.parseInt(args[1]), args[2], Integer.parseInt(args[3]), Integer.parseInt(args[4])
            );

        } catch (NumberFormatException e) {
            System.err.println(e.getMessage());
        }
    }


    /**
     * {@inheritDoc}
     * Sends requests in the format {@code prefix + "number of current thread" +
     * {@value HelloUtil#REQUEST_SPECIAL_CHAR } + "number of request in current thread"}
     * Waits for responses according to the server {@link HelloUDPServer}.
     *
     * @param host     server host
     * @param port     server port
     * @param prefix   request prefix
     * @param threads  number of request threads
     * @param requests number of requests per thread.
     */
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        if (requests < 0) {
            throw new IllegalArgumentException("Requests < 0");
        }

        try (final ExecutorService executorService = Executors.newFixedThreadPool(threads)) {
            final SocketAddress address = new InetSocketAddress(host, port);


            for (int i = 1; i < threads + 1; i++) {
                final int curNumberOfThread = i;
                final String prefixThread = prefix + curNumberOfThread + REQUEST_SPECIAL_CHAR;

                executorService.execute(() -> worker(requests, address, curNumberOfThread, prefixThread));
            }
        }
    }

    private void worker(int requests, SocketAddress address, int numberOfThread, String prefixThread) {
        try (final DatagramSocket socket = new DatagramSocket(null)) {
            socket.connect(address);
            socket.setSoTimeout(TIME_OUT);

            final DatagramPacket receivePacket = createPacket(getValidReceiveBuffer(socket));

            for (int j = 1; j < requests + 1; j++) {
                processRequest(address, prefixThread, socket, receivePacket, j);
            }
        } catch (SocketException e) {
            System.err.println(e.getMessage());
            System.err.println("Cannot create client socket on " + numberOfThread + " thread.");
        }
    }

    private void processRequest(SocketAddress address, String prefixThread,
                                DatagramSocket socket, DatagramPacket receivePacket, int numberOfRequest) {
        final String fullRequestString = prefixThread + numberOfRequest;

        final byte[] sendingData = fullRequestString.getBytes(CHARSET);

        String response = null;
        while (!isCorrectResponse(response, fullRequestString)) {
            try {
                sendData(socket, sendingData, address);
            } catch (IOException e) {
                logClientMessagesExceptions(
                        "Error when sending request: " + fullRequestString, e
                );
                continue;
            }


            try {
                socket.receive(receivePacket);
                response = getStringFromDatagramPacket(receivePacket);
            } catch (SocketTimeoutException ignored) {
                // normal situation
            } catch (IOException e) {
                logClientMessagesExceptions(
                        "Error when receiving response on request: " + fullRequestString, e
                );
            }
        }

        System.out.println("Receive: " + response);
    }

    private void logClientMessagesExceptions(String metaInformation, IOException e) {
        //anything
        //
        //for example:
        System.err.println(metaInformation + " " + e.getMessage());
    }

}
