package info.kgeorgiy.ja.levitskii.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.Set;
import java.util.concurrent.*;

import static info.kgeorgiy.ja.levitskii.hello.HelloUtil.*;

public class HelloUDPNonblockingServer implements HelloServer {
    private ExecutorService receiveExecutorService = null;
    private ExecutorService handlerExecutorService = null;
    private DatagramChannel socket = null;

    private Selector selector = null;



    /**
     * Maximum of count simultaneous requests ({@value}).
     */
    public static final int MAX_REQUESTS = 1000000;


    private final class Response {
        SocketAddress socketAddress;
        String resp;

        public Response(SocketAddress socketAddress, String resp) {
            this.socketAddress = socketAddress;
            this.resp = resp;
        }
    }


    @Override
    public void start(int port, int threads) {
        final SocketAddress socketAddress = new InetSocketAddress(port);

        if (receiveExecutorService != null) {
            System.err.println("Server has already been started and isn't closed.");
            return;
        }

        final ByteBuffer receiveBuffer;

        try {
            socket = DatagramChannel.open();
            socket.bind(socketAddress);
            socket.configureBlocking(false);

            selector = Selector.open();
            receiveBuffer = ByteBuffer.allocate(1024);
            socket.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
        } catch (IOException e) {
            System.out.println("Bad creating");
            return;
        }


        receiveExecutorService = Executors.newSingleThreadExecutor();

        handlerExecutorService = new ThreadPoolExecutor(
                threads, threads, 0, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(MAX_REQUESTS), Executors.defaultThreadFactory(),
                new ThreadPoolExecutor.CallerRunsPolicy() //DiscardPolicy
        );

        Set<Response> responses = ConcurrentHashMap.newKeySet();

        receiveExecutorService.execute(() -> {
            while (socket.isOpen()) {
                try {
                    selector.select(key -> {
                        if (key.isReadable()) {
                            try {
                                SocketAddress reverseAddress = socket.receive(receiveBuffer);

                                receiveBuffer.flip();
                                byte[] data = new byte[receiveBuffer.remaining()];
                                receiveBuffer.get(data);
                                final String receiveString = new String(data, CHARSET);

                                handlerExecutorService.execute(() -> {
                                    final String response = createResponse(receiveString);
                                    responses.add(new Response(reverseAddress, response));
                                });

                            } catch (IOException e) {
                                System.out.println("readException");
                            }
                        }
                        if (key.isWritable()) {
                            try {
                                for (Response response : responses) {
                                    System.out.println(response.resp);
                                    socket.send(ByteBuffer.wrap(response.resp.getBytes(CHARSET)), response.socketAddress);
                                }
                                responses.clear();
                            } catch (IOException e) {
                                System.out.println("sendException");
                            }
                        }
                    });


                } catch (IOException e) {
                    System.out.println("SelectException");
                }
            }
        });


    }

    @Override
    public void close() {
        if (socket != null) {
            System.out.println("close");
            try {
                socket.disconnect();
                socket.close();

                receiveExecutorService.shutdownNow();
                handlerExecutorService.shutdownNow();
                handlerExecutorService.close();
                receiveExecutorService.close();

                selector.close();

                selector = null;
                socket = null;
                handlerExecutorService = null;
            } catch (IOException e) {
                System.out.println("Server close error");
            }
        }
    }
}
