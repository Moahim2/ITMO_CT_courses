package info.kgeorgiy.ja.levitskii.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.Iterator;


import static info.kgeorgiy.ja.levitskii.hello.HelloUtil.*;

public class HelloUDPNonblockingClient implements HelloClient {


    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        System.out.println("TEST + " + threads + " " + requests);
        if (requests < 0) {
            throw new IllegalArgumentException("Requests < 0");
        }

        ArrayList<Integer> numbers = new ArrayList<>();
        ArrayList<String> prefixes = new ArrayList<>();

        final SocketAddress address = new InetSocketAddress(host, port);
        try (
                final Selector sendingSelector = Selector.open();
                final Selector receiveSelector = Selector.open()
        ) {

            for (int i = 1; i < threads + 1; i++) {
                try {
                    final DatagramChannel socket = DatagramChannel.open();

                    final int curNumberOfThread = i;
                    socket.connect(address);
                    socket.configureBlocking(false);
                    final String prefixThread = prefix + curNumberOfThread + REQUEST_SPECIAL_CHAR;

                    numbers.add(1);
                    prefixes.add(prefixThread);

                    socket.register(sendingSelector, SelectionKey.OP_WRITE, i - 1);
                    socket.register(receiveSelector, SelectionKey.OP_READ, i - 1);
                } catch (IOException e) {
                    System.out.println("Socket");
                }
            }

            ByteBuffer receiveBuffer = ByteBuffer.allocate(1024);
            while (numbers.stream().anyMatch(i -> i <= requests)) {
                try {
                    sendingSelector.select();
                    for (final Iterator<SelectionKey> j = sendingSelector.selectedKeys().iterator(); j.hasNext();) {
                        final SelectionKey key = j.next();
                        try {
                            if (key.isWritable()) {
                                final DatagramChannel socket = (DatagramChannel) key.channel();
                                int i = (int) key.attachment();
                                //System.out.println(i);
                                int curNumberOfRequest = numbers.get(i);

                                try {
                                    if (numbers.get(i) <= requests) {
                                        socket.send(ByteBuffer.wrap((prefixes.get(i) + curNumberOfRequest).getBytes(CHARSET)), address);
                                        System.out.println(i + " send " + numbers.get(i) + " " + requests);
                                    }
                                } catch (IOException e) {
                                    System.out.println("Send exception");
                                }
                            }
                        } finally {
                            j.remove();
                        }
                    }

                    receiveSelector.select(key -> {
                        if (key.isReadable()) {
                            final DatagramChannel socket = (DatagramChannel) key.channel();
                            int i = (int) key.attachment();
                            int curNumberOfRequest = numbers.get(i);

                            boolean flag = true;
                            try {
                                socket.receive(receiveBuffer);

                                receiveBuffer.flip();
                                byte[] data = new byte[receiveBuffer.remaining()];
                                receiveBuffer.get(data);
                                final String receiveString = new String(data, CHARSET);


                                if (!isCorrectResponse(receiveString, prefixes.get(i) + curNumberOfRequest)) {
                                    System.out.println(receiveString);
                                    flag = false;
                                }
                            } catch (IOException e) {
                                flag = false;
                            }

                            if (flag) {
                                System.out.println(i + " receive " + numbers.get(i) + " " + requests);
                                numbers.set(i, curNumberOfRequest + 1);
                            }
                            receiveBuffer.clear();
                        }
                    }, 100);
                } catch (IOException e) {
                    //
                }
            }

        } catch (IOException e) {
            System.out.println("Selector");
        }
    }

}
