package levitskiy.hw.service;

import io.netty.bootstrap.Bootstrap;
import io.netty.channel.Channel;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;
import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.converter.PackedDataDecoder;
import levitskiy.hw.converter.PackedDataEncoder;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.service.handler.ClientProcessingHandler;
import levitskiy.hw.service.processor.ClientResponseProcessor;

import java.io.Closeable;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Map;
import java.util.concurrent.BrokenBarrierException;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import static levitskiy.hw.util.Utils.DEFAULT_CHARSET;

/**
 * A class representing of user interaction with the server.
 */
public class ClientService implements Closeable {
    private final Map<String, ClientResponseProcessor> processors;
    private final DisplayUI displayUI;

    private final ChannelInitializer<SocketChannel> clientInitializer = new ChannelInitializer<>() {
        @Override
        protected void initChannel(SocketChannel ch) {
            ChannelPipeline channelPipeline = ch.pipeline();

            channelPipeline.addLast("encoder", new PackedDataEncoder(DEFAULT_CHARSET));
            channelPipeline.addLast("decoder", new PackedDataDecoder(DEFAULT_CHARSET));
            channelPipeline.addLast(
                    "handler",
                    new ClientProcessingHandler(
                            processors,
                            displayUI,
                            new SimpleDisconnectProcessingHandler()
                    )
            );
        }
    };

    private Channel channel;
    private Future<?> futureClientRunning;

    public ClientService(Map<String, ClientResponseProcessor> processors, DisplayUI displayUI) {
        this.processors = processors;
        this.displayUI = displayUI;
    }

    /**
     * Tries to connect to the specified server address (in parallel thread)
     * and ends only when ready to make requests or when an error occurs.
     * @param hostName hostName.
     * @param port port.
     */
    public void run(final String hostName, int port) {
        if (isConnecting()) {
            displayUI.showServiceInformation("Client has already started!");
            return;
        }

        final CyclicBarrier barrier = new CyclicBarrier(2);
        final ExecutorService executorService = Executors.newSingleThreadExecutor();

        futureClientRunning = executorService.submit(() ->
                workingClient(new InetSocketAddress(hostName, port), barrier)
        );

        try {
            barrier.await();
        } catch (BrokenBarrierException e) {
            close();
        } catch (InterruptedException e) {
            displayUI.showServiceInformation("An external stop has occurred!");
            close();
        } finally {
            executorService.shutdown();
        }
    }

    /**
     * Try to send request to server if {@code request != null}.
     * @param request request.
     */
    public void sendRequest(PackedData request) {
        if (request != null) {
            channel.writeAndFlush(request);
        }
    }

    /**
     * Check connection with server.
     * @return true if connection is open.
     */
    public boolean isConnecting() {
        return channel != null && channel.isOpen();
    }

    /**
     * Interrupts and terminates the client's current working.
     */
    @Override
    public void close() {
        if (channel == null) {
            return;
        }

        futureClientRunning.cancel(true);
        futureClientRunning = null;
        channel = null;
    }

    /**
     * Create connection between server and client.
     * Performs client configuration.
     *
     * @param socketAddress server address.
     * @param barrier synchronization between potential UI and thread starting.
     */
    private void workingClient(SocketAddress socketAddress, CyclicBarrier barrier) {
        EventLoopGroup workerGroup = new NioEventLoopGroup();

        Bootstrap bootstrap = new Bootstrap()
                .group(workerGroup)
                .channel(NioSocketChannel.class)
                .option(ChannelOption.SO_KEEPALIVE, true)
                .handler(clientInitializer);
        try {
            channel = bootstrap.connect(socketAddress).sync().channel();
            barrier.await();

            channel.closeFuture().sync();
        } catch (BrokenBarrierException e) {
            displayUI.showErrorMessage(e.getMessage());
        } catch (InterruptedException e) {
            displayUI.showServiceInformation("Stop client!");
        } catch (Exception e) {
            displayUI.showErrorMessage(e.getMessage());
            displayUI.showServiceInformation("Connection refused!", "(Check if the server is enabled!)");
            barrier.reset();
        } finally {
            displayUI.stopAllShow();
            workerGroup.shutdownGracefully();
        }
    }

    /**
     * Handler of internal failures between server and client.
     */
    private final class SimpleDisconnectProcessingHandler implements DisconnectProcessingHandler {
        @Override
        public void processingCriticalError() {
            close();
        }
    }
}
