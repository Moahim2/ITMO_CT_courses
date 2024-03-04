package levitskiy.hw;

import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.ChannelFuture;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import levitskiy.hw.converter.PackedDataDecoder;
import levitskiy.hw.converter.PackedDataEncoder;
import levitskiy.hw.service.handler.ServerProcessingHandler;
import levitskiy.hw.service.processor.ServerRequestProcessor;

import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Map;

import static levitskiy.hw.service.handler.processor.initialization.ProcessorInitializer.initAllImplementationOfInterfaceWithCommandTypeAnnotation;
import static levitskiy.hw.util.ServerUtil.checkBadCountOfArgs;
import static levitskiy.hw.util.ServerUtil.checkBadLogFlagArg;
import static levitskiy.hw.util.ServerUtil.printConsoleLog;
import static levitskiy.hw.util.ServerUtil.runWithDefaultIfEmptyArgs;
import static levitskiy.hw.util.Utils.DEFAULT_CHARSET;
import static levitskiy.hw.util.Utils.checkIncorrectCMDArgs;

public class AuthServer {
    private static final Map<String, ServerRequestProcessor> requestProcessors =
            initAllImplementationOfInterfaceWithCommandTypeAnnotation(ServerRequestProcessor.class);

    /**
     * Starting the server from cmd.
     * Expects either an empty array of arguments (then default arguments will be substituted),
     * or an array of length 3, where:
     * the 1st arg - hostname,
     * the 2nd - port number.
     * the 3rd - on/off logs in console.
     *
     * @param args cmd arguments.
     */
    public static void main(String[] args) {
        if (checkIncorrectCMDArgs(args) || runWithDefaultIfEmptyArgs(args) || checkBadCountOfArgs(args)) {
            return;
        }

        try {
            int thirdArg = Integer.parseInt(args[2]);
            if (checkBadLogFlagArg(thirdArg)) {
                return;
            }
            new AuthServer().run(new InetSocketAddress(args[0], Integer.parseInt(args[1])), thirdArg == 1);
        } catch (NumberFormatException e) {
            System.err.println("Second and third argument must be only integer number!");
        }
    }

    /**
     * Run server on provided address in current thread with efficient parallel handlers.
     *
     * @param socketAddress address of server.
     * @param log flag on/off simple cmd logging.
     */
    public void run(SocketAddress socketAddress, boolean log) {
        EventLoopGroup bossGroup = new NioEventLoopGroup();
        EventLoopGroup workerGroup = new NioEventLoopGroup();

        ServerBootstrap bootstrap = new ServerBootstrap()
                .group(bossGroup, workerGroup)
                .channel(NioServerSocketChannel.class)
                .childHandler(new ChannelInitializer<SocketChannel>() {
                    @Override
                    protected void initChannel(SocketChannel ch) {
                        ChannelPipeline channelPipeline = ch.pipeline();
                        printConsoleLog(log, "Init new channel: " + ch);

                        channelPipeline.addLast("encoder", new PackedDataEncoder(DEFAULT_CHARSET));
                        channelPipeline.addLast("decoder", new PackedDataDecoder(DEFAULT_CHARSET));
                        channelPipeline.addLast("handler", new ServerProcessingHandler(requestProcessors, log));
                    }
                });
        try {
            ChannelFuture f = bootstrap.bind(socketAddress).sync();
            printConsoleLog(log, "Server start!");

            f.channel().closeFuture().sync();
        } catch (InterruptedException e) {
            printConsoleLog(log, "Server close!");
        } catch (Exception e) {
            System.err.println(e.getMessage());
            printConsoleLog(log, "The server cannot start or working!");
            printConsoleLog(log, "Check the port availability!");
        } finally {
            workerGroup.shutdownGracefully();
            bossGroup.shutdownGracefully();
        }
    }
}
