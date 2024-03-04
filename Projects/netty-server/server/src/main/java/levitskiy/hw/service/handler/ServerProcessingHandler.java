package levitskiy.hw.service.handler;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.SimpleChannelInboundHandler;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.service.processor.ServerRequestProcessor;

import java.util.Map;

import static levitskiy.hw.service.storage.ServerConnectionsStatus.removeAndGetLastLogin;
import static levitskiy.hw.service.storage.ServerConnectionsStatus.removeUserChannelConnection;
import static levitskiy.hw.util.ServerUtil.printConsoleLog;
import static levitskiy.hw.util.ServerUtil.printErrorConsoleLog;

public class ServerProcessingHandler extends SimpleChannelInboundHandler<PackedData> {
    private final Map<String, ServerRequestProcessor> requestProcessors;
    private final boolean log;

    public ServerProcessingHandler(Map<String, ServerRequestProcessor> requestProcessors, boolean log) {
        this.requestProcessors = requestProcessors;
        this.log = log;
    }

    @Override
    public void channelInactive(ChannelHandlerContext ctx) {
        printConsoleLog(log, "Inactive channel: " + ctx.channel());

        String lastLogin = removeAndGetLastLogin(ctx);
        if (lastLogin != null) {
            removeUserChannelConnection(lastLogin, ctx);
        }
        ctx.close();
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        printErrorConsoleLog(log, cause.getMessage());
        channelInactive(ctx);
    }

    @Override
    protected void channelRead0(ChannelHandlerContext ctx, PackedData request) {
        ServerRequestProcessor serverRequestProcessor = requestProcessors.get(request.typeCommand());
        if (serverRequestProcessor == null) {
            printConsoleLog(
                    log,
                    "For technical reasons, the server is not able to process the following type of request: " +
                            request.typeCommand()
            );
            return;
        }
        serverRequestProcessor.processRequestAndWriteResponse(ctx, request, log);
    }
}
