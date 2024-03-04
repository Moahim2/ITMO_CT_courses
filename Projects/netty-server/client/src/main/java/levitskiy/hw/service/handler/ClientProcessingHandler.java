package levitskiy.hw.service.handler;

import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.SimpleChannelInboundHandler;
import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.service.DisconnectProcessingHandler;
import levitskiy.hw.service.processor.ClientResponseProcessor;

import java.util.Map;

public class ClientProcessingHandler extends SimpleChannelInboundHandler<PackedData> {
    private final Map<String, ClientResponseProcessor> processors;
    private final DisplayUI displayUI;
    private final DisconnectProcessingHandler disconnectProcessingHandler;

    public ClientProcessingHandler(Map<String, ClientResponseProcessor> processors,
                                   DisplayUI displayUI,
                                   DisconnectProcessingHandler disconnectProcessingHandler) {
        this.processors = processors;
        this.displayUI = displayUI;
        this.disconnectProcessingHandler = disconnectProcessingHandler;
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) {
        displayUI.showErrorMessage("Server connection critical error!", cause.getMessage());
        ctx.close();

        disconnectProcessingHandler.processingCriticalError();
    }
    @Override
    protected void channelRead0(ChannelHandlerContext ctx, PackedData response) {
        ClientResponseProcessor processor = processors.get(response.typeCommand());

        if (processor == null) {
            System.err.println("Client cannot processing server's responses with type: " + response.typeCommand());
            return;
        }
        processor.unpackAndProcessResponse(response, displayUI);
    }
}
