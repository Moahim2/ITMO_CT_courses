package levitskiy.hw.service.processor.info;

import io.netty.channel.ChannelHandlerContext;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.model.info.Info;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.ServerRequestProcessor;

/**
 * An implementation option implying the definition of two additional classes (types I O)
 * and the definition of only auxiliary functions separately for unpacking and processing.
 *
 * @param <I> requestType.
 * @param <O> responseType.
 */
public abstract class AbstractServerInfoProcessor<I extends Info, O extends Info> implements ServerRequestProcessor {
    protected final String typeCommand = this.getClass().getAnnotation(CommandType.class).type();

    @Override
    public final void processRequestAndWriteResponse(ChannelHandlerContext ctx, PackedData request, boolean log) {
        ctx.writeAndFlush(processRequest(ctx, unpackRequest(request), log).toPackedData(typeCommand));
    }

    protected abstract I unpackRequest(PackedData request);

    protected abstract O processRequest(ChannelHandlerContext ctx, I requestInfo, boolean log);
}
