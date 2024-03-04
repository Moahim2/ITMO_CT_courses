package levitskiy.hw.service.processor;

import io.netty.channel.ChannelHandlerContext;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

/**
 * Base interface for processing user requests.
 * For the application to work correctly with a specific implementation of this interface,
 * implementation must be:
 * <ul>
 *      <li>annotated {@link CommandType} with specific uniq type</li>
 *      <li>unique type must match the annotation type of the corresponding client processors.</li>
 *      <li>the implementation of this class must have a public constructor</li>
 *      <li>the public constructor must be without arguments</li>
 * </ul>
 */
public interface ServerRequestProcessor {
    void processRequestAndWriteResponse(ChannelHandlerContext ctx, PackedData request, boolean log);
}
