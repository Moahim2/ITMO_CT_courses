package levitskiy.hw.service.processor.info.jsoninfo;

import levitskiy.hw.model.PackedData;
import levitskiy.hw.model.info.jsoninfo.JSONInfo;
import levitskiy.hw.service.processor.info.AbstractServerInfoProcessor;

/**
 * {@inheritDoc}
 * Simplified implementation based on JSON representation of all command arguments.
 * When using it, it is reasonable to create a symmetric processor on the client side.
 *
 * @param <I> requestType.
 * @param <O> responseType.
 */
public abstract class AbstractServerJSONInfoProcessor<I extends JSONInfo, O extends JSONInfo>
        extends AbstractServerInfoProcessor<I, O> {
    protected final Class<I> responseClazz;

    /**
     * @param responseClazz class for json parsing (as first generic argument)
     */
    protected AbstractServerJSONInfoProcessor(Class<I> responseClazz) {
        this.responseClazz = responseClazz;
    }

    /**
     * The default implementation only unpacks json (without another date).
     *
     * @see JSONInfo#unpackedJSONObjectFromPackedDataArguments(PackedData, Class)
     * @param request request with json arguments.
     */
    @Override
    protected I unpackRequest(PackedData request) {
        return JSONInfo.unpackedJSONObjectFromPackedDataArguments(request, responseClazz);
    }
}
