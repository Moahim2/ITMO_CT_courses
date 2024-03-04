package levitskiy.hw.service.processor.info.jsoninfo;

import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.model.info.jsoninfo.JSONInfo;
import levitskiy.hw.service.processor.info.AbstractClientInfoProcessor;

/**
 * {@inheritDoc}
 * Simplified implementation based on JSON representation of all command arguments.
 * When using it, it is reasonable to create a symmetric processor on the server side.
 *
 * @param <O> responseType.
 * @param <I> requestType.
 */
public abstract class AbstractClientJSONInfoProcessor<O extends JSONInfo, I extends JSONInfo>
        extends AbstractClientInfoProcessor<O, I> {
    protected final Class<O> responseClazz;

    /**
     * @param responseClazz class for json parsing (as first generic argument).
     */
    protected AbstractClientJSONInfoProcessor(Class<O> responseClazz) {
        this.responseClazz = responseClazz;
    }

    /**
     * The default implementation only unpacks json (without another date).
     *
     * @see JSONInfo#unpackedJSONObjectFromPackedDataArguments(PackedData, Class)
     * @param response response with json arguments.
     */
    @Override
    protected O unpackResponse(PackedData response, DisplayUI displayUI) {
        return JSONInfo.unpackedJSONObjectFromPackedDataArguments(response, responseClazz);
    }
}
