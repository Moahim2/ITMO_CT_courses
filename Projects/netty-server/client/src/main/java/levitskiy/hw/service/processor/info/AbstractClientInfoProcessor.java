package levitskiy.hw.service.processor.info;

import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.model.info.Info;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.ClientInputProcessor;
import levitskiy.hw.service.processor.ClientResponseProcessor;

/**
 * {@inheritDoc}
 * An implementation variant combining interfaces {@link ClientResponseProcessor} and {@link ClientInputProcessor}
 * and implying the definition of two additional classes (types I O)
 * and the definition of only auxiliary functions separately for unpacking and processing.
 *
 * @param <O> responseType.
 * @param <I> requestType.
 */
public abstract class AbstractClientInfoProcessor<O extends Info, I extends Info>
        implements ClientResponseProcessor, ClientInputProcessor {
    protected final String typeCommand = this.getClass().getAnnotation(CommandType.class).type();

    @Override
    public final void unpackAndProcessResponse(PackedData response, DisplayUI displayUI) {
        processResponse(unpackResponse(response, displayUI), displayUI);
    }

    @Override
    public final PackedData processAndPackClientInput(DisplayUI displayUI, String[] input) {
        I requestInfo = processClientInput(input, displayUI);
        if (requestInfo == null) {
            return null;
        } else {
            return requestInfo.toPackedData(typeCommand);
        }
    }

    protected abstract O unpackResponse(PackedData response, DisplayUI displayUI);

    protected abstract void processResponse(O responseInfo, DisplayUI displayUI);

    protected abstract I processClientInput(String[] input, DisplayUI displayUI);
}
