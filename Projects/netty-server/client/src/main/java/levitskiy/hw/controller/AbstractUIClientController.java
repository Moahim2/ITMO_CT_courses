package levitskiy.hw.controller;

import levitskiy.hw.service.ClientService;
import levitskiy.hw.service.processor.ClientInputProcessor;
import levitskiy.hw.service.processor.ClientResponseProcessor;

import java.util.Map;

import static levitskiy.hw.service.handler.processor.initialization.ProcessorInitializer.initAllImplementationOfInterfaceWithCommandTypeAnnotation;

/**
 * A controller that combines external request processing and displaying all information in a certain UI.
 */
public abstract class AbstractUIClientController implements ClientController, DisplayUI {
    protected final Map<String, ClientInputProcessor> clientInputProcessors = initAllClientInputProcessors();
    protected final Map<String, ClientResponseProcessor> serverResponseProcessors = initAllServerResponseProcessors();
    protected final ClientService clientService;

    public AbstractUIClientController() {
        this.clientService = new ClientService(serverResponseProcessors, this);
    }


    protected Map<String, ClientInputProcessor> initAllClientInputProcessors() {
        return initAllImplementationOfInterfaceWithCommandTypeAnnotation(ClientInputProcessor.class);
    }

    protected Map<String, ClientResponseProcessor> initAllServerResponseProcessors() {
        return initAllImplementationOfInterfaceWithCommandTypeAnnotation(ClientResponseProcessor.class);
    }
}
