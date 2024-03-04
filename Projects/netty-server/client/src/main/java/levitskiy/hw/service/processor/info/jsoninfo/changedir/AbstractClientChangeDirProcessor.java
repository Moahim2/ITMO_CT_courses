package levitskiy.hw.service.processor.info.jsoninfo.changedir;

import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.model.info.jsoninfo.change.ChangeDirRequestInfo;
import levitskiy.hw.service.processor.info.jsoninfo.AbstractClientJSONInfoProcessor;

/**
 * Template processor for any requests of the type {@code 'file' 'dir'},
 * where both parameters are for the server file system.
 */
public abstract class AbstractClientChangeDirProcessor
        extends AbstractClientJSONInfoProcessor<MessageResponseInfo, ChangeDirRequestInfo> {
    protected AbstractClientChangeDirProcessor(Class<MessageResponseInfo> responseClazz) {
        super(responseClazz);
    }

    @Override
    protected void processResponse(MessageResponseInfo responseInfo, DisplayUI displayUI) {
        displayUI.showServiceInformation(responseInfo.message());
    }

    @Override
    protected ChangeDirRequestInfo processClientInput(String[] input, DisplayUI displayUI) {
        if (input.length != 2) {
            displayUI.showErrorMessage("Bad count of arguments!");
            return null;
        }
        return new ChangeDirRequestInfo(input[0], input[1]);
    }
}
