package levitskiy.hw.service.processor.info.jsoninfo.login;

import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.info.jsoninfo.login.LoginRequestInfo;
import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.info.jsoninfo.AbstractClientJSONInfoProcessor;

@CommandType(type = "LOGIN")
public class ClientLoginProcessor extends AbstractClientJSONInfoProcessor<MessageResponseInfo, LoginRequestInfo> {
    public ClientLoginProcessor() {
        super(MessageResponseInfo.class);
    }

    @Override
    public void processResponse(MessageResponseInfo responseInfo, DisplayUI displayUI) {
        displayUI.showMessageAfterRequest(responseInfo.message());
    }

    @Override
    public LoginRequestInfo processClientInput(String[] input, DisplayUI displayUI) {
        if (input.length != 2) {
            displayUI.showErrorMessage("Bad count of arguments!");
            return null;
        }
        return new LoginRequestInfo(input[0], input[1]);
    }
}
