package levitskiy.hw.service.processor.info.jsoninfo.changedir;

import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

@CommandType(type = "MOVE")
public class ClientMoveProcessor extends AbstractClientChangeDirProcessor {
    public ClientMoveProcessor() {
        super(MessageResponseInfo.class);
    }
}
