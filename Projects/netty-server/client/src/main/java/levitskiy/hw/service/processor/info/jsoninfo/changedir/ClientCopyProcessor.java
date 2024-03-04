package levitskiy.hw.service.processor.info.jsoninfo.changedir;

import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

@CommandType(type = "COPY")
public class ClientCopyProcessor extends AbstractClientChangeDirProcessor {
    public ClientCopyProcessor() {
        super(MessageResponseInfo.class);
    }
}
