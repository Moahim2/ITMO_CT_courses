package levitskiy.hw.service.processor.info.jsoninfo.changedir;

import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.model.info.jsoninfo.change.ChangeDirRequestInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

import java.nio.file.Files;
import java.nio.file.Path;

@CommandType(type = "COPY")
public class ServerCopyProcessor extends AbstractServerChangeDirProcessor {
    public ServerCopyProcessor() {
        super(ChangeDirRequestInfo.class);
    }

    @Override
    protected MessageResponseInfo changeDir(Path oldFullFilePath, Path newDirName) throws Exception {
        Files.copy(oldFullFilePath, newDirName.resolve(oldFullFilePath.getFileName()));
        return new MessageResponseInfo("File has copied!");
    }
}
