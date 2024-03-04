package levitskiy.hw.service.processor.info.jsoninfo.changedir;

import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.model.info.jsoninfo.change.ChangeDirRequestInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

@CommandType(type = "MOVE")
public class ServerMoveProcessor extends AbstractServerChangeDirProcessor {
    public ServerMoveProcessor() {
        super(ChangeDirRequestInfo.class);
    }

    @Override
    protected MessageResponseInfo changeDir(Path oldFullFilePath, Path newDirName) throws Exception {
        Files.move(
                oldFullFilePath,
                newDirName.resolve(oldFullFilePath.getFileName()),
                StandardCopyOption.REPLACE_EXISTING
        );
        return new MessageResponseInfo("File has moved!");
    }
}
