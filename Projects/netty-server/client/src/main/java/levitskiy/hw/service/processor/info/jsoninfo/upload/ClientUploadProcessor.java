package levitskiy.hw.service.processor.info.jsoninfo.upload;

import io.netty.buffer.Unpooled;
import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.model.info.jsoninfo.upload.UploadRequestInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.info.jsoninfo.AbstractClientJSONInfoProcessor;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;

@CommandType(type = "UPLOAD")
public class ClientUploadProcessor extends AbstractClientJSONInfoProcessor<MessageResponseInfo, UploadRequestInfo> {
    public ClientUploadProcessor() {
        super(MessageResponseInfo.class);
    }

    @Override
    protected void processResponse(MessageResponseInfo responseInfo, DisplayUI displayUI) {
        displayUI.showMessageAfterRequest(responseInfo.message());
    }

    @Override
    protected UploadRequestInfo processClientInput(String[] input, DisplayUI displayUI) {
        if (input.length != 2) {
            displayUI.showErrorMessage("Bad count of arguments!");
            return null;
        }
        Path file;
        try {
            file = Paths.get(input[0]);
        } catch (InvalidPathException e) {
            displayUI.showErrorMessage("Incorrect file name");
            return null;
        }

        UploadRequestInfo requestInfo = new UploadRequestInfo(input[1], file.getFileName().toString());
        try {
            requestInfo.setByteBuf(Unpooled.copiedBuffer(Files.readAllBytes(file)));
        } catch (IOException e) {
            displayUI.showErrorMessage("File not found or not readable.");
            displayUI.showServiceInformation(
                    "Please, input absolute path to file or directory on your system!"
            );
            return null;
        }
        return requestInfo;
    }
}
