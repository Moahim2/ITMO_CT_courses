package levitskiy.hw.service.processor.info.jsoninfo.download;

import levitskiy.hw.controller.DisplayUI;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.model.info.jsoninfo.download.DownloadRequestInfo;
import levitskiy.hw.model.info.jsoninfo.download.DownloadResponseInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.info.jsoninfo.AbstractClientJSONInfoProcessor;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;

import static levitskiy.hw.util.ClientUtil.createUnknownErrorMessage;
import static levitskiy.hw.util.Utils.createErrorMessage;
import static levitskiy.hw.util.Utils.readAllBytes;

@CommandType(type = "DOWNLOAD")
public class ClientDownloadProcessor extends AbstractClientJSONInfoProcessor<DownloadResponseInfo, DownloadRequestInfo> {
    private static final String MES_CREATING_IO_ERROR =
            "A file recording error occurred when trying to create the specified directory: ";
    private static final String MES_SAVING_IO_ERROR =
            "A file recording error occurred when trying to save loading file to directory: ";
    private static final String MES_INCORRECT_FILE_NAME_FOR_SYSTEM =
            "The returned file name is not suitable for our system";

    public ClientDownloadProcessor() {
        super(DownloadResponseInfo.class);
    }

    @Override
    protected DownloadResponseInfo unpackResponse(PackedData response, DisplayUI displayUI) {
        DownloadResponseInfo responseInfo = super.unpackResponse(response, displayUI);
        responseInfo.setByteBuf(response.otherData());
        return responseInfo;
    }

    @Override
    protected void processResponse(DownloadResponseInfo responseInfo, DisplayUI displayUI) {
        displayUI.showServiceInformation(responseInfo.getMessage());
        if (responseInfo.getByteBuf() == null) {
            return;
        }

        byte[] byteArray = readAllBytes(responseInfo.getByteBuf());
        try {
            Path fullPath = Paths.get(responseInfo.getPathToClientDir()).resolve(responseInfo.getFileName());
            Files.write(fullPath, byteArray);
            displayUI.showServiceInformation(
                    "File has saved correctly in directory: " + responseInfo.getPathToClientDir()
            );
        } catch (InvalidPathException e) {
            displayUI.showServiceInformation(MES_INCORRECT_FILE_NAME_FOR_SYSTEM);
        } catch (IOException e) {
            displayUI.showErrorMessage(createErrorMessage(MES_SAVING_IO_ERROR, e));
        } catch (Exception e) {
            displayUI.showErrorMessage(createUnknownErrorMessage(e));
        }
    }

    @Override
    protected DownloadRequestInfo processClientInput(String[] input, DisplayUI displayUI) {
        if (input.length != 2) {
            displayUI.showErrorMessage("Bad count of arguments!");
            return null;
        }
        try {
            Path pathToDir = Paths.get(input[1]);
            Files.createDirectories(pathToDir);
            return new DownloadRequestInfo(input[0], input[1]);
        } catch (InvalidPathException e) {
            displayUI.showServiceInformation(
                    "Incorrect path. You have specified an incorrect directory name for your system"
            );
        } catch (IOException e) {
            displayUI.showErrorMessage(createErrorMessage(MES_CREATING_IO_ERROR, e));
        } catch (Exception e) {
            displayUI.showErrorMessage(createUnknownErrorMessage(e));
        }
        return null;
    }
}
