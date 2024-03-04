package levitskiy.hw.service.processor.info.jsoninfo.upload;

import io.netty.channel.ChannelHandlerContext;
import levitskiy.hw.model.PackedData;
import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.model.info.jsoninfo.upload.UploadRequestInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.info.jsoninfo.AbstractServerJSONInfoProcessor;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.Arrays;

import static levitskiy.hw.service.storage.ServerConnectionsStatus.getChannelLogin;
import static levitskiy.hw.util.ServerUtil.createIOErrorMessage;
import static levitskiy.hw.util.ServerUtil.createUnknownErrorMessage;
import static levitskiy.hw.util.ServerUtil.getFullUserPath;
import static levitskiy.hw.util.ServerUtil.printConsoleLog;
import static levitskiy.hw.util.ServerUtil.printErrorConsoleLog;
import static levitskiy.hw.util.Utils.readAllBytes;

@CommandType(type = "UPLOAD")
public class ServerUploadProcessor extends AbstractServerJSONInfoProcessor<UploadRequestInfo, MessageResponseInfo> {
    private static final String MES_CORRECT_UPLOAD = "File has uploaded correctly.";

    public ServerUploadProcessor() {
        super(UploadRequestInfo.class);
    }

    @Override
    protected UploadRequestInfo unpackRequest(PackedData request) {
        UploadRequestInfo requestInfo = super.unpackRequest(request);
        requestInfo.setByteBuf(request.otherData());
        return requestInfo;
    }

    @Override
    protected MessageResponseInfo processRequest(ChannelHandlerContext ctx, UploadRequestInfo requestInfo, boolean log) {
        String login = getChannelLogin(ctx);
        if (login == null) {
            return new MessageResponseInfo("You are not logged in to the system, enter your username and password.");
        }

        byte[] byteArray = readAllBytes(requestInfo.getByteBuf());
        try {
            Path fullPath = getFullUserPath(login, requestInfo.getPathToDir(), requestInfo.getFileName());
            Files.createDirectories(fullPath.getParent());
            Files.write(fullPath, byteArray);
        } catch (InvalidPathException e) {
            printConsoleLog(log, e.getMessage());
            return new MessageResponseInfo("You have specified an incorrect directory name for server.");
        } catch (IOException e) {
            printErrorConsoleLog(log, e.toString());
            return new MessageResponseInfo(createIOErrorMessage(e));
        } catch (Exception e) {
            printConsoleLog(log, Arrays.toString(e.getStackTrace()));
            return new MessageResponseInfo(createUnknownErrorMessage(e));
        }
        return new MessageResponseInfo(MES_CORRECT_UPLOAD);
    }
}
