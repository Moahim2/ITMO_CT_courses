package levitskiy.hw.service.processor.info.jsoninfo.download;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelHandlerContext;
import levitskiy.hw.model.info.jsoninfo.download.DownloadRequestInfo;
import levitskiy.hw.model.info.jsoninfo.download.DownloadResponseInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.info.jsoninfo.AbstractServerJSONInfoProcessor;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Arrays;

import static levitskiy.hw.service.storage.ServerConnectionsStatus.getChannelLogin;
import static levitskiy.hw.util.ServerUtil.createIOErrorMessage;
import static levitskiy.hw.util.ServerUtil.createNoSuchFileErrorMessage;
import static levitskiy.hw.util.ServerUtil.createUnknownErrorMessage;
import static levitskiy.hw.util.ServerUtil.getFullUserPath;
import static levitskiy.hw.util.ServerUtil.printConsoleLog;
import static levitskiy.hw.util.ServerUtil.printErrorConsoleLog;

@CommandType(type = "DOWNLOAD")
public class ServerDownloadProcessor extends AbstractServerJSONInfoProcessor<DownloadRequestInfo, DownloadResponseInfo> {
    private static final String MES_CORRECT_LOAD = "File has correctly downloaded.";

    public ServerDownloadProcessor() {
        super(DownloadRequestInfo.class);
    }

    @Override
    protected DownloadResponseInfo processRequest(ChannelHandlerContext ctx, DownloadRequestInfo requestInfo, boolean log) {
        String pathToDir = requestInfo.pathToDir();
        String login = getChannelLogin(ctx);
        if (login == null) {
            return new DownloadResponseInfo(
                    "You are not logged in to the system, enter your username and password.",
                    null,
                    null);
        }

        try {
            Path fullPath = getFullUserPath(login, requestInfo.pathToLoadingFile());
            Files.createDirectories(fullPath.getParent());

            DownloadResponseInfo responseInfo = new DownloadResponseInfo(
                    MES_CORRECT_LOAD,
                    pathToDir,
                    fullPath.getFileName().toString()
            );

            ByteBuf byteBuf = Unpooled.copiedBuffer(Files.readAllBytes(fullPath));
            responseInfo.setByteBuf(byteBuf);
            return responseInfo;
        } catch (InvalidPathException e) {
            printConsoleLog(log, e.getMessage());
            return new DownloadResponseInfo(
                    "You have specified an incorrect directory name for server.",
                    pathToDir,
                    null
            );
        } catch (NoSuchFileException e) {
            printErrorConsoleLog(log, e.toString());
            return new DownloadResponseInfo(createNoSuchFileErrorMessage(e), pathToDir, null);
        } catch (IOException e) {
            printErrorConsoleLog(log, e.toString());
            return new DownloadResponseInfo(createIOErrorMessage(e), pathToDir, null);
        } catch (Exception e) {
            printConsoleLog(log, Arrays.toString(e.getStackTrace()));
            return new DownloadResponseInfo(createUnknownErrorMessage(e), pathToDir, null);
        }
    }
}
