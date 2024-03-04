package levitskiy.hw.service.processor.info.jsoninfo.changedir;

import io.netty.channel.ChannelHandlerContext;
import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.model.info.jsoninfo.change.ChangeDirRequestInfo;
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

/**
 * Template processor for any requests of the type {@code 'file' 'dir'},
 * where both parameters are for the server file system.
 */
public abstract class AbstractServerChangeDirProcessor
        extends AbstractServerJSONInfoProcessor<ChangeDirRequestInfo, MessageResponseInfo> {
    protected AbstractServerChangeDirProcessor(Class<ChangeDirRequestInfo> responseClazz) {
        super(responseClazz);
    }

    @Override
    protected MessageResponseInfo processRequest(ChannelHandlerContext ctx, ChangeDirRequestInfo requestInfo, boolean log) {
        String login = getChannelLogin(ctx);
        if (login == null) {
            return new MessageResponseInfo("You are not logged in to the system, enter your username and password.");
        }
        try {
            Path oldFullFilePath = getFullUserPath(login, requestInfo.oldFilePathName());
            Path newDirName = getFullUserPath(login, requestInfo.newDirName());
            Files.createDirectories(newDirName);

            return changeDir(oldFullFilePath, newDirName);
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
    }

    /**
     * The final semantic operation on the transmitted correct paths.
     * @param oldFullFilePath file
     * @param newDirName dir
     * @return response
     */
    protected abstract MessageResponseInfo changeDir(Path oldFullFilePath, Path newDirName) throws Exception;
}
