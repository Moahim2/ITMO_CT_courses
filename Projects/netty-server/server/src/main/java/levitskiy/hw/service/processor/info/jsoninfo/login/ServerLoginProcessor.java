package levitskiy.hw.service.processor.info.jsoninfo.login;

import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;
import levitskiy.hw.dao.UserDao;
import levitskiy.hw.model.info.jsoninfo.login.LoginRequestInfo;
import levitskiy.hw.model.info.jsoninfo.MessageResponseInfo;
import levitskiy.hw.service.handler.processor.initialization.CommandType;
import levitskiy.hw.service.processor.info.jsoninfo.AbstractServerJSONInfoProcessor;
import levitskiy.hw.service.storage.UserStatus;

import static levitskiy.hw.service.storage.ServerConnectionsStatus.addNewChannel;
import static levitskiy.hw.service.storage.ServerConnectionsStatus.addNewUserIfAbsent;
import static levitskiy.hw.service.storage.ServerConnectionsStatus.getChannelLogin;
import static levitskiy.hw.service.storage.ServerConnectionsStatus.getUserStatus;
import static levitskiy.hw.util.ServerUtil.printConsoleLog;

@CommandType(type = "LOGIN")
public class ServerLoginProcessor extends AbstractServerJSONInfoProcessor<LoginRequestInfo, MessageResponseInfo> {
    private static final int MAX_ATTEMPTS_COUNT = 3;

    public ServerLoginProcessor() {
        super(LoginRequestInfo.class);
    }

    @Override
    protected MessageResponseInfo processRequest(ChannelHandlerContext ctx, LoginRequestInfo requestInfo, boolean log) {
        if (requestInfo == null) {
            printConsoleLog(log, "BAD request!");
            return null;
        }

        String activeLogin = getChannelLogin(ctx);
        if (activeLogin != null) {
            return new MessageResponseInfo("You are already logged in to the system under the username: " + activeLogin);
        }


        String userLogin = requestInfo.login();
        String userPassword = requestInfo.password();

        String correctPassword = UserDao.getUserPassword(userLogin);
        if (correctPassword == null) {
            return new MessageResponseInfo("The login is incorrect.");
        }

        addNewUserIfAbsent(userLogin);
        UserStatus curUserStatus = getUserStatus(userLogin);

        Channel curChannel = ctx.channel();
        if (curUserStatus.getAttemptNumber(curChannel) >= MAX_ATTEMPTS_COUNT) {
            return new MessageResponseInfo("You already have no attempts to enter the password.");
        }

        String response = checkIncorrectInputPassword(correctPassword, userPassword, curUserStatus, curChannel);
        if (response != null) {
            return new MessageResponseInfo(response);
        } else {
            addAuthenticationChannel(ctx, userLogin, log);
            return new MessageResponseInfo("Good authentication!");
        }
    }

    private String checkIncorrectInputPassword(String correctPassword,
                                               String userInputPassword,
                                               UserStatus curUserStatus,
                                               Channel curChannel) {
        if (!correctPassword.equals(userInputPassword)) {
            curUserStatus.incAttemptNumber(curChannel);
            return String.format(
                    "%s %d %s",
                    "The login is correct but the password is not correct. You have",
                    MAX_ATTEMPTS_COUNT - curUserStatus.getAttemptNumber(curChannel),
                    "attempts left to enter your password."
            );
        }
        return null;
    }

    private void addAuthenticationChannel(ChannelHandlerContext ctx, String userInputLogin, boolean log) {
        addNewChannel(ctx, userInputLogin);
        printConsoleLog(log, "User " + userInputLogin + " is authenticated from the channel " + ctx.channel());
    }
}
