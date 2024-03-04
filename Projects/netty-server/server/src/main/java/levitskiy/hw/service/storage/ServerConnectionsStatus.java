package levitskiy.hw.service.storage;

import io.netty.channel.Channel;
import io.netty.channel.ChannelHandlerContext;

import java.util.HashMap;
import java.util.Map;

/**
 * Stores information about established connections.
 */
public final class ServerConnectionsStatus {
    private static final Map<Channel, String> activeChannels = new HashMap<>();
    private static final Map<String, UserStatus> userInfo = new HashMap<>();

    public static String getChannelLogin(ChannelHandlerContext ctx) {
        return activeChannels.get(ctx.channel());
    }

    public static String removeAndGetLastLogin(ChannelHandlerContext ctx) {
        return activeChannels.remove(ctx.channel());
    }

    public static void addNewChannel(ChannelHandlerContext ctx, String login) {
        activeChannels.put(ctx.channel(), login);
    }

    public static UserStatus getUserStatus(String login) {
        return userInfo.get(login);
    }

    public static void removeUserChannelConnection(String login, ChannelHandlerContext ctx) {
        getUserStatus(login).removeChannel(ctx.channel());
    }

    public static void addNewUserIfAbsent(String login) {
        userInfo.putIfAbsent(login, new UserStatus());
    }

}
