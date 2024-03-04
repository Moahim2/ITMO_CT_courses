package levitskiy.hw.service.storage;

import io.netty.channel.Channel;

import java.util.HashMap;
import java.util.Map;

/**
 * A class that stores information about
 * the connection status to the user on the server from different devices.
 */
public class UserStatus {
    /**
     * Information about the number of authentication attempts from each specific device.
     */
    private final Map<Channel, Integer> channelAttemptNumberMap = new HashMap<>();

    public void incAttemptNumber(Channel channel) {
        int attemptNumber = channelAttemptNumberMap.getOrDefault(channel, 0);
        channelAttemptNumberMap.put(channel, attemptNumber + 1);
    }

    public int getAttemptNumber(Channel channel) {
        channelAttemptNumberMap.putIfAbsent(channel, 0);
        return channelAttemptNumberMap.get(channel);
    }

    public void removeChannel(Channel channel) {
        channelAttemptNumberMap.remove(channel);
    }
}
