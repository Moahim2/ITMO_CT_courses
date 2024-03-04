package levitskiy.hw.util;

import io.netty.buffer.ByteBuf;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;

/**
 * Utilities and constants common to server and client.
 */
public class Utils {
    public static final Charset DEFAULT_CHARSET = StandardCharsets.UTF_8;
    public static final String DEFAULT_HOST_NAME = "localhost";
    public static final int DEFAULT_PORT = 8080;

    public static byte[] readAllBytes(ByteBuf in) {
        byte[] byteArray = new byte[in.readableBytes()];
        in.readBytes(byteArray);
        return byteArray;
    }

    /**
     * Auxiliary function for checking incorrect program input.
     *
     * @return true if args isn't null and doesn't contain nulls.
     */
    public static boolean checkIncorrectCMDArgs(String[] args) {
        return args == null || Arrays.stream(args).anyMatch(Objects::isNull);
    }

    /**
     * Format message about exception.
     * @param staticMessage start of message.
     * @param e exception
     * @return message start with {@code staticMessage}
     */
    public static String createErrorMessage(String staticMessage, Exception e) {
        return staticMessage + e.toString();
    }
}
