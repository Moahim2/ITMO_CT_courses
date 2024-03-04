package levitskiy.hw.util;

import java.util.Map;

import static levitskiy.hw.util.Utils.DEFAULT_HOST_NAME;
import static levitskiy.hw.util.Utils.DEFAULT_PORT;
import static levitskiy.hw.util.Utils.createErrorMessage;

public class ClientUtil {
    private static final String MES_UNKNOWN_ERROR = "An unexpected error occurred in the client application: ";

    public static String createUnknownErrorMessage(Exception e) {
        return createErrorMessage(MES_UNKNOWN_ERROR, e);
    }

    /**
     * Check correcting of input arguments and parse it.
     *
     * @param args cmd arguments.
     * @return default values if {@code args.length == 0}, else:
     * if the user's input is correct returns the entered arguments
     * otherwise returns {@code null}.
     */
    public static Map.Entry<String, Integer> getClientMainArgs(String[] args) {
        String hostName = DEFAULT_HOST_NAME;
        int port = DEFAULT_PORT;

        if (args.length != 0) {
            if (args.length != 2) {
                System.out.println("Count of arguments must be 2.");
                return null;
            }
            hostName = args[0];
            try {
                port = Integer.parseInt(args[1]);
            } catch (NumberFormatException e) {
                System.err.println("Second argument must be only integer number!");
                return null;
            }
        }
        return Map.entry(hostName, port);
    }
}
