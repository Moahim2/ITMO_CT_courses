package levitskiy.hw.util;

import levitskiy.hw.AuthServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.file.InvalidPathException;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Consumer;

import static levitskiy.hw.util.Utils.DEFAULT_HOST_NAME;
import static levitskiy.hw.util.Utils.DEFAULT_PORT;
import static levitskiy.hw.util.Utils.createErrorMessage;

/**
 * Storage of server-specific utilities.
 */
public class ServerUtil {
    private static final String MES_NO_SUCH_FILE_ERROR =
            "the specified file or directory does not exist on the server in the client folder";
    private static final String MES_IO_ERROR = "A file recording error occurred on the server: ";
    private static final String MES_UNKNOWN_ERROR = "An unexpected error occurred on the server: ";

    /**
     * Root of directories all users: {@value}
     */
    private static final String USERS_DATA_NAME = "userdata";
    private static final Path USERS_DATA_PATH = Paths.get(".").resolve(USERS_DATA_NAME);

    public static String createNoSuchFileErrorMessage(NoSuchFileException e) {
        return createErrorMessage(MES_NO_SUCH_FILE_ERROR, e);
    }

    public static String createIOErrorMessage(IOException e) {
        return createErrorMessage(MES_IO_ERROR, e);
    }

    public static String createUnknownErrorMessage(Exception e) {
        return createErrorMessage(MES_UNKNOWN_ERROR, e);
    }

    /**
     * Prints logs to the console in default input if {@code log} is true.
     *
     * @param log flag of console logging.
     * @param message the string to be printed.
     */
    public static void printConsoleLog(boolean log, String message) {
        writeLog(log, message, System.out::println);
    }

    /**
     * Prints logs to the console in error input if {@code log} is true.
     *
     * @param log flag of console logging.
     * @param message the string to be printed.
     */
    public static void printErrorConsoleLog(boolean log, String message) {
        writeLog(log, message, System.err::println);
    }

    public static boolean runWithDefaultIfEmptyArgs(String[] args) {
        if (args.length == 0) {
            new AuthServer().run(new InetSocketAddress(DEFAULT_HOST_NAME, DEFAULT_PORT), true);
            return true;
        }
        return false;
    }

    public static boolean checkBadCountOfArgs(String[] args) {
        if (args.length != 3) {
            System.out.println("Count of arguments must be 3.");
            return true;
        }
        return false;
    }

    public static boolean checkBadLogFlagArg(int arg) {
        if (arg != 0 && arg != 1) {
            System.err.println("Third argument must be 0 or 1 (0 - off logs, 1 - on logs)");
            return true;
        }
        return false;
    }

    /**
     * Format correct path to user file on server.
     * Always starts with {@value #USERS_DATA_NAME} and continues with the username.
     *
     * @param login user name.
     * @param pathPartNames a chain of paths without a system separator.
     *
     * @return the path starting relative to the user's personal folder
     * @throws InvalidPathException if {@code pathPartNames} is incorrect for system.
     */
    public static Path getFullUserPath(String login, String... pathPartNames) throws InvalidPathException {
        Path result = USERS_DATA_PATH.resolve(login);
        for (String pathPartName : pathPartNames) {
            result = result.resolve(pathPartName);
        }
        return result;
    }

    private static void writeLog(boolean log, String message, Consumer<String> writer) {
        if (log) {
            writer.accept(message);
        }
    }
}
