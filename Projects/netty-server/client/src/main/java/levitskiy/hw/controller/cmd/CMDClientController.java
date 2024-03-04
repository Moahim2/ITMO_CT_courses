package levitskiy.hw.controller.cmd;

import levitskiy.hw.controller.AbstractUIClientController;
import levitskiy.hw.service.processor.ClientInputProcessor;

import java.util.Arrays;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Scanner;

import static levitskiy.hw.util.ClientUtil.getClientMainArgs;
import static levitskiy.hw.util.Utils.checkIncorrectCMDArgs;

/**
 * Implementing the controller on the command line.
 */
public class CMDClientController extends AbstractUIClientController {
    private final static String SEP = System.lineSeparator();
    private final static String HELLO_MESSAGE = "Welcome to the console version of the client!";
    private final static String COMMAND_DESCRIPTIONS = String.format(
            "%s" + ("," + SEP + "%s").repeat(4),
            "LOGIN <login> <password>",
            "UPLOAD <absolute file name> <full name directory on server>",
            "DOWNLOAD <full file name on server> <absolute directory name for file saving>",
            "COPY <old full file name> <new full directory name>",
            "MOVE <old full file name> <new full directory name>"
    );

    private final String START_MESSAGE = String.format(
            ("%s" + SEP).repeat(8),
            HELLO_MESSAGE + SEP,
            "The following commands are now implemented:",
            String.join(
                    "," + SEP,
                    serverResponseProcessors.keySet().stream().toList()
            ) + SEP,
            "All commands start with the command name!",
            "For example: ",
            COMMAND_DESCRIPTIONS,
            "Attention! All files on the server are in your personal folder" +
                    " (you don't need to know its names) all entered paths " +
                    "will start relative to it.",
            "Log in first as you need to be logged in to use the rest of the functions!"
    );

    private volatile boolean workFlag = false;

    /**
     * Expects either an empty array of arguments (then default arguments will be substituted),
     * or an array of length 2, where:
     * the 1st arg - hostname,
     * the 2nd - port number.
     *
     * @param args cmd args.
     */
    public static void main(String[] args) {
        if (checkIncorrectCMDArgs(args)) {
            return;
        }
        Map.Entry<String, Integer> address = getClientMainArgs(args);
        if (address == null) {
            return;
        }
        new CMDClientController().runClientWorking(address.getKey(), address.getValue());
    }

    @Override
    public void showMessageAfterRequest(String responseMessage) {
        showServiceInformation(responseMessage);
    }

    @Override
    public void showServiceInformation(String... serviceMessages) {
        Arrays.stream(serviceMessages).forEach(System.out::println);
    }

    @Override
    public void showErrorMessage(String... errorMessages) {
        Arrays.stream(errorMessages).forEach(System.err::println);
    }

    @Override
    public void stopAllShow() {
        workFlag = false;
    }

    /**
     * Starts the client
     * and reads the client input from the console
     * before the interrupt or the end of the input.
     * Shuts down if it could not connect to the server or the connection was interrupted
     * (taking into account the input cmd buffer).
     *
     * @param hostName hostName
     * @param port port
     */
    @Override
    public void runClientWorking(String hostName, int port) {
        if (workFlag) {
            showErrorMessage("Client has already started!");
            return;
        }

        clientService.run(hostName, port);

        if (!clientService.isConnecting()) {
            clientService.close();
            stopAllShow();
            return;
        }
        workFlag = true;

        try (Scanner scanner = new Scanner(System.in)) {
            if (workFlag) {
                showServiceInformation(START_MESSAGE);
            }
            working(scanner);
        } catch (NoSuchElementException e) {
            showErrorMessage("Input wasn't processed.");
        } finally {
            stopAllShow();
            clientService.close();
        }
    }

    private void working(Scanner scanner) {
        while (workFlag && scanner.hasNextLine()) {
            String[] input = scanner.nextLine().split(" ");
            if (!workFlag) {
                break;
            }

            ClientInputProcessor clientInputProcessor = clientInputProcessors.get(input[0]);
            if (clientInputProcessor == null) {
                showServiceInformation(input[0], " is unsupported command.");
                continue;
            }

            String[] inputWithoutCommandType = Arrays.copyOfRange(input, 1, input.length);
            clientService.sendRequest(
                    clientInputProcessor.processAndPackClientInput(this, inputWithoutCommandType)
            );
        }
    }
}
