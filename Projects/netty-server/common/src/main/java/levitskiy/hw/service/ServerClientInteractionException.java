package levitskiy.hw.service;

/**
 * Exception related specifically to the interaction of the server and the client.
 * For example, if there is a command data packet configuration not agreed between server and client.
 */
public class ServerClientInteractionException extends RuntimeException {

    public ServerClientInteractionException(String message, Throwable e) {
        super(message, e);
    }
}
