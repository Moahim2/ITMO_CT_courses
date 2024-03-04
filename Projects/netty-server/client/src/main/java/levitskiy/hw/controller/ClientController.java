package levitskiy.hw.controller;

/**
 * The interface regulating the interaction of the real user and the client-service.
 */
public interface ClientController {
    void runClientWorking(String hostName, int port);
}
