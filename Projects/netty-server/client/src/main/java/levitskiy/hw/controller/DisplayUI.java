package levitskiy.hw.controller;

/**
 * Interface for displaying client information.
 */
public interface DisplayUI {
    /**
     * Outputs the server's response to our request to the UI
     * @param responseMessage server response
     */
    void showMessageAfterRequest(String responseMessage);

    /**
     * Outputs arbitrary normal information from the server to the UI
     * @param serviceMessages information from server.
     */
    void showServiceInformation(String... serviceMessages);

    /**
     * Outputs critical information from the server to the UI.
     * @param errorMessages information from server.
     */
    void showErrorMessage(String... errorMessages);

    /**
     * Completes interaction with the UI.
     */
    void stopAllShow();
}
