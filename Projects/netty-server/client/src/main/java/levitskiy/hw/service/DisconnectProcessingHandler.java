package levitskiy.hw.service;

/**
 * An interface that handles client shutdown when the connection is broken.
 */
public interface DisconnectProcessingHandler {
    /**
     * Notifies the outside about the failure.
     */
    void processingCriticalError();
}
