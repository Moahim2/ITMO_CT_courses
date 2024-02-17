package hw.levitskiy.exception;

public class GenException extends RuntimeException {

    protected GenException(String message, Throwable e) {
        super(message, e);
    }

    protected GenException(String message) {
        super(message);
    }

}
