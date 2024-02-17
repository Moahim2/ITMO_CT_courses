package hw.levitskiy.exception;

public class ParserGenException extends GenException {

    public ParserGenException(String message, Throwable e) {
        super(message, e);
    }

    public ParserGenException(String message) {
        super(message);
    }

}
