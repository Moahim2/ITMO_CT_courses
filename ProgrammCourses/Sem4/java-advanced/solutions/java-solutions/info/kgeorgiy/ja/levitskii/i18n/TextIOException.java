package info.kgeorgiy.ja.levitskii.i18n;

import java.io.IOException;

public class TextIOException extends IOException {

    public TextIOException(final String message) {
        super(message);
    }

    public TextIOException(final String message, final Throwable cause) {
        super(message, cause);
    }

}
