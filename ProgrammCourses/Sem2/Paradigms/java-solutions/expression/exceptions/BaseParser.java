package expression.exceptions;

public class BaseParser {
    private static final char END = '\0';
    private final expression.exceptions.CharSourse source;
    private char ch;
    protected StringBuilder errorBuffer = new StringBuilder();

    public BaseParser(CharSourse source) {
        this.source = source;
        take();
    }


    protected char take() {
        final char result = ch;
        if (errorBuffer.length() < 8) {
            errorBuffer.append(result);
        } else {
            errorBuffer.delete(0, 1);
            errorBuffer.append(result);
        }
        ch = source.hasNext() ? source.next() : END;
        return result;
    }

    protected boolean take(final char expected) {
        if (current(expected)) {
            take();
            return true;
        } else {
            return false;
        }
    }

    protected boolean current(final char expected) {
        return ch == expected;
    }

    protected char current() {
        return ch;
    }

    protected void expect(final char expected) {
        if (!take(expected)) {
            throw source.error(String.format("Expected '%s', found '%s'", expected, ch));
        }
    }

    protected IllegalArgumentException error(final String message) {
        return source.error(message);
    }

    protected void expect(String expexted) {
        for (char c : expexted.toCharArray()) {
            expect(c);
        }
    }

    protected boolean between(char min, char max) {
        return ch >= min && ch <= max;
    }

    protected boolean end() {
        return take(END);
    }

    protected boolean isWhitespace() {
        return Character.isWhitespace(ch);
    }
}
