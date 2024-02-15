package expression.parser;

public class StringCharSourse implements CharSourse {
    private final String string;
    private int pos;

    protected StringCharSourse(String string) {
        this.string = string;
    }

    @Override
    public char next() {
        return string.charAt(pos++);
    }

    @Override
    public boolean hasNext() {
        return pos < string.length();
    }

    @Override
    public IllegalArgumentException error(String message) {
        return new IllegalArgumentException(String.format("%d: %s", pos, message));
    }
}
