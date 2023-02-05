package expression.generic;

public class StringCharSource implements CharSourse {
    private final String string;
    protected int pos;

    public StringCharSource(String string) {
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
    public ParsingException error(String message) {
        return new ParsingException(String.format("%d: %s", pos, message));
    }
}
