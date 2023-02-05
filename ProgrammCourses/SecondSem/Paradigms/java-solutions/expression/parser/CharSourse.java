package expression.parser;

public interface CharSourse {
    char next();
    boolean hasNext();

    IllegalArgumentException error(String message);
}
