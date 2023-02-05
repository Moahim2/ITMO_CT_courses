package expression.generic;

public interface CharSourse {
    char next();
    boolean hasNext();
    ParsingException error(String message);
}
