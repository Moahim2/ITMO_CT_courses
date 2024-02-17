package hw.levitskiy.$generating$.calculator;

public class Token {
    private final String textVal;
    private final Terminal terminal;

    public Token(String textVal, Terminal terminal) {
        this.textVal = textVal;
        this.terminal = terminal;
    }


    public String getTextVal() {
        return textVal;
    }

    public Terminal getTerminal() {
        return terminal;
    }
}
