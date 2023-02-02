package expression;

public class Subtract extends Operation {

    public Subtract(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "-";
    }

    @Override
    protected int toOperation(int x, int y) {
        return x - y;
    }
}
