package expression;

public class Divide extends Operation {
    public Divide(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "/";
    }

    @Override
    protected int toOperation(int x, int y) {
        return x / y;
    }
}
