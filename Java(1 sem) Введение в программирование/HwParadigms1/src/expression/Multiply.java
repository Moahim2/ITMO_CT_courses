package expression;

public class Multiply extends Operation {
    public Multiply(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "*";
    }

    @Override
    protected int toOperation(int x, int y) {
        return x * y;
    }
}
