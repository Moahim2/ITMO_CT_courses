package expression;

public class Add extends Operation {

    public Add(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "+";
    }

    @Override
    protected int toOperation(int x, int y) {
        return x + y;
    }
}
