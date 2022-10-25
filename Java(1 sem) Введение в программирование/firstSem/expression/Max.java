package expression;

public class Max extends Operation{

    public Max(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "max";
    }

    @Override
    protected int toOperation(int x, int y) {
        return Math.max(x, y);
    }
}

