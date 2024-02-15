package expression.exceptions;

import expression.Operation;
import expression.TripleExpression;

public class CheckedDivide extends Operation {
    public CheckedDivide(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "/";
    }

    @Override
    protected int toOperation(int x, int y) {
        if (x == Integer.MIN_VALUE && y == -1) {
            throw new ArithmeticException("Overflow");
        }
        if (y == 0) {
            throw new ArithmeticException("Division by zero");
        }
        return x / y;
    }
}
