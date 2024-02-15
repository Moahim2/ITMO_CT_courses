package expression.exceptions;

import expression.Operation;
import expression.TripleExpression;

public class CheckedMultiply extends Operation {
    public CheckedMultiply(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "*";
    }

    @Override
    protected int toOperation(int x, int y) {
        if ((x < 0 && y == Integer.MIN_VALUE) || (y < 0 && x == Integer.MIN_VALUE)) {
            throw new ArithmeticException("Overflow");
        }
        if (x > 0) {
            if (Integer.MAX_VALUE / x < y || Integer.MIN_VALUE / x > y) {
                throw new ArithmeticException("Overflow");
            }
        } else if (x < 0) {
            if (x != -1 && (Integer.MAX_VALUE / x > y || Integer.MIN_VALUE / x < y)) {
                throw new ArithmeticException("Overflow");
            }
        }
        return x * y;
    }
}
