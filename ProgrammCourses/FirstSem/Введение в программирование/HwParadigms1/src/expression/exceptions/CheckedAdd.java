package expression.exceptions;

import expression.Operation;
import expression.TripleExpression;

public class CheckedAdd extends Operation {

    public CheckedAdd(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "+";
    }

    @Override
    protected int toOperation(int x, int y) {
        if (x >= 0) {
            if (Integer.MAX_VALUE - x < y) {
                throw new ArithmeticException("Overflow");
            }
        } else if (x == Integer.MIN_VALUE) {
            if (y < 0) {
                throw new ArithmeticException("Overflow");
            }
        } else {
            if (Integer.MIN_VALUE - x > y) {
                throw new ArithmeticException("Overflow");
            }
        }
        return x + y;
    }
}
