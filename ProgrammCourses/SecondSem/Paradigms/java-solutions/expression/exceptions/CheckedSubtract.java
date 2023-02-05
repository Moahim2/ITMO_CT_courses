package expression.exceptions;

import expression.Operation;
import expression.TripleExpression;

public class CheckedSubtract extends Operation {

    public CheckedSubtract(TripleExpression a, TripleExpression b) {
        super(a, b);
        typeOperation = "-";
    }

    @Override
    protected int toOperation(int x, int y) {
        if (x == Integer.MIN_VALUE && y > 0) {
            throw new ArithmeticException("Overflow");
        }
        if (y == Integer.MIN_VALUE && x > -1) {
            throw new ArithmeticException("Overflow");
        }
        if (x >= 0) {
            if (Integer.MAX_VALUE - x < -y) {
                throw new ArithmeticException("Overflow");
            }
        } else if (x != Integer.MIN_VALUE && y != Integer.MIN_VALUE && Integer.MIN_VALUE - x > -y) {
            throw new ArithmeticException("Overflow");
        }
        return x - y;
    }
}
