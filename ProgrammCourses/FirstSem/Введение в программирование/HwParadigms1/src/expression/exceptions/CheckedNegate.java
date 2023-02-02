package expression.exceptions;

import expression.Expression;
import expression.TripleExpression;

import java.util.Objects;

public class CheckedNegate implements TripleExpression, Expression {
    private final TripleExpression expression;

    public CheckedNegate(TripleExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int c1, int c2, int c3) {
        if (expression.evaluate(c1, c2, c3) == Integer.MIN_VALUE) {
            throw new ArithmeticException("Overflow");
        }
        return -expression.evaluate(c1, c2, c3);
    }
    public int evaluate(int c1) {
        if (expression.evaluate(c1, c1, c1) == Integer.MIN_VALUE) {
            throw new ArithmeticException("Overflow");
        }
        return -expression.evaluate(c1, c1, c1);
    }

    @Override
    public String toString() {
        return "-(" + expression.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof CheckedNegate) {
            final CheckedNegate unaryMinus = (CheckedNegate) obj;
            return Objects.equals(expression, unaryMinus.expression);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression);
    }
}
