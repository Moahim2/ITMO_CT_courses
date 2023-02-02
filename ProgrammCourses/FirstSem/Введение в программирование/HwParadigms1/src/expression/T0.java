package expression;

import java.util.Objects;

public class T0 implements TripleExpression, Expression {
    private final TripleExpression expression;

    public T0(TripleExpression expression) {
        this.expression = expression;
    }

    @Override
    public int evaluate(int c1, int c2, int c3) {
        String binaryNum = Integer.toBinaryString(expression.evaluate(c1, c2, c3));
        int count = 0;
        int i = binaryNum.length() - 1;
        if (binaryNum.equals("0")) {
            return 32;
        }
        while (i >= 0 && count <= binaryNum.length() && binaryNum.charAt(i) == '0') {
            count++;
            i--;
        }
        return count;
    }
    public int evaluate(int c1) {
        String binaryNum = Integer.toBinaryString(expression.evaluate(c1, c1, c1));
        int count = 0;
        int i = binaryNum.length() - 1;
        if (binaryNum.equals("0")) {
            return 32;
        }
        while (i >= 0 && count <= binaryNum.length() && binaryNum.charAt(i) == '0') {
            count++;
            i--;
        }
        return count;
    }

    @Override
    public String toString() {
        return "t0(" + expression.toString() + ")";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof T0) {
            final T0 t0 = (T0) obj;
            return Objects.equals(expression, t0.expression);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(expression);
    }
}
