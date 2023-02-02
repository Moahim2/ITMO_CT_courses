package expression;

import java.util.Objects;

public abstract class Operation implements TripleExpression, Expression {
    protected TripleExpression a, b;
    protected String typeOperation;

    public Operation(TripleExpression a, TripleExpression b) {
        this.a = a;
        this.b = b;
    }

    protected abstract int toOperation (int x, int y);

    @Override
    public int evaluate(int c1, int c2, int c3) {
        int x = a.evaluate(c1, c2, c3);
        int y = b.evaluate(c1, c2, c3);
        return toOperation(x, y);
    }
    public int evaluate(int c1) {
        int x = a.evaluate(c1, c1, c1);
        int y = b.evaluate(c1, c1, c1);
        return toOperation(x, y);
    }

    @Override
    public String toString() {
        return ("(" + a.toString() + " " + typeOperation + " " + b.toString() + ")");
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Operation) {
            final Operation operation1 = (Operation) obj;
            return (Objects.equals(a, operation1.a) && Objects.equals(b, operation1.b) &&
                    Objects.equals(typeOperation, operation1.typeOperation));
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(a, b, typeOperation);
    }
}
