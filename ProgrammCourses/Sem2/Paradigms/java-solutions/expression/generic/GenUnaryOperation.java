package expression.generic;

public abstract class GenUnaryOperation<T extends GenericExprType<I>, I> implements Exp<T, I>{
    protected Exp<T, I> expression;
    protected String typeOperation;

    protected GenUnaryOperation(Exp<T, I> expression) {
        this.expression = expression;
    }

    protected abstract T toOperation (T x);

    @Override
    public T evaluateImpl(T a, T b, T c) {
        T x = expression.evaluateImpl(a, b, c);
        return toOperation(x);
    }

    @Override
    public String toString() {
        return typeOperation + "(" + expression.toString() + ")";
    }
}
