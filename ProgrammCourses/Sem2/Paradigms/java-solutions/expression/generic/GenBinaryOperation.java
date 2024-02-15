package expression.generic;

public abstract class GenBinaryOperation<T extends GenericExprType<I>, I> implements Exp<T, I>  {
    protected Exp<T, I> a, b;
    protected String typeOperation;

    protected GenBinaryOperation(Exp<T, I> a, Exp<T, I> b) {
        this.a = a;
        this.b = b;
    }

    protected abstract T toOperation (T x, T y);

    public T evaluateImpl(T c1, T c2, T c3) {
        T x = a.evaluateImpl(c1, c2, c3);
        T y = b.evaluateImpl(c1, c2, c3);
        return toOperation(x, y);
    }

    @Override
    public String toString() {
        return ("(" + a.toString() + " " + typeOperation + " " + b.toString() + ")");
    }
}
