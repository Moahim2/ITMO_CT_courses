package expression.generic;

public class GenMax<T extends GenericExprType<I>, I> extends GenBinaryOperation<T, I> {

    public GenMax(Exp<T, I> a, Exp<T, I> b) {
        super(a, b);
        typeOperation = "max";
    }

    @Override
    protected T toOperation(T x, T y) {
        x.max(y);
        return x;
    }
}

