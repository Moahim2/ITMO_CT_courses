package expression.generic;

public class GenCheckedAdd<T extends GenericExprType<I>, I> extends GenBinaryOperation<T, I> {
    public GenCheckedAdd(Exp<T, I> a, Exp<T, I> b) {
        super(a, b);
        typeOperation = "+";
    }

    @Override
    protected T toOperation(T x, T y) {
        x.add(y);
        return x;
    }
}
