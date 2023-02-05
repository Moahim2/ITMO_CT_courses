package expression.generic;

public class GenCheckedSubtract<T extends GenericExprType<I>, I> extends GenBinaryOperation<T, I> {

    public GenCheckedSubtract(Exp<T, I> a, Exp<T, I> b) {
        super(a, b);
        typeOperation = "-";
    }

    @Override
    protected T toOperation(T x, T y) {
        x.sub(y);
        return x;
    }
}
