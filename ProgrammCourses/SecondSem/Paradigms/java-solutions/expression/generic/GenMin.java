package expression.generic;

public class GenMin<T extends GenericExprType<I>, I> extends GenBinaryOperation<T, I> {

    public GenMin(Exp<T, I> a, Exp<T, I> b) {
        super(a, b);
        typeOperation = "min";
    }

    @Override
    protected T toOperation(T x, T y) {
        x.min(y);
        return x;
    }

}
