package expression.generic;

public class GenT0<T extends GenericExprType<I>, I> extends GenUnaryOperation<T, I> {

    public GenT0(Exp<T, I> expression) {
        super(expression);
        typeOperation = "t0";
    }

    @Override
    protected T toOperation(T x) {
        x.t0();
        return x;
    }
}