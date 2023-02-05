package expression.generic;


public class GenL0<T extends GenericExprType<I>, I> extends GenUnaryOperation<T, I> {
    public GenL0(Exp<T, I> expression) {
        super(expression);
        typeOperation = "l0";
    }

    @Override
    protected T toOperation(T x) {
        x.l0();
        return x;
    }
}
