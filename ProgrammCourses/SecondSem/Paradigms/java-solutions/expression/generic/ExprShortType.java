package expression.generic;


public class ExprShortType extends GenericExprType<Short> {

    public ExprShortType(short val) {
        super(val);
    }

    @Override
    public <T extends GenericExprType<Short>> void add(T y) {
        val = (short) (val + y.val);
    }

    @Override
    public <T extends GenericExprType<Short>> void sub(T y) {
        val = (short) (val - y.val);
    }

    @Override
    public <T extends GenericExprType<Short>> void mul(T y) {
        val = (short) (val * y.val);
    }

    @Override
    public <T extends GenericExprType<Short>> void div(T y) {
        if (val == Short.MIN_VALUE && y.val == -1) {
            throw new ArithmeticException("Overflow");
        }
        if (y.val == 0) {
            throw new ArithmeticException("Division by zero");
        }
        val = (short) (val / y.val);
    }

    @Override
    protected Short val() {
        return val;
    }

    public void neg() {
        val = (short) -val;
    }

    @Override
    public String toString() {
        return Short.toString(val);
    }

    @Override
    public void constant(Object val) {
        this.val = (Short) val;
    }

    @Override
    public void addInt(int x) {
        val = (short) (val + x);
    }

    //Unsupported
    @Override
    public <T extends GenericExprType<Short>> void max(T a) {
        val = (short) Math.max(val, a.val);
    }

    @Override
    public <T extends GenericExprType<Short>> void min(T a) {
        val = (short) Math.min(val, a.val);
    }

    @Override
    public void count() {
        val = (short) Integer.bitCount(Short.toUnsignedInt(val));
    }


    //Unused in HW5
    @Override
    public void l0() {
        throw new UnsupportedOperationException("l0");
    }

    @Override
    public void t0() {
        throw new UnsupportedOperationException("t0");
    }
}

