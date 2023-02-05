package expression.generic;

public class ExprLongType extends GenericExprType<Long> {

    public ExprLongType(Long val) {
        super(val);
    }

    @Override
    public <T extends GenericExprType<Long>> void add(T y) {
        val += y.val;
    }

    @Override
    public <T extends GenericExprType<Long>> void sub(T y) {
        val -= y.val;
    }

    @Override
    public <T extends GenericExprType<Long>> void mul(T y) {
        val *= y.val;
    }

    @Override
    public <T extends GenericExprType<Long>> void div(T y) {
        if (val == Long.MIN_VALUE && y.val == -1) {
            throw new ArithmeticException("Overflow");
        }
        if (y.val == 0) {
            throw new ArithmeticException("Division by zero");
        }
        val /= y.val;
    }

    @Override
    protected Long val() {
        return val;
    }

    public void neg() {
       val = -val;
    }

    @Override
    public String toString() {
        return Long.toString(val);
    }

    @Override
    public void constant(Object val) {
        this.val = (Long) val;
    }

    @Override
    public void addInt(int x) {
        val += x;
    }

    @Override
    public <T extends GenericExprType<Long>> void max(T a) {
        val = Math.max(val, a.val);
    }

    @Override
    public <T extends GenericExprType<Long>> void min(T a) {
        val = Math.min(val, a.val);
    }


    @Override
    public void count() {
        val = (long) Long.bitCount(val);
    }



    //Unused in HW5
    @Override
    public void l0() {

    }

    @Override
    public void t0() {

    }
}
