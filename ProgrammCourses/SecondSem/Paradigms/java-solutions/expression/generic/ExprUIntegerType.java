package expression.generic;

public class ExprUIntegerType extends GenericExprType<Integer> {

    public ExprUIntegerType(Integer val) {
        super(val);
    }

    @Override
    public <T extends GenericExprType<Integer>> void add(T y) throws ArithmeticException {
        val += y.val;
    }

    @Override
    public <T extends GenericExprType<Integer>> void sub(T y) {
        val -= y.val;
    }

    @Override
    public <T extends GenericExprType<Integer>> void mul(T y) {
        val *= y.val;
    }

    @Override
    public <T extends GenericExprType<Integer>> void div(T y) {
        if (val == Integer.MIN_VALUE && y.val == -1) {
            throw new ArithmeticException("Overflow");
        }
        if (y.val == 0) {
            throw new ArithmeticException("Division by zero");
        }
        val /= y.val;
    }

    @Override
    protected Integer val() {
        return val;
    }

    public void neg() {
        val = -val;
    }

    @Override
    public String toString() {
        return Integer.toString(val);
    }

    @Override
    public void constant(Object val) {
        this.val = (Integer) val;
    }

    @Override
    public void addInt(int x) {
        val += x;
    }

    @Override
    public <T extends GenericExprType<Integer>> void max(T a) {
        val = Math.max(val, a.val);
    }

    @Override
    public <T extends GenericExprType<Integer>> void min(T a) {
        val = Math.min(val, a.val);
    }

    @Override
    public void count() {
        val = Integer.bitCount(val);
    }





    //Unused in HW5
    @Override
    public void l0() {
        String binaryNum = Integer.toBinaryString(val);
        if (binaryNum.equals("0")) {
            val = 32;
        } else {
            val = 32 - binaryNum.length();
        }
    }

    @Override
    public void t0() {
        String binaryNum = Integer.toBinaryString(val);
        int count = 0;
        int i = binaryNum.length() - 1;
        if (binaryNum.equals("0")) {
            val = 32;
        } else {
            while (i >= 0 && count <= binaryNum.length() && binaryNum.charAt(i) == '0') {
                count++;
                i--;
            }
            val = count;
        }
    }
}
