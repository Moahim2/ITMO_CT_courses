package expression.generic;

//import java.math.BigInteger;
//import java.util.function.Function;

public class Main {

    public static void main(String[] args) throws ParsingException {
        int[] values = {-2, 2, -2, 2, -2, 2};
        Object[][][] tab = new GenericTabulator().tabulate(args[0], args[1], values[0], values[1],
                values[2], values[3], values[4], values[5]);
        for (int i = 0; i <= values[1] - values[0]; i++) {
            for (int j = 0; j <= values[3] - values[2]; j++) {
                for (int k = 0; k <= values[5] - values[4]; k++) {
                        try {
                            System.out.println("i = " + (i + values[0]) + ", j = " +
                                    (j + values[2]) + ", k = " + (k + values[4]) + " VALUE == " + tab[i][j][k]);
                        } catch (NullPointerException e) {
                            System.out.println("i = " + (i + values[0]) + ", j = " +
                                    (j + values[2]) + ", k = " + (k + values[4]) + " VALUE == null");
                        }
                }
            }
        }
    }
}
