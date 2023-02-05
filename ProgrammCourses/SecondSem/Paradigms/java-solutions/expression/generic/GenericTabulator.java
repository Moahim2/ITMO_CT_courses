package expression.generic;

import java.math.BigInteger;
import java.util.function.Function;

public class GenericTabulator implements Tabulator {
    private int x1, x2, y1, y2, z1, z2;

    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1,
                                 int y2, int z1, int z2) throws ParsingException {
        this.x1 = x1;
        this.x2 = x2;
        this.y1 = y1;
        this.y2 = y2;
        this.z1 = z1;
        this.z2 = z2;
        Object[][][] ans = new Object[x2 + 1 - x1][y2 + 1 - y1][z2 + 1 - z1];
        switch (mode) {
            case "i" -> tabulateImpl(ans, new ExpressionParserWithMods<ExprIntegerType, Integer>().
                                    parse(expression, fSToInt), fI,
                                    new ExprIntegerType(x1),
                                    new ExprIntegerType(y1),
                                    new ExprIntegerType(z1));
            case "d" -> tabulateImpl(ans, new ExpressionParserWithMods<ExprDoubleType, Double>().
                                    parse(expression, fSToDouble), fD,
                                    new ExprDoubleType((double) x1),
                                    new ExprDoubleType((double) y1),
                                    new ExprDoubleType((double) z1));
            case "bi" -> tabulateImpl(ans, new ExpressionParserWithMods<ExprBigIntegerType, BigInteger>().
                                    parse(expression, fSToBigInt), fBI,
                                    new ExprBigIntegerType(new BigInteger(Integer.toString(x1))),
                                    new ExprBigIntegerType(new BigInteger(Integer.toString(y1))),
                                    new ExprBigIntegerType(new BigInteger(Integer.toString(z1))));
            case "u" -> tabulateImpl(ans, new ExpressionParserWithMods<ExprUIntegerType, Integer>().
                                    parse(expression, fSToUInt), fI,
                                    new ExprUIntegerType(x1),
                                    new ExprUIntegerType(y1),
                                    new ExprUIntegerType(z1));
            case "l" -> tabulateImpl(ans, new ExpressionParserWithMods<ExprLongType, Long>().
                                    parse(expression, fSToLong), fL,
                                    new ExprLongType((long) x1),
                                    new ExprLongType((long) y1),
                                    new ExprLongType((long) z1));
            case "s" -> tabulateImpl(ans, new ExpressionParserWithMods<ExprShortType, Short>().
                                    parse(expression, fSToShort), fS,
                                    new ExprShortType((short) x1),
                                    new ExprShortType((short) y1),
                                    new ExprShortType((short) z1));
            //+
            default -> throw new IllegalArgumentException("Incorrectly mode value");
        }
        return ans;
    }


     private <T extends GenericExprType<I>, I> void tabulateImpl(Object[][][] ans, Exp<T, I> exp, Function<Integer, I> f,
                                                                 T x, T y, T z) {
        I a = f.apply(x1);
        I b = f.apply(y1);
        I c = f.apply(z1);
        for (int i = 0; i <= x2 - x1; i++) {
            x.constant(a);
            x.addInt(i);
            for (int j = 0; j <= y2 - y1; j++) {
                y.constant(b);
                y.addInt(j);
                for (int k = 0; k <= z2 - z1; k++) {
                    z.constant(c);
                    z.addInt(k);
                    try {
                        ans[i][j][k] = exp.evaluate(x, y, z);
                    } catch (ArithmeticException e) {
                        ans[i][j][k] = null;
                    }
                }
            }
        }
    }


//+     //Ф-ции для создания новых экземпляров наследников GenericExprType внутри парсера
    Function<String, ExprIntegerType> fSToInt =
            (String value) -> new ExprIntegerType(Integer.parseInt(value));
    Function<String, ExprDoubleType> fSToDouble =
            (String value) -> new ExprDoubleType(Double.parseDouble(value));
    Function<String, ExprBigIntegerType> fSToBigInt =
            (String value) -> new ExprBigIntegerType(new BigInteger(value));
    Function<String, ExprUIntegerType> fSToUInt =
            (String value) -> new ExprUIntegerType(Integer.parseInt(value));
    Function<String, ExprLongType> fSToLong =
            (String value) -> new ExprLongType(Long.parseLong(value));
    Function<String, ExprShortType> fSToShort =
            (String value) -> new ExprShortType((short) Integer.parseInt(value));
    //+


//+ //Ф-ции для удобства конверта в tabulateImpl
    Function<Integer, Integer> fI = (Integer value) -> value;
    Function<Integer, Double> fD = (Integer value) -> (double) value;
    Function<Integer, BigInteger> fBI = (Integer value) -> new BigInteger(Integer.toString(value));
    Function<Integer, Long> fL = (Integer value) -> (long) value;
    Function<Integer, Short> fS = (Integer value) -> (short)((int) value);
}