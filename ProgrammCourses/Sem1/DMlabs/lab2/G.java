import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;

import javax.sound.sampled.SourceDataLine;

import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;
import java.math.*;
import java.math.BigDecimal;


public class G {

    static Frac diff(Frac a, Frac b) {  
        return new Frac(a.m.multiply(b.n).subtract(a.n.multiply(b.m)), a.n.multiply(b.n));
    }

    static Frac mul(Frac a, Frac b) {
        return new Frac(a.m.multiply(b.m), a.n.multiply(b.n));
    }

    static Frac sum(Frac a, Frac b) {
        return new Frac(a.m.multiply(b.n).add(a.n.multiply(b.m)), a.n.multiply(b.n));
    }

    static class Frac {
        public BigInteger m;
        public BigInteger n;
        
        public Frac(BigInteger m, BigInteger n) {
            BigInteger GCD = m.gcd(n);
            this.m = m.divide(GCD);
            this.n = n.divide(GCD);
        }
    }    

    static class Line {
        public Frac l;
        public Frac r;
        public Frac[] e;

        public Line(Frac l, Frac r, Frac[] inp) {
            this.l = l;
            this.r = r;
            Frac R = l;
            e = new Frac[inp.length + 1];
            e[0] = R;
            for (int i = 0; i < inp.length; i++) {
                R = sum(R, mul(diff(r, l), inp[i]));
                e[i + 1] = R; 
            }
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(in.readLine()); //размер алфавита
        String str = in.readLine();
        in.close();
        ArrayList<String> alhabet = new ArrayList<>(List.of("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
        "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")); 
        ArrayList<String> alh = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            alh.add(alhabet.get(i));
        }
        int[] frec = new int[n];
        for (int i = 0; i < str.length(); i++) {
            frec[alh.indexOf(Character.toString(str.charAt(i)))]++;
        }

        System.out.println(n);
        for (int i = 0; i < n; i++) {
            System.out.print(frec[i] + " ");
        }
        System.out.println();


        Frac[] inp = new Frac[n];
        for (int i = 0; i < n; i++) { //инициализация
            inp[i] = new Frac(BigInteger.ONE, BigInteger.ONE);
        }
        for (int i = 0; i < n; i++) { //finish freq
            inp[i] = new Frac(new BigInteger(Integer.toString(frec[i])), new BigInteger(Integer.toString(str.length())));
        }

        Line line = new Line(new Frac(BigInteger.ZERO, BigInteger.ONE), new Frac(BigInteger.ONE, BigInteger.ONE), inp);
        for (int i = 0; i < str.length(); i++) {
            int number = alh.indexOf(Character.toString(str.charAt(i)));
            line = new Line(line.e[number], line.e[number + 1], inp);
        }
        int q = 1;
        BigInteger Q = new BigInteger("2");
        BigInteger P = new BigInteger("1");
        while (true) {
            BigDecimal Y = new BigDecimal(line.l.m);
            Y = Y.divide(new BigDecimal(line.l.n), new MathContext(1000));
            Y = Y.multiply(new BigDecimal(Q), new MathContext(1000));
            P = Y.toBigInteger().add(BigInteger.ONE);
            //P = line.l.m.divide(line.l.n).multiply(Q).add(BigInteger.ONE);
            if(diff(line.r, new Frac(P, Q)).m.signum() > 0) {
                break;
            } else {
                Q = Q.multiply(new BigInteger("2"));
                q++;
            }
        }
        String answer = P.toString(2);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < q - answer.length(); i++) {
            sb.append("0");
        }
        sb.append(answer);
        int count = 0;
        for (int i = 0; i < str.length() - 1; i++) {
            if (str.charAt(i) == str.charAt(i + 1)) {
                count++;
            }
        }
        if (count ==         str.length() - 1) {
            System.out.println(0);
        } else {
            System.out.println(sb.toString());
        }
    }
}
