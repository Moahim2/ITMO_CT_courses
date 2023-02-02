import java.math.BigInteger;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class H {
    
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
        int n = Integer.parseInt(in.readLine());
        String str = in.readLine();
        String number = in.readLine();
        int h = 0;
        while (number.charAt(h) == '0') {
            if (h == number.length() - 1) {
                break;
            }
            h++;
        }
        int q = number.length();
        BigInteger Q = new BigInteger("1");
        for (int i = 0; i < q; i++) {
            Q = Q.multiply(new BigInteger("2"));
        }
        number = number.substring(h);
        BigInteger N = new BigInteger(number, 2);
        Frac M = new Frac(N, Q);
        in.close();
        String[] strS = str.split(" ");
        ArrayList<BigInteger> frec = new ArrayList<>();
        int f = 0;
        for (int i = 0; i < strS.length; i++) {
            f += Integer.parseInt(strS[i]);
            frec.add(new BigInteger(strS[i]));
        }
        ArrayList<Frac> input = new ArrayList<>();
        for (int i = 0; i < frec.size(); i++) {
            input.add(new Frac(frec.get(i), new BigInteger(Integer.toString(f))));
        } 

        ArrayList<String> alhabet = new ArrayList<>(List.of("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
        "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")); 
        ArrayList<String> alh = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            alh.add(alhabet.get(i));
        } 
        Frac[] inp = new Frac[input.size()];
        for (int i = 0; i < input.size(); i++) {
            inp[i] = new Frac(BigInteger.ONE, BigInteger.ONE);
        }
        for (int i = 0; i < input.size(); i++) {
            inp[i] = input.get(i);
        }
        String answer = "";
        Line line = new Line(new Frac(BigInteger.ZERO, BigInteger.ONE), new Frac(BigInteger.ONE, BigInteger.ONE), inp);
        for (int i = 0; i < f; i++) {
            int s = 0;
            for (int j = 0; j < line.e.length - 1; j++) {
                //System.out.println(line.e[j].m.toString(10));
                //System.out.println(diff(M, line.e.get(j)).m.signum() + " " + diff(line.e.get(j + 1), M).m.signum());
                if (diff(M, line.e[j]).m.signum() >= 0 && diff(line.e[j + 1], M).m.signum() > 0) {
                    // BigInteger a = line.e[j];
                    // BigInteger b = line.e[j + 1];
                    line = new Line(line.e[j], line.e[j + 1], inp);
                    answer += alh.get(s);
                    break;
                }
                s++;
            }
        }
        System.out.println(answer);
    }    
}
