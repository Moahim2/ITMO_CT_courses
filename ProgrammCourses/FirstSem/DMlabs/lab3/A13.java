import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class A13 {
    public static long number;
    public static long phactor = 1;
    public static long before = 0;
    public static long c;

    public static void createVector(long n, ArrayList<Integer> prefix) {
        if (prefix.size() == n) {
            if (before == number) {
                for(int i = 0; i < n; i++) {
                    System.out.print(prefix.get(i) + " ");
                }
                System.out.println();
                return;
            }
        }
        for (int i = 1; i < n + 1; i++) {
            if (!(prefix.contains(i))) {
                if (before + phactor <= number) {
                    before += phactor;
                    continue;
                } else {
                    prefix.add(i);
                    if (c > 0) {
                        phactor = phactor / c;
                    } else {
                        phactor = 1;
                    }
                    c--;
                    createVector(n, prefix);
                    prefix.remove(prefix.size() - 1);
                    break;
                }
            }
        }
    }
       

    public static void main(String[] args) throws IOException {
        Scanner in = new Scanner(System.in);
        long n = in.nextLong();
        number = in.nextLong();
        in.close();
        for (int i = 1; i <= n - 1; i++) {
            phactor *= i;
        }
        c = n - 1;
        createVector(n, new ArrayList());
    }
}
