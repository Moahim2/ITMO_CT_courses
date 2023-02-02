import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class E2 {
    static boolean ok(long n, long a, long b, long w, long h, long d) {
        boolean k = false;
        if ((w / (a + 2 * d)) * (h / (b + 2 * d)) >= n) {
            k = true;
        } else if ((w / (b + 2 * d)) * (h / (a + 2 * d)) >= n) {
            k = true;
        }
        return k;
    }

    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String str1 = in.readLine();
            String[] str2 = str1.split(" ");
            long n = Long.parseLong(str2[0]);
            long a = Long.parseLong(str2[1]);
            long b = Long.parseLong(str2[2]);
            long w = Long.parseLong(str2[3]);
            long h = Long.parseLong(str2[4]);
            long l = 0; 
            long r = w + 1;
            while (r > l + 1) {
                long d = (l + r) / 2;
                if (ok(n, a, b, w, h, d)) {
                    l = d;
                } else {
                    r = d;
                }
            }
            if (r - 1 < 1) {
                System.out.print(0);
            } else {
                System.out.print(r - 1);
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}