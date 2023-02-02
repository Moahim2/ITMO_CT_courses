import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class D2 {
    static long[] merge(long[] a, long[] b) {
        long[] c = new long[a.length + b.length];
        int i  = 0;
        int j = 0;
        while (i < a.length || j < b.length) {
            if (j == b.length || (i < a.length && a[i] <= b[j])) {
                c[i + j] = a[i++];
            } else {
                c[i + j] = b[j++];
            }
        }
        return c;
    }
    static long[] sort(long[] arr) {
        if (arr.length <= 1) {
            return arr;
        }
        int n = arr.length;
        long[] l = Arrays.copyOf(arr, n / 2);
        long[] r = Arrays.copyOfRange(arr, n / 2, n);
        l = sort(l);
        r = sort(r);
        return merge(l, r);
    }

    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String str1 = in.readLine();
            String[] str2 = str1.split(" ");
            int n = Integer.parseInt(str2[0]);
            int k = Integer.parseInt(str2[1]);
            long[] tabl = new long[n];
            for (int i = 0; i < n; i++) {
                tabl[i] = Long.parseLong(in.readLine());
            }
            tabl = sort(tabl);
            long l = 0;
            long r = tabl[n - 1] + 1;
            while (r > l + 1) {
                long m = (l + r) / 2;
                long count = 0;
                for (int i = 0; i < n; i++) {
                    count += (tabl[i] / m);
                }
                if (count < k) {
                    r = m;
                } else {
                    l = m;
                }
            }
            if (r - 1 >= 1) {
                System.out.print(r - 1);
            } else {
                System.out.print(0);
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}