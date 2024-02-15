import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class D1 {
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
            int n = Integer.parseInt(in.readLine());
            String[] inputstr = new String[n];
            for (int i = 0; i < n; i++) {
                inputstr[i] = in.readLine();
            }
            long[][] tabl = new long[n][2];
            long minA = 0;
            long maxB = 0;
            for (int i = 0; i < n; i++) {
                String[] spl = new String[2];
                spl = inputstr[i].split(" ");
                tabl[i][0] = Integer.parseInt(spl[0]);
                tabl[i][1] = Integer.parseInt(spl[1]);
                if (i == 0) {
                    minA = tabl[i][0];
                }
                if (minA > tabl[i][0]) {
                    minA = tabl[i][0];
                }
                if (maxB < tabl[i][1]) {
                    maxB = tabl[i][1];
                }
            }
            long[] tablb = new long[n];
            for (int i = 0; i < n; i++) {
                tablb[i] = tabl[i][1];
            }
            tablb = sort(tablb);
            long[] tabla = new long[n];
            for (int i = 0; i < n; i++) {
                tabla[i] = tabl[i][0];
            }
            tabla = sort(tabla);
            int[] tablDis = new int[n]; // показывает номер первой ашки в tabla такой что она больше 
            for (int i = 0; i < n; i++) {
                int l = -1;
                int r = n;
                long x = tablb[i];   
                while(r > l + 1) {
                    int m = (l + r) / 2;
                    if (tabla[m] >= x) {
                        r = m;
                    } else {
                        l = m;
                    }
                }
                tablDis[i] = r;
            }
            long[] tablSum = new long[n];
            long sum = 0;
            for (int i = 0; i < n; i++) {
                sum += tabla[i];
                tablSum[i] = sum;
            }
            long[] tablDisnumber = new long[n];
            for (int i = 0; i < n; i++) {
                if (tablDis[i] < n) {
                    tablDisnumber[i] = tablSum[n - 1] - tablSum[tablDis[i] - 1];
                } else {
                    tablDisnumber[i] = 0;
                }
            }
            long S = 0;
            long R = 0;
            for (int i = 0; i < n; i++) {
                long s = tablDisnumber[i];
                if (tablDis[i] < n) {
                    s += (n - i - (n - tablDis[i])) * tablb[i];
                } else {
                    s += (n - i) * tablb[i];
                }
                if (s > S) {
                    S = s;
                    R = tablb[i];
                }   
            }
            //System.out.println(Arrays.toString(tablDis) + " " + Arrays.toString(tablSum) + " " + Arrays.toString(tablDisnumber));
            System.out.print(Long.toString(R) + " " + Long.toString(S));
        } catch (IOException e) {   
            System.out.print(e.getMessage());
        }
    }
}