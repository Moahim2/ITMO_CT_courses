import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class C2 {
    static int[] merge(int[] a, int[] b) {
        int[] c = new int[a.length + b.length];
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
    static int[] sort(int[] arr) {
        if (arr.length <= 1) {
            return arr;
        }
        int n = arr.length;
        int[] l = Arrays.copyOf(arr, n / 2);
        int[] r = Arrays.copyOfRange(arr, n / 2, n);
        l = sort(l);
        r = sort(r);
        return merge(l, r);
    }
    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String str1 = in.readLine(); // считывание
            String[] str2 = str1.split(" ");
            int n = Integer.parseInt(str2[0]);
            int k = Integer.parseInt(str2[1]);
            String str3 = in.readLine();
            String[] str4 = str3.split(" ");
            int[] tabl = new int[n];
            for (int i = 0; i < n; i++) {
                tabl[i] = Integer.parseInt(str4[i]); //
            }
            tabl = sort(tabl);
            double l = -1;
            double r = k + 0.000001;
            while (r > l + 0.000001) {
               double m = (r + l) / 2; 
               double lastEdge = 0;
               boolean a = true;
               for (int i = 0; i < n; i++) {
                   if (i < n - 1) {
                       if (tabl[i] - lastEdge > m) {
                            a = false;
                            break;
                       } else {
                            double x = m - (tabl[i] - lastEdge);
                            if (x >= tabl[i + 1] - tabl[i]) {
                                lastEdge = tabl[i + 1];
                            } else {
                                lastEdge = tabl[i] + x;
                            }
                       }
                   } else {
                    if (tabl[i] - lastEdge + k - tabl[i] > m) {
                        a = false;
                        break;
                    }
                   }
               }
               if (a) {
                   r = m;
               } else {
                   l = m;
               }
            }
            System.out.print(r);
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}