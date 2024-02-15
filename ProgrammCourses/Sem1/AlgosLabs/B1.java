import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;
public class B1 {
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
            String str1 = in.readLine();
            String[] str2 = str1.split(" ");
            int r = Integer.parseInt(str2[0]);
            int c = Integer.parseInt(str2[1]);
            int[] tabl = new int[r * c];
            for (int i = 0; i < r * c; i++) {
                tabl[i] = Integer.parseInt(in.readLine());
            }
            tabl = sort(tabl);
            int max = 0;
            for (int i = 1; i <= r; i++) {
                if (tabl[i * c - 1] - tabl[c * i - c] > max) {
                    max = tabl[i * c - 1] - tabl[c * i - c];
                }
            }
            System.out.print(max);
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}