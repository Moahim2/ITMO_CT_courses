import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;
public class mergeSort {
    static int[] merge(int[] a, int[] b) {
        int[] c = new int[a.length + b.length];
        int i  = 0;
        int j = 0;
        while (i < a.length || j < b.length) {
            if (j == b.length || (i < a.length && a[i] < b[j])) {
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
    public static void main(String[] arg) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine());
            String args = in.readLine();
            String[] spl = new String [n];
            spl = args.split(" ");
            int[] arr = new int [n];
            for (int i = 0; i < n; i++) {
                arr[i] = Integer.parseInt(spl[i]);
            }
            arr = sort(arr);
            for (int i = 0; i < n; i++) {
                System.out.print(arr[i]);
                if (i < n - 1) {
                    System.out.print(" ");
                }
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}