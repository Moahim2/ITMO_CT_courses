import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class A2 {

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

    static int binarySearch(int[] arr, int L, int R) {
        int x = 0;
        int l = -1;
        int r = arr.length;
        while (r > l + 1) {
            int m = (l + r) / 2;
            if (arr[m] > R) {
                r = m;
            } else {
                l = m;
            }
        }
        int maxIndex = r;
        //System.out.println(maxIndex);
        l = 0;
        r = arr.length;
        while (r > l + 1) {
            int m = (l + r) / 2;
            if (arr[m] < L) {
                l = m;
            } else {
                r = m;
            } 
        }
        int minIndex = l;
        if (minIndex >= 0) {
            if (arr[minIndex] < L) {
                minIndex++;
            }
        } else {
            minIndex++;
        }
        if (minIndex < arr.length && minIndex >= 1) {
            while (arr[minIndex] == arr[minIndex - 1] && arr[minIndex] == L) {
                minIndex--;
                if (minIndex < 1) {
                    break;
                }
            }
        }
        if (maxIndex < arr.length) {
            if (arr[maxIndex] > R) {
                maxIndex--;
            }
        } else {
            maxIndex--;
        }
        if (maxIndex < arr.length - 1 && maxIndex >= 0) {
              while (arr[maxIndex] == arr[maxIndex + 1] && arr[maxIndex] == R) {
                maxIndex++;
                if (maxIndex + 1 >= arr.length) {
                    break;
                }
            }
        }
        x = maxIndex - minIndex + 1;
        /*for (int i = minIndex; i < maxIndex; i++) {
            if (arr[i] <= R && arr[i] >= L) { 
                x++;
            }
        }*/

        /*if (arr[minIndex] < L) {
            minIndex++;
        }
        if (minIndex > 0 && minIndex < arr.length) {
            while (arr[minIndex] == arr[minIndex - 1]) {
                minIndex--;
                if (minIndex - 1 > 0) {
                    break;
                }
            }
        }
        if (maxIndex > arr.length - 1) {
            maxIndex--;
        }
        if (arr[maxIndex] > R) {
            maxIndex--;
        }
        if (maxIndex < arr.length - 1 && maxIndex >= 0) {
            while (arr[maxIndex] == arr[maxIndex + 1]) {
                maxIndex++;
                if (maxIndex + 1 <= arr.length) {
                    break;
                }
            }
        }
        while (minIndex <= maxIndex) {
            minIndex++;
            x++;
        }*/
        return x;
    }
    
    public static void main(String[] args) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine());
            String arg = in.readLine();
            String[] spl = new String [n];
            spl = arg.split(" ");
            int[] arr = new int[n];
            for (int i = 0; i < n; i++) {
                arr[i] = Integer.parseInt(spl[i]);
            }
            arr = sort(arr);
            int k = Integer.parseInt(in.readLine());
            String[] inputstr = new String[k];
            for (int i = 0; i < k; i++) {
                inputstr[i] = in.readLine();
            }
            int[][] tabl = new int[k][2];
            for (int i = 0; i < k; i++) {
                String[] splK = new String[2];
                splK = inputstr[i].split(" ");
                tabl[i][0] = Integer.parseInt(splK[0]);
                tabl[i][1] = Integer.parseInt(splK[1]);
            }
            for (int i = 0; i < k; i++) {
                System.out.print(binarySearch(arr, tabl[i][0], tabl[i][1]));
                if (i < k - 1) {
                    System.out.print(" ");
                }
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}