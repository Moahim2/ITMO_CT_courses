import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
public class BinaryRelations {
    public static void main(String[] arg) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine());
            String[] args = new String[(2 * n) + 1];
            for (int i = 1; i <= 2 * n; i++) {
                args[i] = in.readLine();
            }
            int r1 = 1;
            int r2 = 1;
            int ar1 = 1;
            int ar2 = 1;
            int s1 = 1;
            int s2 = 1;
            int as1 = 1;
            int as2 = 1;
            int tr1 = 1;
            int tr2 = 1;
            char[][] func1 = new char[n][n];
            char[][] func2 = new char[n][n];
            for (int i = 1; i <= n; i++) {
                char[] chars = args[i].toCharArray();
                for (int j = 0; j < 2 * n - 1; j += 2) {
                    func1[i - 1][j / 2] = chars[j];
                }
            }
            for (int i = n + 1; i <= 2 * n; i++) {
                char[] chars = args[i].toCharArray();
                for (int j = 0; j < 2 * n - 1; j += 2) {
                    func2[i - n - 1][j / 2] = chars[j];
                }
            }
            for (int i = 0; i < n; i++) { //1
                if (func1[i][i] == '0') {
                    r1 = 0;
                }
            }
            for (int i = 0; i < n; i++) { //2
                if (func2[i][i] == '0') {
                    r2 = 0;
                    break;
                }
            }
            for (int i = 0; i < n; i++) { //1
                if (func1[i][i] == '1') {
                    ar1 = 0;
                    break;
                }
            }
            for (int i = 0; i < n; i++) { //2
                if (func2[i][i] == '1') {
                    ar2 = 0;
                    break;
                }
            }
            for (int i = 0; i < n; i++) { //1
                for (int j = 0; j < n; j++) {
                    if (func1[i][j] != func1[j][i] && func1[i][j] == '1') {
                        s1 = 0;
                        break;
                    }
                }
            }
            for (int i = 0; i < n; i++) { //2
                for (int j = 0; j < n; j++) {
                    if (func2[i][j] != func2[j][i] && func2[i][j] == '1') {
                        s2 = 0;
                        break;
                    }
                }
            }
            for (int i = 0; i < n; i++) { //1
                for (int j = 0; j < n; j++) {
                    if (func1[i][j] == func1[j][i] && func1[i][j] == '1' && i != j) {
                        as1 = 0;
                        break;
                    }
                }
            }
            for (int i = 0; i < n; i++) { //2
                for (int j = 0; j < n; j++) {
                    if (func2[i][j] == func2[j][i] && func2[i][j] == '1' && i != j) {
                        as2 = 0;
                        break;
                    }
                }
            }
            for (int i = 0; i < n; i++) { //1
                for (int j = 0; j < n; j++) {
                    for (int k = 0; k < n; k++) {
                        if (func1[i][j] == '1' && func1[j][k] == '1' && func1[i][k] == '0') {
                            tr1 = 0;
                            break;
                        }
                    }
                }
            }
            for (int i = 0; i < n; i++) { //2
                for (int j = 0; j < n; j++) {
                    for (int k = 0; k < n; k++) {
                        if (func2[i][j] == '1' && func2[j][k] == '1' && func2[i][k] == '0') {
                            tr2 = 0;
                            break;
                        }
                    }
                }
            }
            int[][] relat = new int[n][n];
            for (int i = 0; i < n; i++) { //0
                for (int j = 0; j < n; j++) {
                    boolean x = false;
                    for (int k = 0; k < n; k++) {
                        if (func1[i][k] == '1' && func2[k][j] == '1') {
                            x = true;
                            break;
                        }
                    }
                    if (x) {
                        relat[i][j] = 1;
                    } else {
                        relat[i][j] = 0;
                    }
                }
            }
            System.out.println(Integer.toString(r1) + " " + Integer.toString(ar1) + " " + Integer.toString(s1) + " " + Integer.toString(as1) + " " + Integer.toString(tr1));
            System.out.println(Integer.toString(r2) + " " + Integer.toString(ar2) + " " + Integer.toString(s2) + " " + Integer.toString(as2) + " " + Integer.toString(tr2));
            for (int i = 0; i < n; i++) {
                for(int j = 0; j < n; j++) {
                    System.out.print(relat[i][j] + " ");
                }
                if (i != n - 1) {
                    System.out.println();
                }
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}