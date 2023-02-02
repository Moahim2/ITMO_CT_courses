import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
public class ClassesPost {
    static int pow(int a, int k) {
        if (k == 1) {
            return a;
        } else if (k == 0) {
            return 1;
        } else {
            return a * pow(a, k - 1);
        }
    }
    public static void main(String[] arg) {
        try {
            int F0 = 1;
            int F1 = 1;
            int M = 1;
            int S = 1;
            int L = 1;
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine());
            String[] args = new String[n];
            for (int i = 0; i < n; i++) {
                args[i] = in.readLine();
            }
            String[][] tabl = new String[n][2];
            for (int i = 0; i < n; i++) {
                String[] spl = args[i].split(" ");
                tabl[i][0] = spl[0];
                tabl[i][1] = spl[1];
            }
            for (int i = 0; i < n; i++) { //F0
                if (tabl[i][1].charAt(0) == '1') {
                    F0 = 0;
                    break;
                }
            }
            for (int i = 0; i < n; i++) { //F1
                if ((tabl[i][1].charAt(tabl[i][1].length() - 1) == '0')) {
                    F1 = 0;
                    break;
                }
            }
            for (int i = 0; i < n; i++) { //S
                int m = pow(2, Integer.parseInt(tabl[i][0]));
                for (int j = 0; j < m / 2; j++) {
                    if (tabl[i][1].charAt(j) == tabl[i][1].charAt(m - 1 - j)) {
                        S = 0;
                        break;  
                    }
                }
                if (Integer.parseInt(tabl[i][0]) == 0) {
                    if (tabl[i][1].charAt(0) == '0') {
                        S = 0;
                    }
                }
                if (S == 0) {
                    break;
                }
            }
            
            for (int i = 0; i < n; i++) { //L
                int m = pow(2, Integer.parseInt(tabl[i][0]));
                String[][] poly = new String[m][m];
                for (int j = 0; j < m; j++) {
                    poly[j][0] = Character.toString(tabl[i][1].charAt(j));
                }
                int k = m - 1;
                for (int j = 1; j < m; j++) {
                    for (int z = 0; z < k; z++) {
                        poly[z][j] = Integer.toString((Integer.parseInt(poly[z][j - 1]) + Integer.parseInt(poly[z + 1][j - 1])) % 2);
                    }
                    k--;
                }
                for (int j = 1; j < m; j++) {
                    if ((m % j) != 0 && Integer.parseInt(poly[0][j]) == 1) {
                        L = 0;
                        break;
                    }
                }
                if (L == 0) {
                    break;
                }
            }
            for (int i = 0; i < n; i++) { //M
                int m = pow(2, Integer.parseInt(tabl[i][0]));
                for (int j = 0; j < m; j++) {
                    if (tabl[i][1].charAt(j) == '1') {
                        String bin = Integer.toBinaryString(j);
                        int max = Integer.parseInt(tabl[i][0]) - 1;
                        StringBuilder zeroes = new StringBuilder();
                        for (int z = 0; z < max + 1 - bin.length(); z++) {
                            zeroes.append('0');
                        }
                        bin = zeroes.toString() + bin;
                        for (int z = 0; z < max + 1; z++) {
                            if (bin.charAt(z) == '0') {
                                if (tabl[i][1].charAt(j + (pow(2, max - z))) == '0') {
                                    M = 0;
                                }
                            }
                            if (M == 0) {
                                break;
                            }
                        }
                        if (M == 0) {
                            break;
                        }
                    }
                    if (M == 0) {
                        break;
                    }
                }
            }
            if (F0 == 0 && F1 == 0 && S == 0 && L == 0 && M == 0) {
                System.out.println("YES");
            } else {
                System.out.println("NO");
            }

        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}