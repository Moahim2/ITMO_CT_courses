import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
public class Polynom {
    public static void main(String[] arg) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine());
            int m = (int) Math.pow(2, n);
            String[] args = new String[m];
            for (int i = 0; i < m; i++) {
                args[i] = in.readLine();
            }
            String[][] tabl = new String[m][2];
            for (int i = 0; i < m; i++) {
                String[] spl = args[i].split(" ");
                tabl[i][0] = spl[0];
                tabl[i][1] = spl[1];
            }
            String[][] poly = new String[m][m];
            for (int i = 0; i < m; i++) {
                poly[i][0] = tabl[i][1];
            }
            int k = m - 1;
            for (int i = 1; i < m; i++) {
                for (int j = 0; j < k; j++) {
                    poly[j][i] = Integer.toString((Integer.parseInt(poly[j][i - 1]) + Integer.parseInt(poly[j + 1][i - 1])) % 2);
                }
                k--;
            }
            for (int i = 0; i < m; i++) {
                System.out.println(tabl[i][0] + " " + poly[0][i]);
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}