import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
public class D {
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
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int m = Integer.parseInt(in.readLine());
            int k = pow(2, m);
            String[] args = new String[k];
            for (int i = 0; i < k; i++) {
                args[i] = in.readLine();
            }

            String[][] tabl = new String[k][2];
            for (int i = 0; i < k; i++) {
                String[] spl = args[i].split(" ");
                for (int j = 0; j < 2; j++) {
                    tabl[i][j] = spl[j];
                }
            }
            int count = 0;
            for (int i = 0; i < k; i++) {
                if (tabl[i][1].equals("1")) {
                    count++;
                }
            }
            if (count == 0) {
                System.out.println(Integer.toString(m + 2));
                System.out.println(Integer.toString(1) + " " + Integer.toString(1));
                System.out.println(Integer.toString(2) + " " + Integer.toString(m + 1) + " " + Integer.toString(1));
            } else {
                int t = 0;
                int[][] tabl1 = new int[count][m]; // табличка едениц
                for (int i = 0; i < k; i++) {
                    if (tabl[i][1].equals("1")) {
                        for (int j = 0; j < m; j++) {
                            tabl1[i - t][j] = Integer.parseInt(Character.toString(tabl[i][0].charAt(j)));
                        }
                    } else {
                        t++;
                    }
                }
                /*for (int i = 0; i < count; i++) {
                    for (int j = 0; j < m; j++) {
                        System.out.print(tabl1[i][j]);
                    }
                    System.out.println();
                }*/
                System.out.println(2 * m + count - 1 + (m - 1) * count);
                t = m + 1;
                for (int i = 0; i < m; i++) {
                    System.out.println(Integer.toString(1) + " " + Integer.toString(i + 1));
                    t++;
                }
                int y = 1;
                for (int i = 0; i < count; i++) {
                    if (tabl1[i][0] == 0) {
                        y = m + 1;
                    }
                    for (int j = 1; j < m; j++) {
                        if (tabl1[i][j] == 0) {
                            System.out.println(Integer.toString(2) + " " + Integer.toString(y) + " " + Integer.toString(m + j + 1));
                        } else {
                            System.out.println(Integer.toString(2) + " " + Integer.toString(y) + " " + Integer.toString(j + 1));
                        }
                        y = t;
                        t++;
                    }
                    y = 1;
                }
                int z = 2 * m + (m - 1);
                for (int i = 1; i < count; i++) {
                    System.out.println(Integer.toString(3) + " " + Integer.toString(z) + " " + Integer.toString(2 * m + (m - 1) + i * (m - 1)));
                    z = t;
                    t++;
                }
            }

        } catch (IOException e) {
            System.out.println("Cannot read");
        }
    }
}