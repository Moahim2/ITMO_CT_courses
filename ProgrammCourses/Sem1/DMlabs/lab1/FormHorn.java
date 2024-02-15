import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.InputStreamReader;
import java.io.IOException;
public class FormHorn {
    static boolean fBool(int[][] tabl, int[] vector, int k, int n) {
        boolean F = false;
        for (int i = 0; i < k; i++) {
            boolean S = false;
            int count = 0;
            for (int j = 0; j < n; j++) {
                if (tabl[i][j] != -1) {
                    if (vector[j] == 2) { // значит не знаем
                        S = true;
                        break;
                    } else if (tabl[i][j] == vector[j]) { //значит точно 1
                        S = true;
                        break;
                    }
                } else {
                    count++;
                }
            }
            if (!S && count < n) {
                F = true;
            }
        }
        return F;
    }

    static boolean func(int[][] tabl, int k, int n) {
        //System.out.println(5);
        boolean one = false;
        for (int i = 0; i < k; i++) {
            int count = 0;
            for (int j = 0; j < n; j++) {
                if (tabl[i][j] == -1) {
                    count += 1;
                }
            }
            if (count == n - 1) {
                one = true;
                break;
            }
        }
        if (one) {
            int[] vector = new int[n];
            for (int i = 0; i < n; i++) {
                vector[i] = 2;
            }
            for (int i = 0; i < k; i++) { // записали в вектор номера одиночных переменных и их позицию (с отрицанием или без)
                int count = 0;
                int number = 0;
                for (int j = 0; j < n; j++) {
                    if (tabl[i][j] == -1) {
                        count += 1;
                    } else {
                        number = j;
                    }
                }
                if (count == n - 1 && vector[number] == 2) {
                    vector[number] = tabl[i][number];
                }
            }
            if (fBool(tabl, vector, k, n)) {
                return true; 
            } else {
                int[] dis = new int[k];
                for (int i = 0; i < k; i++) {
                    for (int j = 0; j < n; j++) {
                        if (tabl[i][j] != -1) {
                            if (tabl[i][j] == vector[j]) { //значит точно 1
                                dis[i] = 1;
                                break;
                            }
                        }
                    }
                }
                for (int i = 0; i < k; i++) {
                    if (dis[i] == 1) {
                        for (int j = 0; j < n; j++) {
                            tabl[i][j] = -1;
                        }
                    }
                    for (int j = 0; j < n; j++) {
                        if (vector[j] != 2) {
                            tabl[i][j] = -1;
                        }
                    }
                }
                /*for (int i = 0; i < k; i++) { //vcp
                    for (int j = 0; j < n; j++) {
                        System.out.print(tabl[i][j]);
                    }
                    System.out.println();
                }*/
                return func(tabl, k, n);
            }
        } else {
            /*int q = 0;
            for (int i = 0; i < k; i++) {
                for (int j = 0; j < n; j++) {
                    if (tabl[i][j] != -1) {
                    q++;
                    }
                }
            }
            if (q == 0) {
                return false;
            } */
            return false;
        }
    }
    public static void main(String[] arg) {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String a = in.readLine();
            String[] a1 = a.split(" ");
            int n = Integer.parseInt(a1[0]); //кол-во переменных
            int k = Integer.parseInt(a1[1]); // кол-во строк
            String[] args = new String[k];
            for (int i = 0; i < k; i++) {
                args[i] = in.readLine();
            }
            String[][] tabl = new String[k][n];
            for (int i = 0; i < k; i++) {
                String[] spl = args[i].split(" ");
                for (int j = 0; j < n; j++) {
                    tabl[i][j] = spl[j];
                }
            }
            int[][] tablInt = new int[k][n];
            for (int i = 0; i < k; i++) {
                for (int j = 0; j < n; j++) {
                    tablInt[i][j] = Integer.parseInt(tabl[i][j]);
                }
            }
            if (func(tablInt, k, n)) {
                System.out.println("YES");
            } else {
                System.out.println("NO");
            }


        } catch(IOException e) {
            System.out.println(e.getMessage());
        }
    }
}