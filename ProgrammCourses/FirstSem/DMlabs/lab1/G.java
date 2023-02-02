import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.math.BigInteger;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;
public class G {
    static long pow(int a, int k) {
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
            int countOne = 0;
            boolean impossible = false;
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            int n = Integer.parseInt(in.readLine()); // кол-во данных чисел
            String args = in.readLine(); //
            String K = in.readLine();
            if (K.length() > 33) {
                System.out.print("Impossible");
            } else {
                long k = Long.parseLong(K); //
                String s = Long.toBinaryString(k);//
                String[] x = args.split(" ");
                String[] binaryNambers = new String[n];
                int maxLength = s.length();
                for (int i = 0; i < n; i++) {
                    binaryNambers[i] = Long.toBinaryString(Long.parseLong(x[i])); //ищем максимальное число по длине
                    //System.out.println(binaryNambers[i]);
                    if (maxLength < binaryNambers[i].length()) {
                        maxLength = binaryNambers[i].length();
                    }
                }
                int[][] tabl = new int[maxLength][n + 1];
                for (int i = 0; i < n; i++) { // заполняем первые n столцов
                    int length = binaryNambers[i].length();
                    for (int j = 0; j < length; j++) {
                        tabl[maxLength - 1 - j][i] = Integer.parseInt(Character.toString(binaryNambers[i].charAt(length - 1 - j)));
                    }
                    int diff = maxLength - length;
                    if (diff > 0) {
                        for (int j = diff - 1; j >= 0; j--) {
                            tabl[j][i] = 0;
                        }
                    }
                }
                for (int i = 0; i < s.length(); i++) {
                    tabl[maxLength - i - 1][n] = Integer.parseInt(Character.toString(s.charAt(s.length()- 1 - i)));
                    if (tabl[maxLength - i - 1][n] == 1) {
                        //System.out.println(maxLength - i - 1);
                        countOne++;
                    }
                }
                if (maxLength - s.length() > 0) {
                    for (int i = maxLength - s.length() - 1; i >= 0; i--) {
                        tabl[i][n] = 0;
                    }
                }
                /*for (int i = 0; i < maxLength; i++) { // otladka
                    for (int j = 0; j < n + 1; j++) {
                        System.out.print(tabl[i][j]);
                    }
                    System.out.println();
                }
                System.out.println();*/
                for (int i = 0; i < maxLength; i++) {
                    if (tabl[i][n] == 1) {
                        int c = 0;
                        for (int j = 0; j < n; j++) {
                            if (tabl[i][j] == 0) {
                                c++;
                            }
                        }
                        if (c == n) {
                            impossible = true;
                        }
                    }
                }
                if (!impossible) {
                    String[] tablStr = new String[maxLength];
                    for (int i = 0; i < maxLength; i++) {
                        StringBuilder sb = new StringBuilder();
                        for (int j = 0; j < n; j++) {
                            sb.append(Character.forDigit(tabl[i][j], 2));
                        }
                        tablStr[i] = sb.toString();
                    }
                    for (int i = 0; i < maxLength; i++) {
                        for (int j = 0; j < maxLength; j++) {
                            if (((i != j) && (tablStr[i].equals(tablStr[j])) && (tabl[i][n] != tabl[j][n]))){
                                impossible = true;
                                break;
                            }
                        }
                        if (impossible) {
                            break;
                        }
                    }
                }
                if (impossible || k < 0) {
                    System.out.print("Impossible");
                } else if (k == 0) {
                    System.out.print("~1&1");
                } else {
                    /*for (int i = 0; i < maxLength; i++) { // otladka
                        for (int j = 0; j < n + 1; j++) {
                            System.out.print(tabl[i][j]);
                        }
                        System.out.println();
                    }*/
                    String[] Dis= new String[countOne];
                    int countBack = 0;
                    //System.out.println(countOne);
                    for (int i = 0; i < maxLength; i++) {
                        StringBuilder sb = new StringBuilder();
                        //System.out.println(tabl[i][n]);
                        if (tabl[i][n] == 1) {
                            sb.append('(');
                            for (int j = 0; j < n; j++) {
                                if (tabl[i][j] == 1) {
                                    sb.append(Character.forDigit(j + 1, 10));
                                } else {
                                    sb.append('~');
                                    sb.append(Character.forDigit(j + 1, 10));
                                }
                                if (j < n - 1) {
                                    sb.append('&');
                                }
                            }
                            sb.append(')');
                        } else {
                            countBack++;
                        }
                        if (sb.length() > 0) {
                            //System.out.println(1);
                            Dis[i - countBack] = sb.toString();
                        }
                    }
                    for (int i = 0; i < countOne; i++) {
                        System.out.print(Dis[i]);
                        if (i < countOne - 1) {
                            System.out.print('|');
                        }
                    }
                }
            }
        } catch (IOException e) {
            System.out.println(1);
        }
    }
}