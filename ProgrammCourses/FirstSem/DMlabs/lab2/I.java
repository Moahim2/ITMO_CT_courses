import java.math.BigInteger;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.*;

public class I {



    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        int start = Integer.parseInt(in.readLine());
        if (start == 1) {
            String str = in.readLine();
            int lenS = str.length();
            int q = 1;
            int Q = 0;
            while (lenS + Q >=   q) {
                q = q * 2;
                Q++;
            }
            StringBuilder sb = new StringBuilder();
            int n = 1;
            str = " " + str;
            int k = 1;
            int period = 1;
            for (int i = 1; i < lenS + 1 + Q; i++) {
                if (i == n) {
                    int ans = 0;
                    int N = n;
                    int p = period;
                    int K = k;
                    for (int j = i; j < lenS + 1 + Q; j++) {
                        if (p > 0) {
                            if (j == N) {
                                N = N * 2;
                            } else {
                                ans += (int) str.charAt(K);
                                ans = ans % 2;
                                K++;
                            }
                            p--;
                            if (p == 0) {
                                p = -(period);
                            }
                        } else {
                            if (j == N) {
                                N = N * 2;
                            } else {
                                K++;
                            }
                            p++;
                            if (p == 0) {
                                p = period;
                            }
                        }
                    }
                    sb.append(ans);
                    n = n * 2;
                    period = period * 2;
                } else {
                    sb.append(str.charAt(k));
                    k++;
                }
            }
            System.out.println(sb.toString());    
        } else {
            String str = in.readLine();
            int lenS = str.length();
            str = " " + str;
            int n = 1;
            int period = 1;
            int bad = -1;
            for (int i = 0; i < lenS + 1; i++) {
                if (i == n) {
                    int ans = 0;
                    int N = n;
                    int p = period;
                    for (int j = i; j < lenS + 1; j++) {
                        if (p > 0) {
                            if (j == N) {
                                N = N * 2;
                            } else {
                                ans += (int) (str.charAt(j));
                                ans = ans % 2;
                            }
                            p--;
                            if (p == 0) {
                                p = -(period);
                            }
                        } else {
                            if (j == N) {
                                N = N * 2;
                            }
                            p++;
                            if (p == 0) {
                                p = period;
                            }
                        }
                    }
                    if (Integer.parseInt(Character.toString(str.charAt(n))) != ans) {
                        if (bad == -1) {    
                            bad = n;
                        } else {
                            bad += n;
                        }
                    }
                    n = n * 2;
                    period = period * 2;
                }
            }
            StringBuilder sb = new StringBuilder();
            n = 1;
            //  System.out.println(bad);
            if (bad == -1) {
                for (int i = 1; i < lenS + 1; i++) {
                    if (i == n) {
                        n = n * 2;
                    } else {
                        sb.append(str.charAt(i));
                    }
                }
            } else {
                for (int i = 1; i < lenS + 1; i++) {
                    if (i == n) {
                        n = n * 2;
                    } else {
                        if (i == bad) {
                        sb.append((((int) str.charAt(i)) + 1) % 2);
                        } else {
                            sb.append(str.charAt(i));
                        }
                    }
                }
            }
            System.out.println(sb.toString());
        }
    }
}