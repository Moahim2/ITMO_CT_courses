import java.io.IOException;

import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.io.InputStreamReader;
import java.io.IOException; 
import java.util.Set;
import java.util.HashSet;
import java.util.*;
import java.math.*;

public class B4 {


    public static void main(String[] args) {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String a = in.readLine();
        String[] aa = a.split(" ");
        int n = Integer.parseInt(aa[0]);
        int m = Integer.parseInt(aa[1]);
        int[] weights = new int[n];
        int[] costs = new int[n];
        for (int i = 0; i < 2; i++) {
            String s = in.readLine();
            String[] ss = a.split(" ");
            if (i == 0) {
                for (int j = 0; j < n; j++) {
                    weights[j] = Integer.parseInt(ss[j]);
                }
            } else {
                for (int j = 0; j < n; j++) {
                    costs[j] = Integer.parseInt(ss[j]);
                }
            }
        }
        int[][] dp = new int[n][m + 1];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m + 1; j++) {
                dp[i][j] = -1;
            }
        }
        for (int i = 0; i <= n; i++) {
            for (int j = 0; j <= m; j++) {
                if (i == 0) {
                    
                }
                if (j == 0) {
                    dp[i][j] = 0;
                } 
                 else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i - 1][j - weights[i - 1]] + costs[i - 1]);
                }
            }
        }
        
    }
}