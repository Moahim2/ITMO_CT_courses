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

public class A3 {

    static class SNM {
        private int countVertex;
        private int[] graph;
        private int[] rangs;
        private int[] exp;
        private int[] bossExp;
        //private ArrayList<HashSet<Integer>> set;

        public SNM(int n) {
            countVertex = n;
            graph = new int[countVertex + 1];
            exp = new int[countVertex + 1];
            rangs = new int[countVertex + 1];
            bossExp = new int[countVertex + 1];
            for (int i = 1; i <= countVertex; i++) {
                graph[i] = i;
                rangs[i] = 0;
                bossExp[i] = 0;
                exp[i] = 0;
            }
        }

        public int get(int m) { 
            return graph[m];
        }   

        public int find(int m) {
            while(get(m) != m) {
                m = get(m);
            }
            return m;
        }

        public int getExp(int m) {
            int s = bossExp[m];
            while(get(m) != m) {
                s += bossExp[get(m)];
                m = get(m);
            }
            return s;
        }

        public void joined(int x, int y) {
            int X = find(x);
            int Y = find(y);
            if (X != Y) {
                if (rangs[X] > rangs[Y]) {
                    graph[Y] = X;
                    bossExp[Y] = bossExp[Y] - bossExp[X];
                    // for (Integer s : set.get(Y) ) {
                    //     exp[s] += bossExp[Y];
                    // }
                    // bossExp[Y] = 0;
                    // for (Integer s : set.get(X) ) {
                    //     exp[s] += bossExp[X];
                    // }
                    // bossExp[X] = 0;
                    // set.get(X).addAll(set.get(Y));
                } else if (rangs[X] < rangs[Y]) {
                    graph[X] = Y;
                    bossExp[X] = bossExp[X] - bossExp[Y];
                    // for (Integer s : set.get(X) ) {
                    //     exp[s] += bossExp[X];
                    // }
                    // bossExp[X] = 0;
                    // for (Integer s : set.get(Y) ) {
                    //     exp[s] += bossExp[Y];
                    // }
                    // bossExp[Y] = 0;
                    // set.get(Y).addAll(set.get(X));
                } else {
                    graph[Y] = X;
                    rangs[X]++;
                    bossExp[Y] = bossExp[Y] - bossExp[X];
                    // for (Integer s : set.get(Y) ) {
                    //     exp[s] += bossExp[Y];
                    // }
                    // bossExp[Y] = 0;
                    // for (Integer s : set.get(X) ) {
                    //     exp[s] += bossExp[X];
                    // }
                    // bossExp[X] = 0;
                    // set.get(X).addAll(set.get(Y));
                }   
            }
        }

        public void add (int x ,int v) {
            int X = find(x);
            bossExp[X] += v;
        }
        
    }


    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String a = in.readLine();
        String[] aa = a.split(" ");
        int n = Integer.parseInt(aa[0]);
        int m = Integer.parseInt(aa[1]);
        int[][] input = new int[m + 1][3];
        for (int i = 1; i <= m; i++) {
            String s = in.readLine();
            String[] ss = s.split(" ");
            char ch = ss[0].charAt(0);
            if (ch == 'a') {
                input[i][0] = 0;
            } else if (ch == 'j') {
                input[i][0] = 1;
            } else if (ch == 'g') {
                input[i][0] = 2;
            }
            if (ch == 'g') {
                input[i][1] = Integer.parseInt(ss[1]);
            } else {
                input[i][1] = Integer.parseInt(ss[1]);
                input[i][2] = Integer.parseInt(ss[2]);
            }
        } 
        SNM snm = new SNM(n);
        for (int i = 1; i <= m; i++) {
            if (input[i][0] == 0) {
                snm.add(input[i][1], input[i][2]);
            } else if (input[i][0] == 1) {
                snm.joined(input[i][1], input[i][2]);
            } else {
                
                System.out.println(snm.getExp(input[i][1]));
            }
        }
    }
}