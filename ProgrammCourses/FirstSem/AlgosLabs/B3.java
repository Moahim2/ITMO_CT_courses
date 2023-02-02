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



public class B3 {

    static class SNM {
        private int countVertex;
        private int[] graph;
        private int[] rangs;

        public SNM(int n) {
            countVertex = n;
            graph = new int[countVertex + 1];
            rangs = new int[countVertex + 1];
            for (int i = 1; i <= countVertex; i++) {
                graph[i] = i;
                rangs[i] = 0;
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

        public void joined(int x, int y) {
            int X = find(x);
            int Y = find(y);
            if (X != Y) {
                if (rangs[X] > rangs[Y]) {
                    graph[Y] = X;
                } else if (rangs[X] < rangs[Y]) {
                    graph[X] = Y;
                } else {
                    graph[Y] = X;
                    rangs[X]++;
                }   
            }
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
        String a = in.readLine();
        String[] aa = a.split(" ");
        int n = Integer.parseInt(aa[0]);
        int m = Integer.parseInt(aa[1]);
        int k = Integer.parseInt(aa[2]);
        int[][] graph = new int[m][2];
        Set<ArrayList<Integer>> set = new HashSet<>();
        for (int i = 0; i < m; i++) {
            String s = in.readLine();
            String[] ss = s.split(" ");
            int b = Integer.parseInt(ss[0]);
            int c = Integer.parseInt(ss[1]);
            if (c >= b) {
                set.add(new ArrayList<Integer>(List.of(b, c)));
                graph[i][1] = c;
                graph[i][0] = b;
            } else {
                set.add(new ArrayList<Integer>(List.of(c, b)));
                graph[i][1] = b;
                graph[i][0] = c;
            }
        }
        int[][] input = new int[k][3];
        // int[][] gr = new int[n + 1][n + 1];
        for (int i = 0; i < k; i++) {
            String s = in.readLine();
            String[] ss = s.split(" ");
            if (ss[0].charAt(0) == 'a') {
                input[i][0] = 1;
                input[i][1] = Integer.parseInt(ss[1]);
                input[i][2] = Integer.parseInt(ss[2]);
            } else {
                input[i][0] = 2;
                input[i][1] = Integer.parseInt(ss[1]);
                input[i][2] = Integer.parseInt(ss[2]);
                if (input[i][1] >= input[i][2]) {
                    set.add(new ArrayList<Integer>(List.of(input[i][2], input[i][1])));
                } else {
                    set.add(new ArrayList<Integer>(List.of(input[i][1], input[i][2])));
                }    
                // gr[input[i][1]][input[i][2]] = 1;
                // gr[input[i][2]][input[i][1]] = 1;
                
            }
        }
        SNM snm = new SNM(n);
        for (int i = 0; i < m; i++) {
            if (!set.contains(new ArrayList<Integer>(List.of(graph[i][0], graph[i][1])))) {
                System.out.println(1);
                snm.joined(graph[i][0], graph[i][1]);
            }
        }
        List<String> answer = new ArrayList<>();
        for (int i = k - 1; i >= 0; i--) {
            if (input[i][0] == 1) {
                if (snm.find(input[i][1]) == snm.find(input[i][2])) {
                    answer.add("YES");
                } else {
                    answer.add("NO");
                }
            } else {
                snm.joined(input[i][1], input[i][2]);
            }
        }
        for (int i = answer.size() - 1; i >= 0; i--) {
            System.out.println(answer.get(i));
        }
    }    
}
