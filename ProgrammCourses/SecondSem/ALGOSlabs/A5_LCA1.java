import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

public class A5_LCA1 {
    static HashSet<Integer>[] graph;
    static int[] reverse_graph;
    static int[] depths;
    static int[][] dp_LCA;
    static int LogN;


    public static void dfs_construct_depth(int x, int depth) {
        depths[x] = depth;
        for (int children : graph[x]) {
            dfs_construct_depth(children, depth + 1);
        }
    }

    public static int find_LCA(int v, int u) {
        for (int i = LogN - 1; i >= 0; i--) {
            u = depths[dp_LCA[u][i]] >= depths[v] ? dp_LCA[u][i] : u;
        }
        if (u == v) {
            return v;
        }
       // System.out.println(u + " " + v);
        for (int i = LogN - 1; i >= 0; i--) {
            if (dp_LCA[v][i] != dp_LCA[u][i]) {
               // System.out.println(v + " d " + u);
                v = dp_LCA[v][i];
                u = dp_LCA[u][i];
            }
        }
        return reverse_graph[v];
    }

    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(source.readLine());
        graph = new HashSet[n];
        reverse_graph = new int[n];
        depths = new int[n];
        LogN = (int) Math.ceil(Math.log(n)/ Math.log(2));
        for (int i = 0; i < n; i++) {
            graph[i] = new HashSet<>();
        }
        for (int i = 1; i < n; ++i) {
            int x = Integer.parseInt(source.readLine()) - 1;
            graph[x].add(i);
            reverse_graph[i] = x;
        }
//        System.out.println(LogN);
        int m = Integer.parseInt(source.readLine());

//        System.out.println(Arrays.toString(graph));
        dp_LCA = new int[n][LogN];
        dfs_construct_depth(0, 0);
        //System.out.println(Arrays.toString(depths));
//        System.out.println();
        for (int i = 0; i < n; i++) {
            dp_LCA[i][0] = reverse_graph[i];
        }

        for (int j = 1; j < LogN; j++) {
            for (int i = 0; i < n; i++) {
                dp_LCA[i][j] = dp_LCA[dp_LCA[i][j - 1]][j - 1];
            }
        }

//        1for (int i = 0; i < n; i++) {
//            System.out.println(Arrays.toString(dp_LCA[i]));
//        }


        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8));

        for (int i = 0; i < m; i++) {
            String[] tmpInput = source.readLine().split(" ");
            int u = Integer.parseInt(tmpInput[0]) - 1;
            int v = Integer.parseInt(tmpInput[1]) - 1;
            //System.out.println(u + " " + v);
            int ans = depths[u] >= depths[v] ? find_LCA(v, u) : find_LCA(u, v);
            out.write(Integer.toString(++ans));
            out.newLine();
        }



        out.close();
    }
}