import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.HashSet;

public class B5_LCA_MIN {
    static class Pair{
        int val;
        int node;
        Pair(int val, int node) {
            this.val = val;
            this.node = node;
        }
    }


    static HashSet<Pair>[] graph;
    static Pair[] reverse_graph;
    static int[] depths;
    static int[][] dp_LCA;
    static int LogN;


    static int[][] minimum;


    public static void dfs_construct_depth(int x, int depth) {
        depths[x] = depth;
        for (var children : graph[x]) {
            dfs_construct_depth(children.node, depth + 1);
        }
    }


    public static int find_min(int node, int highNode) {
        if (node == highNode) {
            return Integer.MAX_VALUE;
        }
        int cur_min = minimum[node][0];
        int cur_node = node;
        for (int i = LogN - 1; i >= 0; i--) {
            if (dp_LCA[cur_node][i] >= highNode) {
                cur_min = Math.min(cur_min, minimum[cur_node][i]);
                cur_node = dp_LCA[cur_node][i];
            }
        }
        return cur_min;
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
        return reverse_graph[v].node;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader source = new BufferedReader(new InputStreamReader(System.in));
        int n = Integer.parseInt(source.readLine());
        graph = new HashSet[n];
        reverse_graph = new Pair[n];
        depths = new int[n];
        LogN = (int) Math.ceil(Math.log(n)/ Math.log(2));
        for (int i = 0; i < n; i++) {
            graph[i] = new HashSet<>();
        }
        reverse_graph[0] = new Pair(Integer.MAX_VALUE, 0);
        for (int i = 1; i < n; ++i) {
            String[] tmp = source.readLine().split(" ");
            int x = Integer.parseInt(tmp[0]) - 1;
            int y = Integer.parseInt(tmp[1]);
            graph[x].add(new Pair(y, i));
            reverse_graph[i] = new Pair(y, x);
        }
//        System.out.println(LogN);
        int m = Integer.parseInt(source.readLine());

//        System.out.println(Arrays.toString(graph));
        dp_LCA = new int[n][LogN];
        minimum = new int[n][LogN];
        dfs_construct_depth(0, 0);
        //System.out.println(Arrays.toString(depths));
//        System.out.println();
        for (int i = 0; i < n; i++) {
            dp_LCA[i][0] = reverse_graph[i].node;
            minimum[i][0] = reverse_graph[i].val;
        }


        for (int j = 1; j < LogN; j++) {
            for (int i = 0; i < n; i++) {
                dp_LCA[i][j] = dp_LCA[dp_LCA[i][j - 1]][j - 1];
            }
        }


        for (int j = 1; j < LogN; j++) {
            for (int i = 0; i < n; i++) {
                minimum[i][j] = Math.min(minimum[i][j - 1], minimum[dp_LCA[i][j - 1]][j - 1]);
            }
        }
//        for (int i = 0; i < n; i++) {
//            System.out.println(Arrays.toString(minimum[i]));
//        }


        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8));

        for (int i = 0; i < m; i++) {
            String[] tmpInput = source.readLine().split(" ");
            int u = Integer.parseInt(tmpInput[0]) - 1;
            int v = Integer.parseInt(tmpInput[1]) - 1;
            //System.out.println(u + " " + v);
            int highNode = depths[u] >= depths[v] ? find_LCA(v, u) : find_LCA(u, v);
            int a1 = find_min(u, highNode);
            int a2 = find_min(v, highNode);
            int ans = Math.min(a1, a2);
            out.write(Integer.toString(ans));
            out.newLine();
        }



        out.close();
    }
}
