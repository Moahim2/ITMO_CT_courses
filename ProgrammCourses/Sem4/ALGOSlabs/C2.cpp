#include <iostream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <deque>
#include <iterator>
#include <algorithm>

using namespace std;

class C2 {

    const vector<unordered_set<int>>& g;
    const vector<vector<long long int>>& c;
    vector<vector<long long int>> f;

public: C2(const std::vector<unordered_set<int>>& g, const std::vector<std::vector<long long int>>& c)
        : g(g), c(c) {
        f = vector<vector<long long int>>(c.size(), vector<long long int>(c.size(), 0));
    }


    long long int bfs(int start, int end, vector<int>& paths) {
        for (int & path : paths) {
            path = -1;
        }
        paths[start] = -2;
        auto queue = deque<pair<int, long long int>>();
        queue.emplace_back(start, LONG_LONG_MAX);

        while (!queue.empty()) {
            auto p = queue.front();
            auto u = p.first;
            auto flow = p.second;
            queue.pop_front();
            for (auto v : g[u]) {
                if (paths[v] == -1) {
                    if (f[u][v] < c[u][v]) {
                        paths[v] = u;
                        auto addingFlow = min(flow, c[u][v] - f[u][v]);
                        queue.emplace_back(v, addingFlow);
                        if (v == end) {
                            return addingFlow;
                        }

                    }
                }
            }
        }
        return 0;
    }


    long long int getMaxFlow(int start, int end) {
        auto paths = vector<int>(c.size(), -1);
        long long int maxFlow = 0;

        while (true) {
            auto addingFlow = bfs(start, end, paths);

            if (addingFlow == 0) {
                return maxFlow;
            }

            maxFlow += addingFlow;
            auto v = end;
            while (v != start) {
                auto u = paths[v];
                f[u][v] += addingFlow;
                f[v][u] -= addingFlow;
                v = u;
            }
        }
    }


    void cutDFS(int v, vector<bool>& vis) {
        if (vis[v]) {
            return;
        }
        vis[v] = true;
        for (auto u : g[v]) {
            if (c[v][u] - f[v][u] > 0) {
                cutDFS(u, vis);
            }
        }
    }

    long long int getMinCutSet(int start, int end, unordered_set<int> & setOfAchievable) {
        auto minCut = getMaxFlow(start, end);
        auto vis = vector<bool>(c.size(), false);
        cutDFS(start, vis);

        /////////////////!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ///
        ///

        for (int i = 0; i < vis.size(); i++) {
            if (vis[i]) {
                setOfAchievable.insert(i);
            }
        }
        return minCut;
    }

    long long int getMinCutEdges(int start, int end, vector<vector<int>>& numbers,
                                                           unordered_set<int>& setEdges) {
        auto setOfAchievable = unordered_set<int>();
        auto minCut = getMinCutSet(start, end, setOfAchievable);

        //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        //
        //
        if (minCut >= (long long int) INT_MAX) {
            return minCut;
        }
        //
        //
        //

        auto fullSet = unordered_set<int>();
        for (int i = 0; i < c.size(); i++) {
            fullSet.insert(i);
        }

        auto setOfNotAchievable = unordered_set<int>();
        for (auto i : fullSet) {
            if (setOfAchievable.count(i) == 0) {
                setOfNotAchievable.insert(i);
            }
        }

        for (auto u : setOfAchievable) {
            for (auto v : setOfNotAchievable) {
                ////////!!!!!!!!!!!!!!!!!!!!!!!!!
                if (c[u][v] != 0) {
                    setEdges.insert(numbers[u][v]);
                }
            }
        }

        return minCut;
    }


};

pair<int, int> getNM(int p, int n) {
    return make_pair(p / n, p % n);
}

int main1() {
    int m, n;
    cin >> m >> n;

    auto map = vector<string>(m, "");

    for (int i = 0; i < m; i++) {
        string str;
        cin >> str;
        map[i] = str;
    }

    auto count = 2 * n * m;

    auto graph = vector<unordered_set<int>>(count, unordered_set<int>());

    auto c = vector<vector<long long int>>(count, vector<long long int>(count, 0));

    auto numbers = vector<vector<int>>(count, vector<int>(count, 0));


    auto shift = m * n;



    auto biection = unordered_map<int, int>();
    int number = 0;

    int start = -1;
    int end  = -2;

    for (int i = 0; i < map.size(); i++) {
        for (int j = 0; j < map[i].size(); j++) {
            auto cur = i * n + j;

            if (map[i][j] == '#') {
                continue;
            }

            if (map[i][j] == 'A') {
                start = cur;
            }

            if (map[i][j] == 'B') {
                end = cur;
            }

            graph[cur].insert(cur + shift);
            graph[cur + shift].insert(cur);
            if (map[i][j] == '-' || map[i][j] == 'A' || map[i][j] == 'B') {
                c[cur][cur + shift] = INT_MAX;
            } else {
                c[cur][cur + shift] = 1;
            }
            c[cur + shift][cur] = 0;

            numbers[cur][cur + shift] = (number);;
            numbers[cur + shift][cur] = (number);
            biection.insert({number++, cur});

            if (cur % n != 0) {
                auto d = cur - 1;

                auto pair = getNM(d, n);
                auto a = pair.first;
                auto b = pair.second;

                if (map[a][b] != '#') {
                    graph[cur + shift].insert(d);
                    graph[d].insert(cur + shift);
                    c[cur + shift][d] = (long long int)  INT_MAX;
                    c[d][cur + shift] = 0;

                    graph[d + shift].insert(cur);
                    graph[cur].insert(d + shift);
                    c[d + shift][cur] = (long long int) INT_MAX;
                    c[cur][d + shift] = 0;
                }
            }
            if ((cur + 1) % n != 0) {
                auto d = cur + 1;

                auto pair = getNM(d, n);
                auto a = pair.first;
                auto b = pair.second;

                if (map[a][b] != '#') {
                    graph[cur + shift].insert(d);
                    graph[d].insert(cur + shift);
                    c[cur + shift][d] = (long long int)  INT_MAX;
                    c[d][cur + shift] = 0;

                    graph[d + shift].insert(cur);
                    graph[cur].insert(d + shift);
                    c[d + shift][cur] = (long long int)  INT_MAX;
                    c[cur][d + shift] = 0;
                }
            }
            if (cur + n < shift) {
                auto d = cur + n;

                auto pair = getNM(d, n);
                auto a = pair.first;
                auto b = pair.second;

                if (map[a][b] != '#') {
                    graph[cur + shift].insert(d);
                    graph[d].insert(cur + shift);
                    c[cur + shift][d] = (long long int)  INT_MAX;
                    c[d][cur + shift] = 0;

                    graph[d + shift].insert(cur);
                    graph[cur].insert(d + shift);
                    c[d + shift][cur] = (long long int)  INT_MAX;
                    c[cur][d + shift] = 0;
                }
            }
            if (cur - n >= 0) {
                auto d = cur - n;

                auto pair = getNM(d, n);
                auto a = pair.first;
                auto b = pair.second;

                if (map[a][b] != '#') {
                    graph[cur + shift].insert(d);
                    graph[d].insert(cur + shift);
                    c[cur + shift][d] = (long long int)  (INT_MAX);
                    c[d][cur + shift] = 0;

                    graph[d + shift].insert(cur);
                    graph[cur].insert(d + shift);
                    c[d + shift][cur] = (long long int)  (INT_MAX);
                    c[cur][d + shift] = 0;
                }
            }

        }
    }



    auto c2 = C2(graph, c);

    auto setEdges = unordered_set<int>();
    auto ans = c2.getMinCutEdges(start, end, numbers, setEdges);

    if (ans >= INT_MAX) {
        cout << -1;
        return 0;
    }

    cout << ans << endl;

    //val set = setEdges.stream().map(C2.Edge::number).map(biection::get).toList().toHashSet()
    auto set = unordered_set<int>();
    for (auto i : setEdges) {
        set.insert(biection[i]);
    }

    for (int i = 0; i < map.size(); i++) {
        for (int j = 0; j < map[i].size(); j++) {
            if (map[i][j] != '.') {
                cout << (map[i][j]);
                continue;
            }
            auto cur = i * n + j;
            if (set.count(cur) != 0) {
                cout << ('+');
            } else {
                cout << ('.');
            }
        }
        cout << endl;
    }


    return 0;
}






