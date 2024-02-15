#include <iostream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <deque>
#include <algorithm>


using namespace std;

class Karp {

    const vector<unordered_set<int>>& g;
    const vector<vector<long long int>>& c;
    vector<vector<long long int>> f;

public: Karp(const std::vector<unordered_set<int>>& g, const std::vector<std::vector<long long int>>& c)
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


        for (int i = 0; i < vis.size(); i++) {
            if (vis[i]) {
                setOfAchievable.insert(i);
            }
        }
        //cout << minCut;
        return minCut;
    }

    long long int getMinCutEdges(int start, int end, vector<vector<int>>& numbers,
                                 unordered_set<int>& setEdges) {
        auto setOfAchievable = unordered_set<int>();
        auto minCut = getMinCutSet(start, end, setOfAchievable);

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
                ///!!!
                setEdges.insert(numbers[u][v]);
            }
        }

        return minCut;
    }


    pair<unordered_set<int>, unordered_set<int>> getMinCutSets(int start, int end) {
        auto setOfAchievable = unordered_set<int>();
        getMinCutSet(start, end, setOfAchievable);

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

        return make_pair(setOfAchievable, setOfNotAchievable);
    }


};


//2D)
int main3() {
    int n, m, p;
    cin >> n >> m >> p;

    int fullCount = m + n + 2;
    vector<unordered_set<int>> g = vector<unordered_set<int>>(fullCount, unordered_set<int>());
    std::vector<std::vector<long long int>> c =
            vector<vector<long long int>>(fullCount, vector<long long int>(fullCount, 0));

    unordered_map<int, int> discounts{};

    for (int i = 0; i < n; i++) {
        int cost, count, instr;
        cin >> cost >> count;
        g[0].insert(1 + i);
        c[0][1 + i] = cost;

        for (int j = 0; j < count; j++) {
            cin >> instr;
            --instr;
            g[1 + i].insert(n + 1 + instr);
            c[1 + i][n + 1 + instr] = (long long int) INT_MAX;
        }
    }

    for (int i = 0 ; i < m; i++) {
        int cost;
        cin >> cost;
        g[n + 1 + i].insert(fullCount - 1);
        c[n + 1 + i][fullCount - 1] = cost;
    }

    for (int i = 0; i < p; i++) {
        int a, b, cost;
        cin >> a >> b >> cost;
        --a;
        --b;
        discounts.insert({n + 1 + a, n + 1 + b});
        discounts.insert({n + 1 + b, n + 1 + a});
        g[n + 1 + a].insert(n + 1 + b);
        g[n + 1 + b].insert(n + 1 + a);
        c[n + 1 + a][n + 1 + b] = cost - c[n + 1 + a][fullCount - 1];
        c[n + 1 + b][n + 1 + a] = cost - c[n + 1 + b][fullCount - 1];
    }

    Karp karp = Karp(g, c);

    auto pair = karp.getMinCutSets(0, fullCount - 1);
    auto setOfAchievable = pair.first;
    auto setOfNotAchievable = pair.second;

    unordered_set<int> achievableInstruments{};
    unordered_set<int> notAchievableInstruments{};
    unordered_set<int> notAchievableOrders{};
    unordered_set<int> achievableOrders{};



    for (auto e : setOfAchievable) {
        if (e > 0 && e < n + 1) {
            achievableOrders.insert(e);
        } else if (e > 0) {
            achievableInstruments.insert(e);
        }
    }

    for (auto e : setOfNotAchievable) {
        if (e < fullCount - 1 && e < n + 1) {
            notAchievableOrders.insert(e);
        } else if (e < fullCount - 1) {
            notAchievableInstruments.insert(e);
        }
    }

    long long int ans = 0;

    for (auto order : notAchievableOrders) {
        cout << order << " ord" << endl;
        ans += c[0][order];
    }

    unordered_set<int> finishInstruments{};

    for (auto instr : achievableInstruments) {
        finishInstruments.insert(instr);
        int t = -1;
        if (discounts.count(instr) != 0) {
            auto d = discounts[instr];
            if (finishInstruments.count(d) == 0) {
                if (achievableInstruments.count(d) != 0) {
                    t = d;
                }
            } else {
                continue;
            }
        }

        ans += c[instr][fullCount - 1];
        if (t != -1) {
            ans += c[instr][t];
        }
    }


    cout << ans;

}


