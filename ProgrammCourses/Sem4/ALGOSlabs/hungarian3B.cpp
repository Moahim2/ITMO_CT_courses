#include <bits/stdc++.h>
#include <iostream>
#include <vector>
#include <unordered_set>
#include <deque>
#include <iterator>
#include <algorithm>
#include <numeric>

using namespace std;


class HungarianAlg {

    vector<vector<long long int>> c; //matrix
    int n;


public:

    HungarianAlg(vector<vector<long long int>>& c, int n)
        : c(c),
          n(n)
    {

    }

    pair<int, vector<pair<int, int>>> getMinimumWeightMatch() {
        auto a = vector<long long int>(n, 0);
        auto b = vector<long long int>(n, 0);
        auto p = vector<int>(n, -1);
        auto reverse_p = vector<int>(n, -1);
        auto reverse_free = vector<int>(n, -1);


        for (int v = 0; v < n; v++) {
            reverse_free = vector<int>(n, -1);
            bool flag = false;
            for (auto i : p) {
                if (i == v) {
                    flag = true;
                    break;
                }
            }
            if (flag) {
                continue;
            }

            auto l_plus = unordered_set<int>();
            l_plus.insert(v);
            auto r_minus = unordered_set<int>();

            for(int j = 0; j < n; j++) {
                r_minus.insert(j);
            }

            auto m = vector<pair<int, long long int>>(n);
            for (int j = 0; j < n; j++) {
                m[j] = make_pair(v, c[v][j] + a[v] + b[j]);
            }


            while (true) {
                long long int d = LONG_LONG_MAX;
                int u;
                int lv;
                for(auto j : r_minus) {
                    if (m[j].second <= d) {
                        d = m[j].second;
                        u = j;
                        lv = m[j].first;
                    }
                }

                for (auto i : l_plus) {
                    a[i] -= d;
                }
                for (int j = 0; j < n; j++) { //r_plus
                    if (r_minus.count(j) == 0) {
                        b[j] += d;
                    }
                }

                for (int j = 0; j < n; j++) {
                    m[j].second -= d;
                }

                r_minus.erase(u);

                reverse_free[u] = lv;/////////////
                if (p[u] == -1) {
                    //слажна
                    auto cur_u = u;
                    while (true) {
                        p[cur_u] = reverse_free[cur_u];
                        int tmp = p[cur_u];
                        swap(reverse_p[p[cur_u]], cur_u);
                        if (tmp == v) {
                            break;
                        }
                    }
                    //
                    break;
                }

                l_plus.insert(p[u]);
                for (int j = 0; j < n; j++) {
                    long long int x = c[p[u]][j] + a[p[u]] + b[j];
                    if (m[j].second > x) {
                        m[j].first = p[u];
                        m[j].second = x;
                    }
                }

            }
        }


        long long int ans = 0;
        vector<pair<int, int>> pairs{};
        for (int i = 0; i < n; i++) {
            pairs.emplace_back(p[i], i);
            ans += c[p[i]][i];
        }
        return make_pair(ans, pairs);
    }




};

//int main3() {
//#ifndef Y
//    freopen("assignment.in", "r", stdin);
//    freopen("assignment.out", "w", stdout);
//#endif
//
//    int N;
//    int x;
//    cin >> N;
//    vector<vector<long long int>> c = vector<vector<long long int>>(N, vector<long long int>(N, 0));
//    for (int i = 0; i < N; i++) {
//        for (int j = 0; j < N; j++) {
//            cin >> x;
//            c[i][j] = x;
//        }
//    }
//
//    HungarianAlg hungarianAlg = HungarianAlg(c, N);
//
//    auto ans = hungarianAlg.getMinimumWeightMatch();
//
//    cout << ans.first << endl;
//    for (auto pair : ans.second) {
//        cout << pair.first + 1 << " " << pair.second + 1 << endl;
//    }
//
//    return 0;
//}
