#include <iostream>
#include <vector>
#include <valarray>

using namespace std;
vector<vector<unsigned long long>> SpTable;
unsigned long long int minOnRange(unsigned long long int x, unsigned long long int y) {
    //cout << SpTable[index][x] << " " << SpTable[index][y + 1 - (1 << index)];
    return min(SpTable[__lg(y - x)][x], SpTable[__lg(y - x)][y + 1 - pow(2, __lg(y - x))]);
}

int main() {
    unsigned long long int n, m, a1, u, v, r, lastU, lastV;
    string s;
    cin >> n >> m >> a1 >> u >> v;
    //cout << u << " " << v << " " << n << " " << m << " " << a1 << endl;
    lastU = u;
    lastV = v;
    vector<unsigned long long> a(n);
    for (size_t i = 0; i < n; ++i) {
        if (i == 0) {
            a[0] = a1;
        } else {
            a[i] = (23 * a[i - 1] + 21563) % 16714589;
        }
    }
    SpTable.resize(__lg(n) + 1, vector<unsigned long long>(n));
    for (int j = 0; j < n; ++j) {
        SpTable[0][j] = a[j];
        //cout << SpTable[0][j] << ", ";
    }
    cout<<endl;
    //cout << __lg(n) << " " << n;
    for (size_t i = 1; i < __lg(n) + 1; ++i) {
        for (size_t j = 0; (j + pow(2, i - 1)) <= n; ++j) {
            SpTable[i][j] = min(SpTable[i - 1][j], SpTable[i - 1][j + (pow(2, i - 1))]);
           // cout << SpTable[i][j] << ", ";
        }
    }

    for (size_t i = 1; i <= m; ++i) {
        //cout << u << " " << v << " ";
        r = minOnRange(min(u - 1, v - 1), max(u - 1, v - 1));
        //cout << r << endl;
        lastU = u;
        lastV = v;
        u = ((17 * u + 751 + r + 2 * i) % n) + 1;
        v = ((13 * v + 593 + r + 5 * i) % n) + 1;
    }
    cout << lastU << " " << lastV << " " << r;
    return 0;
}
