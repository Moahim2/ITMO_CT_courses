#include <iostream>
#include <vector>
#include <cmath>

using namespace std;

struct Node {
    int MAXLEN = 0;
    int count = 0;
    int length = 0;
    bool right = false;
    bool left = false;

    int setting = 0;
    explicit Node(int max)
        :MAXLEN(max)
    {
    }
};


struct PainterTree {
    vector<Node> tree{};
    int size;



    pair<int, int> getAns(int l, int r, int x, int xl, int xr) {
        if (l >= xr || xl >= r) {
            return {0, 0};
        }
        if (xl >= l && xr <= r) {
            propagate(x);
            return {tree[x].count, tree[x].length};
        }
        propagate(x);
        std::size_t m = (xl + xr) / 2;
        auto a = getAns(l, r, 2 * x + 1, xl, m);
        auto b = getAns(l, r, 2 * x + 2, m, xr);
        //std::cout << a << " x "<< x << " " << b << std::endl;
        int ans1 = a.first + b.first;
        if (tree[2 * x + 1].right == tree[2 * x + 2].left && tree[2 * x + 1].right) {
            ans1--;
        }
        return {ans1, a.second + b.second};
    }


    void propagate(int x) {
        int l = 2 * x + 1;
        int r = 2 * x + 2;
        if (l < size) {
            if (tree[x].setting == 1) { //white
                tree[l].setting = 1;
                tree[r].setting = 1;

                tree[l].count = 0;
                tree[r].count = 0;

                tree[l].length = 0;
                tree[r].length = 0;

                tree[l].left = false;
                tree[l].right = false;
                tree[r].left = false;
                tree[r].right = false;

            } else if (tree[x].setting == 2) { //black
                tree[l].setting = 2;
                tree[r].setting = 2;

                tree[l].count = 1;
                tree[r].count = 1;

                tree[l].length = tree[l].MAXLEN;
                tree[r].length = tree[r].MAXLEN;

                tree[l].left = true;
                tree[l].right = true;
                tree[r].left = true;
                tree[r].right = true;

            }
        }
        tree[x].setting = 0;
    }



    //0 - white
    //1 - black
    void change(int l, int  r, bool colour, int x, int xl, int xr) {
        //std::cout << "dcwcdcdcec" << std::endl;
        if (l >= xr || xl >= r) {
            return;
        }
        if (xl >= l && xr <= r) {
            propagate(x);
            tree[x].left = colour;
            tree[x].right = colour;
            if (colour) {
                tree[x].setting = 2;
                tree[x].count = 1;
                tree[x].length = tree[x].MAXLEN;
            } else {
                tree[x].setting = 1;
                tree[x].count = 0;
                tree[x].length = 0;
            }
            return;
        }
        propagate(x);
        std::size_t m = (xl + xr) / 2;
        change(l, r, colour, 2 * x + 1, xl, m);
        change(l, r, colour, 2 * x + 2, m, xr);
        //пересчет
        tree[x].left = tree[2 * x + 1].left;
        tree[x].right = tree[2 * x + 2].right;
        tree[x].count = tree[2 * x + 1].count + tree[2 * x + 2].count;
        if (tree[2 * x + 1].right == tree[2 * x + 2].left && tree[2 * x + 1].right) {
            tree[x].count--;
        }
        tree[x].length = tree[2 * x + 1].length + tree[2 * x + 2].length;
    }














    explicit PainterTree(int size1) {
        int len = 2 * pow(2, ceil(log2((double) size1))) - 1;
        size = len;
        tree.reserve(len);
        int maxL = (len + 1) / 2;
        int f = 2;
        for (std::size_t i = 0; i < len; ++i) {
            tree.push_back(Node(maxL));
            if (i + 2 == f) {
                f *= 2;
                maxL /= 2;
            }
        }
    }
};


int main() {
    int n = 1;
    cin >> n;
    char type;
    int x, y;

    vector<pair<bool, pair<int , int >>> table;
    table.reserve(n);
    int maxL = -500001;
    int minL = 500001;
    for (int i = 0; i < n; ++i) {
        cin >> type >> x >> y;
        y = x + y;
        if (y >= maxL) {
            maxL = y;
        }
        if (x <= minL) {
            minL = x;
        }
        if (type == 'W') {
            table.push_back({0, {x, y}});
        } else {
            table.push_back({1, {x, y}});
        }
    }
    int d = 1 - minL;
    for (int i = 0; i < n; i++) {
        table[i].second.first += d;
        table[i].second.second += d;
    }
    maxL += d;
    //cout << minL + d << " " << maxL<< endl;
    PainterTree painterTree(maxL);

    int len = pow(2, ceil(log2((double) maxL)));
    for (int i = 0; i < n; ++i) {
        painterTree.change(table[i].second.first - 1, table[i].second.second - 1, table[i].first, 0, 0, len);
//        tree.min(x - 1, y, 0, 0, len)
        auto ans = painterTree.getAns(0, maxL, 0, 0, len);
        std::cout << ans.first << " " << ans.second << endl;
    }




    return 0;
}
