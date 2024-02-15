#include <iostream>
#include <vector>
#include <cmath>
#include <fstream>


//
using namespace std;

struct Node {
    int val;

    bool isSet = false;
    int setting = INT_MIN;
    bool isChange = false;
    explicit Node(int val)
            :val(val)
    {
    }
};






struct AntiRMQTree {
    vector<Node> tree{};
    int size;
    int lens;

    bool propagate(int x) {
        int l = 2 * x + 1;
        int r = 2 * x + 2;
        if (l < size) {
            if (tree[x].isSet) {
                int a = tree[l].setting;
                int b = tree[l].setting;
                tree[l].setting = tree[l].isSet ? std::max(tree[x].setting, tree[l].setting) : tree[x].setting;
                tree[r].setting = tree[r].isSet ? std::max(tree[x].setting, tree[r].setting) : tree[x].setting;


                tree[l].isSet =  true;
                tree[r].isSet = true;

//                if (tree[l].val < tree[x].setting) {
//                    return false;
//                }
//
//                if (tree[r].val < tree[x].setting) {
//                    return false;
//                }

                tree[l].val = tree[l].isChange ? std::max(tree[x].setting, tree[l].val) : tree[x].setting;
                tree[r].val = tree[r].isChange ? std::max(tree[x].setting, tree[r].val) : tree[x].setting;

                tree[l].isChange = true;
                tree[r].isChange = true;
            }
        }
        tree[x].isSet = false;
        tree[x].setting = INT_MIN;
        return true;
    }


     void change(int l, int  r, int new_val, int x, int xl, int xr) {
        //std::cout << "dcwcdcdcec" << std::endl;
        if (l >= xr || xl >= r) {
            return;
        }
        if (xl >= l && xr <= r) {
            propagate(x);
            tree[x].val = tree[x].isChange ? std::max(tree[x].val, new_val) : new_val;
            tree[x].setting = tree[x].isSet ? std::max(tree[x].setting, new_val) : new_val;
            tree[x].isSet = true;
            tree[x].isChange = true;
            return;
        }
        propagate(x);
        std::size_t m = (xl + xr) / 2;
        change(l, r, new_val, 2 * x + 1, xl, m);
        change(l, r, new_val, 2 * x + 2, m, xr);
        //пересчет
        tree[x].val = std::max(tree[2 * x + 1].val, tree[2 * x + 2].val);
        if (tree[2 * x + 1].isChange || tree[2 * x + 2].isChange) {
            tree[x].isChange = true;
        }
    }






    vector<int> getAns() {
        vector<int> ans;
        ans.reserve(lens);
        for (int i = 0; i < size; ++i) {
            if (2 * i + 2 < size) {
                propagate(2 * i + 1);
                propagate(2 * i + 2);
            }
            propagate(i);
        }
        for (int i = size / 2; i < size / 2 + lens; ++i) {
            ans.push_back(tree[i].val);
        }
        return ans;
    }



    explicit AntiRMQTree(int size1) {
        lens = size1;
        int len = 2 * pow(2, ceil(log2((double) size1))) - 1;
        size = len;
        tree.reserve(len);
        for (std::size_t i = 0; i < len; ++i) {
            tree.push_back(Node(INT_MAX));
        }
    }


};



struct RMQ {
    vector<Node> tree{};
    int size;


    void set1(int i, int v, int x, int l, int r) {
        if (l == r - 1) {
            tree[x].val = v;
            return;
        }
        int m = (l + r) / 2;
        if (i >= m) {
            set1(i, v, 2 * x + 2, m, r);
        } else {
            set1(i, v, 2 * x + 1, l, m);
        }
        tree[x].val = std::min(tree[2 * x + 1].val, tree[2 * x + 2].val);
    }



    explicit RMQ(int size1) {
        int len = 2 * pow(2, ceil(log2((double) size1))) - 1;
        size = len;
        tree.reserve(len);
        for (std::size_t i = 0; i < len; ++i) {
            tree.push_back(Node(INT_MAX));
        }
    }


    int min(int l, int  r, int x, int xl, int xr) {
        if (l >= xr || xl >= r) {
            return INT_MAX;
        }
        if (xl >= l && xr <= r) {
            return tree[x].val;
        }
        int m = (xl + xr) / 2;
        auto a = min(l, r, 2 * x + 1, xl, m);
        auto b = min(l, r, 2 * x + 2, m, xr);
        //std::cout << a << " x "<< x << " " << b << std::endl;
        return std::min(a, b);
    }
};


int main() {
    std::ifstream source("rmq.in");
    std::ofstream out1("rmq.out");
    vector<pair<int , pair<int, int>>> p_ans;
    int n, m;
    source >> n >> m;
    int len = pow(2, ceil(log2((double) n)));
    AntiRMQTree antiRmqTree(n);

    int x, y;
    int z;
    for (int i = 0; i < m; ++i) {
        source >> x >> y >> z;
        p_ans.push_back({z, {x, y}});
        antiRmqTree.change(x - 1, y, z, 0, 0, len);
    }

    source.close();

    auto p = antiRmqTree.getAns();

    RMQ rmq(n);
    for (int i = 0; i < n; i++) {
        rmq.set1(i, p[i], 0, 0, len);
    }

    for (int i = 0; i < m; ++i) {
        auto k = p_ans[i];
        auto a = rmq.min(k.second.first - 1, k.second.second, 0, 0, len);
        //cout << i << " " << a << endl;
        if (k.first != a) {
            out1 << "inconsistent";
            out1.close();
            return 0;
        }
    }

    out1 << "consistent" << endl;
    for (int i = 0; i < p.size(); ++i) {
        out1 << p[i] << " ";
    }

    out1.close();
    return 0;
}
