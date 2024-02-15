#include <iostream>
#include <vector>
#include <cmath>


using namespace std;


struct Node {
    long long max_sum = 0;
    long long sum = 0;

    bool isSet = false;
    long long setting = 0;


    long long count;
    explicit Node(long long count)
            : count(count)
    {
    }
};











struct SegmentsTreeOn_Max_Sum {
    std::vector<Node> tree{};
    int size;



    void propagate(int x) {
        std::size_t l = 2 * x + 1;
        std::size_t r = 2 * x + 2;
        if (l < size) {
            if (tree[x].isSet) {
                tree[l].max_sum = tree[l].count * tree[x].setting;
                tree[r].max_sum = tree[r].count * tree[x].setting;

                tree[l].sum = tree[l].count * tree[x].setting;
                tree[r].sum = tree[r].count * tree[x].setting;

               // tree[x].sum = tree[l].sum + tree[r].sum;
                tree[l].setting = tree[x].setting;
                tree[r].setting = tree[x].setting;

                tree[l].isSet = true;
                tree[r].isSet = true;
            }
        }
        tree[x].isSet = false;
        tree[x].setting = 0;
    }


    void set1(int i, long long v, int x, int l, int r);



    void changeSet(int l, int r, long long v, int x, int xl, int xr) {
        if (l >= xr || xl >= r) {
            return;
        }
        if (xl >= l && xr <= r) {
            propagate(x);
            tree[x].sum = tree[x].count * v;
            tree[x].max_sum = tree[x].count * v;
            tree[x].setting = v;
            tree[x].isSet = true;
            return;
        }
        propagate(x);
        int m = (xl + xr) / 2;
        changeSet(l, r, v, 2 * x + 1, xl, m);
        changeSet(l, r, v, 2 * x + 2, m, xr);
        tree[x].max_sum = std::max(tree[2 * x + 1].max_sum, tree[2 * x + 2].max_sum);
        tree[x].sum = tree[2 * x + 1].sum + tree[2 * x + 2].sum;
    }



    long long max(int l, int r, int x, int xl, int xr) {
        if (l >= xr || xl >= r) {
            return LLONG_MIN;
        }
        if (xl >= l && xr <= r) {
            //std::cout << xl << " " << xr << std::endl;
            propagate(x);
            return tree[x].max_sum;
        }
        propagate(x);
        int m = (xl + xr) / 2;
        auto a = max(l, r, 2 * x + 1, xl, m);
        auto b = max(l, r, 2 * x + 2, m, xr);
        //std::cout << a << " x "<< x << " " << b << std::endl;
        return std::max(a, b);
    }


    long long sum(int l, int r, int x, int xl, int xr) {
        if (l >= xr || xl >= r) {
            return 0;
        }
        if (xl >= l && xr <= r) {
            //std::cout << xl << " " << xr << std::endl;
            propagate(x);
            return tree[x].sum;
        }
        propagate(x);
        int m = (xl + xr) / 2;
        auto a = sum(l, r, 2 * x + 1, xl, m);
        auto b = sum(l, r, 2 * x + 2, m, xr);
        //std::cout << a << " x "<< x << " " << b << std::endl;
        return a + b;
    }


    explicit SegmentsTreeOn_Max_Sum(int size1) {
        int len = 2 * pow(2, ceil(log2((double) size1))) - 1;
        size = len;
        tree.reserve(len);

        int s = (len + 1) / 2;
        int f = 2;
        for (std::size_t i = 0; i < len; ++i) {
            tree.push_back(Node(s));
            if (i + 2 == f) {
                f *= 2;
                s /= 2;
            }
        }
    }


};

void SegmentsTreeOn_Max_Sum::set1(int i, long long int v, int x, int l, int r) {
    if (l == r - 1) {
        tree[x].max_sum = v;
        tree[x].sum = v;
        return;
    }
    int m = (l + r) / 2;
    if (i >= m) {
        set1(i, v, 2 * x + 2, m, r);
    } else {
        set1(i, v, 2 * x + 1, l, m);
    }
    tree[x].max_sum = std::max(tree[2 * x + 1].max_sum, tree[2 * x + 2].max_sum);
    tree[x].sum = tree[2 * x + 1].sum + tree[2 * x + 2].sum;
}


int main() {

    int n;
    scanf("%d\n", &n);
    int len = pow(2, ceil(log2((double) n)));
//    std::cout << 2 * len - 1<< std::endl;
    SegmentsTreeOn_Max_Sum tree(n);

    for (int i = 0; i < n; i++) {
        tree.set1(i, 0, 0, 0, len);
    }
    for (std::size_t i = n; i < len ; i++) {
        tree.set1(i, 0, 0, 0, len);
    }


    char ch;
    int a, b;
    long long d;

    while (cin >> ch) {
        if (ch == 'E') {
            break;
        }

        if (ch == 'Q') {
            cin >> d;
            int l = -1;
            int r = n + 1;
            while (r > l + 1) {
                int m = (l + r) / 2;
                auto func_ans = tree.max(0, m, 0, 0, len);
                auto sum = tree.sum(0, m, 0, 0, len);
               // cout << sum << " x " << m << endl;
                if (func_ans > d || sum > d) {
                    r = m;
                } else {
                    l = m;
                }
            }
            cout << std::max(0, l) << endl;
        } else {
            cin >> a >> b >> d;
            tree.changeSet(a - 1, b, d, 0, 0, len);
        }
    }

    //cout << tree.sum(1, 2, 0, 0, len);



    return 0;
}
