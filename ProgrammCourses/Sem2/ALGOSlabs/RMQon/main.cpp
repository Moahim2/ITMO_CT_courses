#include <iostream>
#include <vector>
#include <cmath>

struct Node {
    long long val;

    long long setting = 0;
    long long adding = 0;
    int typeOper = 0;
    explicit Node(int val)
        : val(val)
    {
    }
};


struct SegmentsTree {
    std::vector<Node> tree{};
    std::size_t size{};


    void set1(std::size_t i, long long v, std::size_t x, std::size_t l, std::size_t r) {
        if (l == r - 1) {
            tree[x].val = v;
            return;
        }
        std::size_t m = (l + r) / 2;
        if (i >= m) {
            set1(i, v, 2 * x + 2, m, r);
        } else {
            set1(i, v, 2 * x + 1, l, m);
        }
        tree[x].val = std::min(tree[2 * x + 1].val, tree[2 * x + 2].val);
    }

    long long min(std::size_t l, std::size_t  r, std::size_t x, std::size_t xl, std::size_t xr) {
        if (l >= xr || xl >= r) {
            return LLONG_MAX;
        }
        if (xl >= l && xr <= r) {
            //std::cout << xl << " " << xr << std::endl;
            propagate(x);
            return tree[x].val;
        }
        propagate(x);
        std::size_t m = (xl + xr) / 2;
        auto a = min(l, r, 2 * x + 1, xl, m);
        auto b = min(l, r, 2 * x + 2, m, xr);
        //std::cout << a << " x "<< x << " " << b << std::endl;
        return std::min(a, b);
    }



//1 set
//2 add
    void propagate(std::size_t x) {
        std::size_t l = 2 * x + 1;
        std::size_t r = 2 * x + 2;
        if (l < size) {
            if (tree[x].typeOper == 1) { //..., set
               // std::cout << x << " set: " << tree[x].setting << " " << l << " " << r << std::endl;
                tree[l].adding = 0;
                tree[r].adding = 0;

                tree[l].setting = tree[x].setting;
                tree[r].setting = tree[x].setting;
                tree[r].typeOper = 1;
                tree[l].typeOper = 1;
                tree[l].val = tree[x].setting;
                tree[r].val = tree[x].setting;


//                tree[x].val = tree[x].setting;
            } else if (tree[x].typeOper == 2) { //..., add
                if (tree[r].typeOper == 1) {
                    tree[r].setting += tree[x].adding;
                    tree[r].adding = 0;
                } else {
                    tree[r].adding += tree[x].adding;
                    tree[r].typeOper = 2;
                }

                if (tree[l].typeOper == 1) {
                    tree[l].setting += tree[x].adding;
                    tree[l].adding = 0;
                } else {
                    tree[l].adding += tree[x].adding;
                    tree[l].typeOper = 2;
                }

                tree[r].val += tree[x].adding;
                tree[l].val += tree[x].adding;
                //std::cout << x << "     ffff" << std::endl
            }
        }
        tree[x].setting = 0;
        tree[x].adding = 0;
        tree[x].typeOper = 0;
    }


    void changeAdd(std::size_t l, std::size_t  r, long long v, std::size_t x, std::size_t xl, std::size_t xr) {
        //std::cout << "dcwcdcdcec" << std::endl;
        if (l >= xr || xl >= r) {
            return;
        }
        if (xl >= l && xr <= r) {
            propagate(x);
            if (tree[x].typeOper == 1) {
                tree[x].val = tree[x].setting;
                tree[x].adding = 0;
                tree[x].setting += v;
                tree[x].typeOper = 1;
            } else {
                tree[x].adding += v;
                tree[x].typeOper = 2;
            }
            tree[x].val += v;
            return;
        }
        propagate(x);
        std::size_t m = (xl + xr) / 2;
        changeAdd(l, r, v, 2 * x + 1, xl, m);
        changeAdd(l, r, v, 2 * x + 2, m, xr);
        tree[x].val = std::min(tree[2 * x + 1].val, tree[2 * x + 2].val);
    }

    void changeSet(std::size_t l, std::size_t  r, long long v, std::size_t x, std::size_t xl, std::size_t xr) {
        if (l >= xr || xl >= r) {
            return;
        }
        if (xl >= l && xr <= r) {
            propagate(x);
            tree[x].setting = v;
            tree[x].val = v;
            tree[x].adding = 0;

            tree[x].typeOper = 1;
            return;
        }
        propagate(x);
        std::size_t m = (xl + xr) / 2;
        changeSet(l, r, v, 2 * x + 1, xl, m);
        changeSet(l, r, v, 2 * x + 2, m, xr);
        tree[x].val = std::min(tree[2 * x + 1].val, tree[2 * x + 2].val);
    }

    explicit SegmentsTree(std::size_t size1) {
        std::size_t len = 2 * pow(2, ceil(log2((double) size1))) - 1;
        size = len;
        tree.reserve(len);
        for (std::size_t i = 0; i < len; ++i) {
            tree.push_back(Node(0));
        }
    }


    void toString() {
        std::cout << "TREE:    " << std::endl << std::endl;
        int f = 2;
        for (int i = 0; i < size; ++i) {
            std::cout << tree[i].val << " ";
            if (i + 2 == f) {
                std::cout << std::endl;
                f = f * 2;
            }
        }
        std::cout << std::endl;
    }
};




int main() {

    int n;
    scanf("%d\n", &n);
    int len = pow(2, ceil(log2((double) n)));
//    std::cout << 2 * len - 1<< std::endl;
    SegmentsTree tree(n);
    auto *arr = new long long[n];
    for (int i = 0; i < n; i++) {
        scanf("%lld", &arr[i]);
    }
    scanf("\n");
    for (std::size_t i = 0; i < n; i++) {
        tree.set1(i, arr[i], 0, 0, len);
        //std::cout << arr[i] << std::endl;
    }
    for (std::size_t i = n; i < len ; i++) {
        tree.set1(i, LLONG_MAX, 0, 0, len);
    }

    char a;
    char buf;
    std::size_t x;
    std::size_t y;
    long long z;
    scanf ("%c", &a);
    while (a == 's' || a == 'm' || a == 'a' || a == 't') {
        std::cin >> buf;
        std::cin >> buf;
        std::cin >> x;
        std::cin >> y;

        if (a == 's' || a == 'a') {
            std::cin >> z;
        }
        if (a == 's') {
            tree.changeSet(x - 1, y, z, 0, 0, len);
        } else if (a == 'm') {
            printf("%lld", tree.min(x - 1, y, 0, 0, len));
            printf("\n");
        } else if (a == 't') {
            tree.toString();
        } else {
            tree.changeAdd(x - 1, y, z, 0, 0, len);
        }
        //std::cout << tree.tree[3].adding << " add "  << tree.tree[3].typeOper << std::endl;
        if(!(std::cin >> a)) {
            break;
        }
    }

//    10
//    1 2 3 4 5 6 7 8 9 10
//    set 4 7 4
//    add 1 3 1
//    add 1 2 1
//    add 1 1 1
//    add 1 7 1
//    min 1 10
    return 0;
}
