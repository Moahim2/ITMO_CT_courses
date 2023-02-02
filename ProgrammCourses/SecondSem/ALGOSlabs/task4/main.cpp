#include <iostream>
#include <cmath>
#include <climits>



class Node {
public:
    Node() {
        set_d = false;
        val = 0;
        set_chng = 0;
        add_chng = 0;
    }
public: bool set_d;
public: long long val;
public: long long set_chng;
public: long long add_chng;
};

Node * tree;

void calc(int x) {
    tree[x].val = std::min(tree[2 * x + 1].set_chng + tree[2 * x + 1].add_chng,
                           tree[2 * x + 2].set_chng + tree[2 * x + 2].add_chng);
    tree[x].set_chng = tree[x].val;
}

void set1(int i, long long v, int x, int l, int r) {
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


void  set_prop(int x) {
    if (tree[x].set_d) {
        tree[x].val = tree[x].set_chng;
        tree[2 * x + 1].set_chng = tree[x].set_chng;
        tree[2  * x + 2].set_chng = tree[x].set_chng;
        tree[2 * x + 1].set_d = tree[x].set_d;
        tree[2 * x + 2].set_d = tree[x].set_d;
        tree[2 * x + 1].add_chng = 0;
        tree[2 * x + 2].add_chng = 0;
        tree[x].set_d = false;
    }
}

void add_prop(int x) {
    if (tree[x].add_chng != 0) {
        tree[x].val += tree[x].add_chng;
        tree[x].set_chng = tree[x].val;
        tree[2 * x + 1].add_chng += tree[x].add_chng;
        tree[2 * x + 2].add_chng += tree[x].add_chng;
        tree[x].add_chng = 0;
    }
}

long long minim(int L, int R, int x, int l, int r) {
    if (r <= L || R <= l) {
        return LLONG_MAX;
    } else {
        if (l >= L && r <= R) {
            set_prop(x);
            add_prop(x);
            return tree[x].val;
        }
        int m = ((l + r) / 2);
        set_prop(x);
        add_prop(x);
        return std::min(minim(L, R, 2 * x + 1, l, m), minim(L, R, 2 * x + 2, m, r));
    }
}




void set(int L, int R, long long v, int x, int l, int r) {
    if (r <= L || R <= l) {
        return;
    } else {
        if (l >= L && r <= R) {
            set_prop(x);
            tree[x].val = tree[x].set_chng;
            tree[x].set_chng = v;
            return;
        }
        int m = (l + r) / 2;
        add_prop(x);
        set(L, R, v, 2 * x + 1, l, m);
        set(L, R, v, 2 * x + 2, m, r);
        calc(x);
    }
}



void add(int L, int R, long long v, int x, int l, int r) {
    if (r <= L || R <= l) {
        return;
    } else {
        if (l >= L && r <= R) {
            add_prop(x);
            tree[x].add_chng = v;
            return;
        }
        int m = (l + r) / 2;
        set_prop(x);
        add_prop(x);
        add(L, R, v, 2 * x + 1, l, m);
        add(L, R, v, 2 * x + 2, m, r);
        calc(x);
    }
}


int main() {
    int n;
    scanf("%d\n", &n);
    int len = pow(2, ceil(log2((double)n)));
    tree = new Node[2 * len - 1];
    auto *arr = new long long[n];
    for (int i = 0; i < n; i++) {
        scanf("%lld", &arr[i]);
    }
    scanf("\n");
    for (int i = 0; i < n; i++) {
        set1(i, arr[i], 0, 0, n);
    }
    char a;
    char buf;
    int x;
    long long y;
    long long z;
    scanf ("%c", &a);
    while (a == 's' || a == 'm' || a == 'a') {
        std::cin >> buf;
        std::cin >> buf;
        std::cin >> x;
        std::cin >> y;
        if (a == 's' || a == 'a') {
            std::cin >> z;
        }
        if (a == 's') {
            set(x - 1, (int) y, z, 0, 0, n);
        } else if (a == 'm') {
            printf("%lld", minim(x - 1, static_cast<int>(y), 0, 0, n));
            printf("\n");
        } else {
            add(x - 1, static_cast<int> (y), z, 0, 0, n);
        }
        if(!(std::cin >> a)) {
            break;
        }
    }
    delete[] tree;
    delete[] arr;
    return 0;
}


