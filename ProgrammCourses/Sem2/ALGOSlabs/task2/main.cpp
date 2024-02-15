#include <iostream>
#include <cmath>
#include <climits>

long long * tree;

void set(int i, long long v, int x, int l, int r) {
    if (l == r - 1) {
        tree[x] = v;
        return;
    }
    int m = (l + r) / 2;
    if (i >= m) {
        set(i, v, 2 * x + 2, m, r);
    } else {
        set(i, v, 2 * x + 1, l, m);
    }
    tree[x] = std::min(tree[2 * x + 1], tree[2 * x + 2]);
}

long long minim(int L, int R, int x, int l, int r) {
    if (r <= L || R <= l) {
        return LLONG_MAX;
    } else {
        if (l >= L && r <= R) {
            return tree[x];
        }
        int m = ((l + r) / 2);
        return std::min(minim(L, R, 2 * x + 1, l, m), minim(L, R, 2 * x + 2, m, r));
    }
}





int main() {
    int n;
    scanf("%d\n", &n);
    int len = pow(2, ceil(log2((double)n)));
    tree = new long long[2 * len - 1];
    for (int i = 0; i < sizeof(tree); i++) {
        tree[i] = 0;
    }
    auto *arr = new long long[n];
    for (int i = 0; i < n; i++) {
        scanf("%lld", &arr[i]);
    }
    scanf("\n");
    for (int i = 0; i < n; i++) {
        set(i, arr[i], 0, 0, n);
    }


    std::cout << "TREE:    " << std::endl << std::endl;
    int f = 2;
    for (int i = 0; i < 2 * len - 1; ++i) {
        std::cout << tree[i]<< " ";
        if (i + 2 == f) {
            std::cout << std::endl;
            f = f * 2;
        }
    }
    std::cout << std::endl;


    char a;
    char buf;
    int x;
    long long y;
    scanf ("%c", &a);
    while (a == 's' || a == 'm' || a == 'a') {
        //scanf ("%c", &buf);
        std::cin >> buf;
        //scanf("%c", &buf);
        std::cin >> buf;
        //scanf("%d", &x);
        std::cin >> x;
        //scanf("%lld", &y);
        std::cin >> y;
        if (a == 's') {
            set(x - 1, y, 0, 0, n);
        } else if (a == 'm') {
            printf("%lld", minim(x - 1, y, 0, 0, n));
            printf("\n");
        }
        if(!(std::cin >> a)) {
            break;
        }
    }
    delete[] tree;
    delete[] arr;
    return 0;
}

