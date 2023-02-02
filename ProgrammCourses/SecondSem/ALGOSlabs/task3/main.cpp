class Node {
public:
    Node() {
        count_seg = 0;
        left = false;
        right = false;
        sum = 0;
        set = -1;
    }
public: int count_seg;
public: bool left;
public: bool right;
public: int sum;
public: int set;
};

Node * tree;
void set1(int i, int v, int x, int l, int r) {
    if (l == r - 1) {
        tree[x].sum = v;
        return;
    }
    int m = (l + r) / 2;
    if (i >= m) {
        set1(i, v, 2 * x + 2, m, r);
    } else {
        set1(i, v, 2 * x + 1, l, m);
    }
    tree[x].sum = tree[2 * x + 1].sum + tree[2 * x + 2].sum;
    if (tree[2 * x + 1].right && tree[2 * x + 2].left) {
        tree[x].count_seg = tree[2 * x + 1].count_seg + tree[2 * x + 2].count_seg - 1;
    } else {
        tree[x].count_seg = tree[2 * x + 1].count_seg + tree[2 * x + 2].count_seg;
    }
    tree[x].left = tree[2 * x + 1].left;
    tree[x].right = tree[2 * x + 2].right;
}

void set(int L, int R, int v, int x, int l, int r) {
    if (r <= L || R <= l) {
        return;
    } else {
        if (l >= L && r <= R) {
            tree[x].set = v;
            tree[x].set += v;
            return;
        }
        int m = (l + r) / 2;
        set(L, R, v, 2 * x + 1, l, m);
        set(L, R, v, 2 * x + 2, m, r);

    }
}




int main() {

}