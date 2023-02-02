#include <iostream>
#include <vector>
#include <random>

using namespace std;




struct Node {
    long long  val;
    int pr_y;
    Node * left = nullptr;
    Node * right = nullptr;
    int size = 1;

    long long  sum;

    Node(long long val, int y, int size)
            : val(val)
            , pr_y(y)
            , size(size)
            , sum(val)
    {
    }


};

long long SumN(Node * tree) {
    if (tree == nullptr) {
        return 0;
    }
    return tree->sum;
}



Node * merge(Node * tree1, Node * tree2) {
    if (tree1 == nullptr) {
        return tree2;
    } else if (tree2 == nullptr) {
        return tree1;
    } else if (tree1->pr_y >= tree2->pr_y) {
        int x = tree2->size;
        long long s = SumN(tree2);
        tree1->right = merge(tree1->right, tree2);
        tree1->size += x;
        tree1->sum += s;
        return tree1;
    } else {
        int x = tree1->size;
        long long s = SumN(tree1);
        tree2->left = merge(tree1, tree2->left);
        tree2->size += x;
        tree2->sum += s;
        return tree2;
    }
}

pair<Node *, Node *> split(Node * tree, int k) {
    if (tree == nullptr) {
        return {nullptr, nullptr};
    }
    if (k == 0) {
        return {nullptr, tree};
    }
    if (k == tree->size) {
        return {tree, nullptr};
    }

    int l_size = 0;
    if (tree->left != nullptr) {
        l_size = tree->left->size;
    }
    if (l_size >= k) {
        auto p = split(tree->left, k);
        tree->left = p.second;
        int l, r;
        l = tree->left == nullptr ? 0 : tree->left->size;
        r = tree->right == nullptr ? 0 : tree->right->size;
        tree->size = l + r + 1;
        long long ls = SumN(tree->left);
        long long rs = SumN(tree->right);
        tree->sum = ls + rs + tree->val;
        return {p.first, tree};
    } else {
        auto p = split(tree->right, k - 1 - l_size);
        tree->right = p.first;
        int l, r;
        l = tree->left == nullptr ? 0 : tree->left->size;
        r = tree->right == nullptr ? 0 : tree->right->size;
        tree->size = l + r + 1;
        long long ls = SumN(tree->left);
        long long rs = SumN(tree->right);
        tree->sum = ls + rs + tree->val;
        return {tree, p.second};
    }
}

Node * insert (Node * tree, long long x, int index, int y) {
    auto p = split(tree, index);
    auto n1 = merge(p.first, new Node(x, y, 1));
    return merge(n1, p.second);
}

void inOrdertoString (Node * tree) {
    if (tree == nullptr) {
        return;
    }
    inOrdertoString(tree->left);
    cout << tree->val << " ";
    inOrdertoString(tree->right);
}

void testInOrder(Node * tree) {
    if (tree == nullptr) {
        return;
    }
    testInOrder(tree->left);
    cout << tree->val << " " << tree->size << endl;
    testInOrder(tree->right);
}

//Node * mainBuild(size_t n) {
//    vector<pair<int, int>> source;
//    source.reserve(n);
//
//    for (int i = 0; i < n; ++i) {
//        int pr_y = rand();
//        source.push_back({i, pr_y});
//    }
//
//}

//Node * moveToStart(Node * m_tree, int l, int r) {
//    auto p1 = split(m_tree, l - 1);
//    auto p2 = split(p1.second, r - l + 1);
//    auto n1 = merge(p2.first, p1.first);
//    auto n2 = merge(n1, p2.second);
//    return n2;
//}

long long sum (Node * & tree, int l, int r) {
    if (l > r || tree == nullptr) {
        return 0;
    }
    auto p1 = split(tree, l - 1);
    auto p2 = split(p1.second, r - l + 1);
    long long ans = SumN(p2.first);
    auto tmp1 = merge(p1.first, p2.first);
    tree = merge(tmp1, p2.second);
    return ans;
}

int sizeN(Node * tree) {
    if (tree == nullptr) {
        return 0;
    }
    return tree->size;
}

long long sum_even_not_even(Node *& e_tree, Node *& ne_tree, int l, int r) { // -1 6 7 8 9 4
      l = max(1, l);
      r = min(r, sizeN(e_tree) + sizeN(ne_tree));
//    int le = (l + (l % 2)) / 2;
//    int re = (r - (r % 2)) / 2;
//
//    int ln = l % 2 == 1 ? (l / 2) + 1 : le + 1;
//    int rn = r % 2 == 1 ? (r / 2) + 1 : re;
      int le = l % 2 == 0 ? l / 2 : (l + 1) / 2;
      int re = r % 2 == 0 ? r / 2 : (r - 1) / 2;

      int ln = l % 2 == 1 ? (l + 1) / 2 : (l + 2) / 2;
      int rn = r % 2 == 1 ? (r + 1) / 2 : r / 2;
    //cout << le << " " << re << "     " << ln << " " << rn << endl;
      long long ans1 = sum(e_tree, le, re);
      long long ans2 = sum(ne_tree, ln, rn);
    //cout << ans1 << " " << ans2 << endl;
      return ans1 + ans2;
}




int main() {
    int sw = 1;
    while (true) {
        int n, m;
        cin >> n >> m;
        if (n == 0 && m == 0) {
            return 0;
        }
        Node *m_tree_n_even = nullptr;
        Node *m_tree_even = nullptr;                        // = new Node(1, rand(), 1);
        long long x1;
        for (int i = 0; i < n; ++i) {
            cin >> x1;
            if (i % 2 == 0) {
                if (m_tree_n_even == nullptr) {
                    m_tree_n_even = new Node(x1, rand(), 1);
                } else {
                    m_tree_n_even = insert(m_tree_n_even, x1, i / 2, rand());
                }
            } else {
                if (m_tree_even == nullptr) {
                    m_tree_even = new Node(x1, rand(), 1);
                } else {
                    m_tree_even = insert(m_tree_even, x1, i / 2,rand());
                }
            }
        }
        //  cout << sum_even_not_even(m_tree_even, m_tree_n_even, 1, 5);
        cout << "Swapper " << sw << ":" << endl;
        int x, y, z;
        for (int i = 0; i < m; ++i) {
            cin >> z >> x >> y;
            if (z == 1) {
                x = max(1, x);
                y = min(y, sizeN(m_tree_even) + sizeN(m_tree_n_even));

                int le = x % 2 == 0 ? x / 2 : (x + 1) / 2;
                int re = y % 2 == 0 ? y / 2 : (y - 1) / 2;

                int ln = x % 2 == 1 ? (x + 1) / 2 : (x + 2) / 2;
                int rn = y % 2 == 1 ? (y + 1) / 2 : y / 2;

                //cout << le << " " << re << "     " << ln << " " << rn << endl;

                auto pN1 = split(m_tree_n_even, ln - 1);
                auto pN2 = split(pN1.second, rn - ln + 1);

                auto pE1 = split(m_tree_even, le - 1);
                auto pE2 = split(pE1.second, re - le + 1);

                auto tmp1 = merge(pE1.first, pN2.first);
                auto tmp2 = merge(pN1.first, pE2.first);
                m_tree_even = merge(tmp1, pE2.second);
                m_tree_n_even = merge(tmp2, pN2.second);
//                cout << "S: " << endl;
//                inOrdertoString(m_tree_even);
//                cout << endl;
//                inOrdertoString(m_tree_n_even);
//                cout << " <>" << endl;
            } else {
                if (y < 1 || x > sizeN(m_tree_even) + sizeN(m_tree_n_even)) {
                    cout << 0 << endl;
                } else {
                    x = max(1, x);
                    y = min(y, sizeN(m_tree_even) + sizeN(m_tree_n_even));
                }
                cout << sum_even_not_even(m_tree_even, m_tree_n_even, x, y) << endl;
            }
            //m_tree = moveToStart(m_tree, x, y);
        }
        ++sw;
        cout << endl;
        delete m_tree_even;
        delete m_tree_n_even;
    }


}