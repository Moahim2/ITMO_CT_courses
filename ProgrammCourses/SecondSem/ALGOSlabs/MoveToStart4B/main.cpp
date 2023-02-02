#include <iostream>
#include <vector>
#include <random>

using namespace std;






struct Node {
    int val;
    int pr_y;
    Node * left = nullptr;
    Node * right = nullptr;
    int size = 1;
    bool reversing = false;

    Node(int val, int y, int size)
        : val(val)
        , pr_y(y)
        , size(size)
    {
    }
};

void propagateReversing(Node * tree) {
    if (tree == nullptr || !tree->reversing) {
        return;
    }
    swap(tree->left, tree->right);
    if (tree->left != nullptr) {
        tree->left->reversing = tree->left->reversing ? false : true;
    }
    if (tree->right != nullptr) {
        tree->right->reversing = tree->right->reversing ? false : true;
    }
    tree->reversing = false;
}


Node * merge(Node * tree1, Node * tree2) {
    if (tree1 == nullptr) {
        return tree2;
    } else if (tree2 == nullptr) {
        return tree1;
    } else if (tree1->pr_y >= tree2->pr_y) {
        int x = tree2->size;
        propagateReversing(tree1);
        propagateReversing(tree2);
        tree1->right = merge(tree1->right, tree2);
//        propagateReversing(tree1->right);
//        propagateReversing(tree1->left);
        tree1->size += x;
        return tree1;
    } else {
        int x = tree1->size;
        propagateReversing(tree1);
        propagateReversing(tree2);
        tree2->left = merge(tree1, tree2->left);
//        propagateReversing(tree2->right);
//        propagateReversing(tree2->left);
        tree2->size += x;
        return tree2;
    }
}

pair<Node *, Node *> split(Node * tree, int k) {
    if (tree == nullptr) {
        return {nullptr, nullptr};
    }
    propagateReversing(tree);
//    if (tree->reversing) {
//        propagateReversing(tree);
//        return split(tree, tree->size - k);
//    }
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
        return {p.first, tree};
    } else {
        auto p = split(tree->right, k - 1 - l_size);
        tree->right = p.first;
        int l, r;
        l = tree->left == nullptr ? 0 : tree->left->size;
        r = tree->right == nullptr ? 0 : tree->right->size;
        tree->size = l + r + 1;
        return {tree, p.second};
    }
}

Node * insert (Node * tree, int x , int y) {
    auto p = split(tree, x);
    auto n1 = merge(p.first, new Node(x + 1, y, 1));
    return merge(n1, p.second);
}

void inOrdertoString (Node * tree) {
    if (tree == nullptr) {
        return;
    }
    propagateReversing(tree);
    inOrdertoString(tree->left);
    cout << tree->val << " ";
    inOrdertoString(tree->right);
}

void testInOrder(Node * tree) {
    if (tree == nullptr) {
        return;
    }
    propagateReversing(tree);
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

//int sizeN(Node * node) {
//    return node == nullptr ? 0 : node->size;
//}



Node * reverse(Node * tree, int l, int r) {
    propagateReversing(tree);
    auto p1 = split(tree, l - 1);
    auto p2 = split(p1.second, r - l + 1);
    if (p2.first != nullptr) {
        p2.first->reversing = p2.first->reversing ? false : true;
    }
    auto n1 = merge(p1.first, p2.first);
    auto n2 = merge(n1, p2.second);
    return n2;
}

int main() {
    int n, m;
    cin >> n >> m;
    Node * m_tree = new Node(1, rand(), 1);
    for (int i = 1; i < n; ++i) {
        m_tree = insert(m_tree, i, rand());
    }
    //testInOrder(m_tree);

    //cout << m_tree->size << endl;
    //inOrdertoString(m_tree);
    //cout << endl;
//    auto x = moveToStart(m_tree, 4, 5);
//    inOrdertoString(x);
    int x, y;
    for (int i = 0; i < m; ++i) {
        cin >> x >> y;
        m_tree = reverse(m_tree, x, y);
//        inOrdertoString(m_tree);
//        cout << endl;
//        testInOrder(m_tree);
//        cout << endl;
    }
    inOrdertoString(m_tree);

    return 0;
}
