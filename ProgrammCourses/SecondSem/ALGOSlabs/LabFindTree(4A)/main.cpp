#include <iostream>
#include <cmath>
#include <vector>
#include <numeric>
#include <algorithm>
#include <random>

struct ScapegoatTree {

    struct Node
    {
        int64_t key;
        Node * left = nullptr;
        Node * right = nullptr;
        Node * parent = nullptr;
        int64_t sizeN = 1;
        Node(int64_t val, Node *parent)
                : key(val), parent(parent)
        {
        }
    };

    struct MyPair {
        Node * first = nullptr;
        size_t second = 0;

        MyPair(Node * first, size_t second)
        : first(first),
          second(second)
        {
        }
    };

    Node * root = nullptr;
    std::size_t sizeTree = 0;
    double alpha = 0.75;
    size_t max_size_tree = 0;
    ScapegoatTree(double alpha)
            : alpha(alpha) {
        if (alpha < 0.5 || alpha > 1.0) {
            throw std::invalid_argument("Alpha must be in range [0.5; 1]");
        }
    }

    bool insert(int64_t value) {
        if (root == nullptr) {
            root = new Node(value, nullptr);
            ++sizeTree;
            return true;
        }

        auto p = find(root, value);
        if (p.first->key != value) {
            if (p.first->key < value) {
                p.first->right = new Node(value, p.first);
            } else {
                p.first->left = new Node(value, p.first);
            }
            Node * tmp = p.first;
            while (tmp != nullptr) {
                tmp->sizeN++;
                tmp = tmp->parent;
            }
            ++sizeTree;
            if (max_size_tree < sizeTree) {
                max_size_tree = sizeTree;
            }
            if (p.second + 1 > floor(-std::log(sizeTree) / std::log(alpha))) {
                checkRebuildTree(findScapegoat(p.first));
            }
            return true;
        }
        return false;
    }

    bool remove(int64_t value) {
        auto f = find(root, value);
        Node *recedingNode = f.first;
        if (recedingNode == nullptr || recedingNode->key != value) {
            return false;
        }
        Node *parent = recedingNode->parent;
        Node *tmp = nullptr;

        if (recedingNode->left == nullptr) {
            tmp = recedingNode->right;
        } else if (recedingNode->right == nullptr) {
            tmp = recedingNode->left;
        }

        if (recedingNode->left == nullptr || recedingNode->right == nullptr) {
            adoptionSon(parent, tmp, value);
            Node * g = parent;
            while (g != nullptr) {
                g->sizeN--;
                g = g->parent;
            }
        } else {
            tmp = recedingNode->right;
            while (tmp->left != nullptr) {
                tmp->sizeN--;
                tmp = tmp->left;
            }
            tmp->left = recedingNode->left;
            tmp->left->parent = tmp;
            if (recedingNode->right->key != tmp->key) {
                adoptionSon(tmp->parent, tmp->right, tmp->key);
                if (tmp->parent != nullptr) {
                    tmp->parent->sizeN = size(tmp->parent->left) + size(tmp->parent->right) + 1;
                }
                tmp->right = recedingNode->right;
                tmp->right->parent = tmp;
            }
            adoptionSon(parent, tmp, value);
            Node * j = parent;
            while (j != nullptr) {
                j->sizeN--;
                j = j->parent;
            }

            if (tmp->right != nullptr) {
                tmp->right->sizeN = size(tmp->right->right) + size(tmp->right->left) + 1;
            }
            tmp->sizeN = size(tmp->left) + size(tmp->right) + 1;
        }
        if (sizeTree - 1 == 0) {
            root = nullptr;
        } else if (parent == nullptr) {
            root = tmp;
            root->parent = nullptr;
        }
        delete recedingNode;
        // if (parent == nullptr) {
        //            root = tmp;
        //        }
        --sizeTree;
        if (sizeTree < alpha * max_size_tree) {
            checkRebuildTree(root);
        }
        return true;
    }

    std::vector<int64_t> values() const {
        std::vector<int64_t> ans(sizeTree);
        if (sizeTree == 0) {
            return ans;
        }
        size_t p = 0;
        buildVectorValue(root, p, ans);
        return ans;
    }

    ~ScapegoatTree() //+
    {
        cleanup(root);
    }

//private

    int64_t buildVectorValue(Node *node, size_t p, std::vector<int64_t> &ans, Node *par = nullptr) const {
        if (node == nullptr) {
            ans[p++] = par->key;
            return p;
        }
        p = buildVectorValue(node->left, p, ans, node);
        if (node->left != nullptr) {
            ans[p++] = node->key;
        }
        if (node->right != nullptr) {
            p = buildVectorValue(node->right, p, ans, node);
        }
        return p;
    }

    void cleanup(Node *n) {
        if (n == nullptr) {
            return;
        }
        cleanup(n->left);
        cleanup(n->right);
        delete n;
    }

    size_t siblingSize(Node *node) const {
        if (node->parent != nullptr && node->parent->left != nullptr && node->parent->right != nullptr) {
            if (node->parent->left == node) {
                return nodeSize(node->parent->right);
            } else {
                return nodeSize(node->parent->left);
            }
        } else {
            return 0;
        }
    }

    size_t nodeSize(Node *node) const {
        if (node == nullptr) {
            return 0;
        }
        return nodeSize(node->left) + 1 + nodeSize(node->right);
    }

    Node * findScapegoat(Node *node) const {
        size_t size = 1;
        size_t height = 0;
        while (node->parent != nullptr) {
            ++height;
            size_t totalSize = 1 + size + siblingSize(node);
            if (height >= floor(-log(totalSize) / std::log(alpha))) {
                return node->parent;
            }
            node = node->parent;
            size = totalSize;
        }
        return nullptr;
    }

    void checkRebuildTree(Node *scapegoat) {
        if (scapegoat != nullptr) {
            max_size_tree = sizeTree;
            if (scapegoat->parent != nullptr) {
                Node *parent = scapegoat->parent;
                adoptionSon(parent, rebuildScapegoatTree(scapegoat), scapegoat->key);
            } else {
                root = rebuildScapegoatTree(root);
                root->parent = nullptr;
            }
        }
    }

    Node * rebuildScapegoatTree(Node *scapegoat) {
        if (scapegoat == root) {
            return buildTreeOnFlattenTree(sizeTree, flattenTree(root));
        }
        Node *head = flattenTree(scapegoat);
        size_t size = 1;
        Node *tmp = head;
        while (tmp->right != nullptr) {
            ++size;
            tmp = tmp->right;
        }
        return buildTreeOnFlattenTree(size, head);
    }

    Node * flattenTree(Node *newRoot, Node *head = nullptr) {
        if (newRoot == nullptr) {
            return head;
        }
        newRoot->right = flattenTree(newRoot->right, head);
        return flattenTree(newRoot->left, newRoot);
    }

    Node * buildTreeOnFlattenTree(size_t size, Node *node) {
        if (size == 1) {
            node->right = nullptr;
            node->left = nullptr;
            node->sizeN = 1;
            return node;
        }

        if (size == 2) {
            node->sizeN = 2;
            node->right->sizeN = 1;
            node->left = nullptr;
            node->right->left = nullptr;
            node->right->right = nullptr;
            node->right->parent = node;
            return node;
        }

        size_t i = 0;
        Node *mediumNode = node;
        while (i < size / 2) {
            mediumNode = mediumNode->right;
            ++i;
        }

        mediumNode->left = buildTreeOnFlattenTree(size / 2, node);
        if (mediumNode->left != nullptr) {
            mediumNode->left->parent = mediumNode;
        }

        mediumNode->right = buildTreeOnFlattenTree((size - 1) / 2, mediumNode->right);
        if (mediumNode->right != nullptr) {
            mediumNode->right->parent = mediumNode;
        }
        mediumNode->sizeN = size;
        return mediumNode;
    }

    MyPair
    find(Node *node, int64_t value, Node *parent = nullptr, size_t size = 0) const {
        if (node == nullptr) {
            return {parent, size};
        } else if (node->key == value) {
            return {node, size + 1};
        } else if (value < node->key) {
            return find(node->left, value, node, size + 1);
        } else {
            return find(node->right, value, node, size + 1);
        }
    }

    void adoptionSon(Node *parent, Node *son, int64_t value) {
        if (parent == nullptr) {
            return;
        }
        if (parent->left != nullptr && parent->left->key == value) {
            parent->left = son;
        } else {
            parent->right = son;
        }
        if (son != nullptr) {
            son->parent = parent;
        }
    }

    int64_t size(Node * n) {
        if (n == nullptr) {
            return 0;
        }
        return n->sizeN;
    }

    int64_t findK(Node * n, std::size_t k) {
        if (k == size(n->right) + 1) {
            return n->key;
        }
        if (k > size(n->right) + 1) {
            return findK(n->left, k - size(n->right) - 1);
        }
        if (k < size(n->right) + 1) {
            return findK(n->right, k);
        }
        return 0;
    }
};







std::vector<std::size_t> gen_random_permutation(std::size_t i)
{
    std::vector<std::size_t> permutation(i);
    std::iota(permutation.begin(), permutation.end(), 0);
    std::shuffle(permutation.begin(), permutation.end(), std::mt19937(std::random_device{}()));
    return permutation;
}

int main() {
    auto * tree = new ScapegoatTree(0.75);
//    int64_t n;
//    scanf("%lli\n", &n);
//    int64_t a, b;
//    for (int64_t i = 0; i < n; ++i) {
//        scanf("%lli", &a);
//        scanf("%lli", & b);
//        if (a == 1) {
//            tree->insert(b);
//        } else if (a == -1) {
//            tree->remove(b);
//        } else {
//            printf("%lli", tree->findK(tree->root, b));
//            if (i != n - 1) {
//                printf("\n");
//            }
//        }
//    }
    for (int d = 0; d < 2; ++d) {


        std::vector<std::size_t> x = gen_random_permutation(5000);
        std::vector<std::size_t> del = gen_random_permutation(400);
        for (auto const i: x) {
           // std::cout << i << ", ";
            tree->insert(i);
        }
        //std::cout << std::endl;
        for (auto const i: del) {
            //std::cout << i << ", ";
            tree->remove(i);
        }
     //  std::cout << std::endl;
        for (int j = del.size(), k = x.size() - del.size(); j < x.size(); ++j, --k) {
            if (tree->findK(tree->root, k) != j) {
                std::cout << j << " " << k << std::endl;
                return tree->findK(tree->root, k);
            }
        }
    }
}
//    std::vector<std::size_t> x = gen_random_permutation(150);
//    std::vector<std::size_t> del = gen_random_permutation(150);
//    for (auto const i : x) {
//        std::cout << i << ", ";
//        tree->insert(i);
//    }
//    std::cout << std::endl;
//    for (auto const i : del) {
//        std::cout << i << ", ";
//        tree->remove(i);
//    }
//    1, 3, 2, 4, 0,
//            1, 2, 4,
//    tree->insert(1);
//    tree->insert(3);
//    tree->insert(2);
//    tree->insert(4);
//    tree->insert(0);
//
//    tree->remove(1);
//    tree->remove(2);
//    tree->remove(4);