#include "ScapegoatTree.h"

#include <climits>
#include <cmath>
#include <stdexcept> // for std::invalid_argument exception

ScapegoatTree::Node::Node(int val, Node * parent)
    : key(val)
    , parent(parent)
{
}

ScapegoatTree::ScapegoatTree(double alpha)
    : alpha(alpha)
{
    if (alpha < 0.5 || alpha > 1.0) {
        throw std::invalid_argument("Alpha must be in range [0.5; 1]");
    }
}

bool ScapegoatTree::ScapegoatTree::contains(int value) const
{
    return sizeTree != 0 && find(root, value).first->key == value;
}

bool ScapegoatTree::insert(int value)
{
    if (root == nullptr) {
        root = new Node(value, nullptr);
        ++sizeTree;
        return true;
    }

    auto p = find(root, value);
    if (p.first->key != value) {
        if (p.first->key < value) {
            p.first->right = new Node(value, p.first);
        }
        else {
            p.first->left = new Node(value, p.first);
        }
        ++sizeTree;
        if (p.second + 1 > floor(-std::log(sizeTree) / std::log(alpha))) {
            checkRebuildTree(findScapegoat(p.first));
        }
        return true;
    }
    return false;
}

bool ScapegoatTree::remove(int value)
{
    auto f = find(root, value);
    Node * recedingNode = f.first;
    if (recedingNode == nullptr || recedingNode->key != value) {
        return false;
    }
    Node * parent = recedingNode->parent;
    Node * tmp = nullptr;

    if (recedingNode->left == nullptr) {
        tmp = recedingNode->right;
    }
    else if (recedingNode->right == nullptr) {
        tmp = recedingNode->left;
    }

    if (recedingNode->left == nullptr || recedingNode->right == nullptr) {
        adoptionSon(parent, tmp, value);
    }
    else {
        tmp = recedingNode->right;
        while (tmp->left != nullptr) {
            tmp = tmp->left;
        }
        tmp->left = recedingNode->left;
        tmp->left->parent = tmp;
        if (recedingNode->right->key != tmp->key) {
            adoptionSon(tmp->parent, tmp->right, tmp->key);
            tmp->right = recedingNode->right;
            tmp->right->parent = tmp;
        }
        adoptionSon(parent, tmp, value);
    }
    if (parent == nullptr) {
        root = tmp;
    }
    delete recedingNode;
    --sizeTree;
    return true;
}

size_t ScapegoatTree::size() const
{
    return sizeTree;
}

bool ScapegoatTree::empty() const
{
    return sizeTree == 0;
}

std::vector<int> ScapegoatTree::values() const
{
    std::vector<int> ans(sizeTree);
    if (sizeTree == 0) {
        return ans;
    }
    std::size_t p = 0;
    buildVectorValue(root, p, ans);
    return ans;
}

ScapegoatTree::~ScapegoatTree()
{
    cleanup(root);
}

int ScapegoatTree::buildVectorValue(Node * node, size_t p, std::vector<int> & ans, Node * par) const
{
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

void ScapegoatTree::cleanup(Node * n)
{
    if (n == nullptr) {
        return;
    }
    cleanup(n->left);
    cleanup(n->right);
    delete n;
}

size_t ScapegoatTree::siblingSize(Node * node) const
{
    if (node->parent != nullptr && node->parent->left != nullptr && node->parent->right != nullptr) {
        return node->parent->left == node ? nodeSize(node->parent->right) : nodeSize(node->parent->left);
    }
    return 0;
}

size_t ScapegoatTree::nodeSize(Node * node) const
{
    if (node == nullptr) {
        return 0;
    }
    return nodeSize(node->left) + 1 + nodeSize(node->right);
}

ScapegoatTree::Node * ScapegoatTree::findScapegoat(Node * node) const
{
    std::size_t size = 1;
    std::size_t height = 0;
    while (node->parent != nullptr) {
        ++height;
        size_t totalSize = 1 + size + siblingSize(node);
        if (height >= floor(-std::log(totalSize) / std::log(alpha))) {
            return node->parent;
        }
        node = node->parent;
        size = totalSize;
    }
    return nullptr;
}

void ScapegoatTree::checkRebuildTree(Node * scapegoat)
{
    if (scapegoat == nullptr) {
        return;
    }
    if (scapegoat->parent != nullptr) {
        Node * parent = scapegoat->parent;
        adoptionSon(parent, rebuildScapegoatTree(scapegoat), scapegoat->key);
    }
    else {
        root = rebuildScapegoatTree(root);
        root->parent = nullptr;
    }
}

ScapegoatTree::Node * ScapegoatTree::rebuildScapegoatTree(Node * scapegoat)
{
    if (scapegoat == root) {
        return buildTreeOnFlattenTree(sizeTree, flattenTree(root));
    }
    Node * head = flattenTree(scapegoat);
    std::size_t size = 1;
    Node * tmp = head;
    while (tmp->right != nullptr) {
        ++size;
        tmp = tmp->right;
    }
    return buildTreeOnFlattenTree(size, head);
}

ScapegoatTree::Node * ScapegoatTree::flattenTree(Node * newRoot, Node * head)
{
    if (newRoot == nullptr) {
        return head;
    }
    newRoot->right = flattenTree(newRoot->right, head);
    return flattenTree(newRoot->left, newRoot);
}

ScapegoatTree::Node * ScapegoatTree::buildTreeOnFlattenTree(size_t size, Node * node)
{
    if (size == 1) {
        node->right = nullptr;
        node->left = nullptr;
        return node;
    }

    if (size == 2) {
        node->left = nullptr;
        node->right->left = nullptr;
        node->right->right = nullptr;
        node->right->parent = node;
        return node;
    }

    std::size_t i = 0;
    Node * mediumNode = node;
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
    return mediumNode;
}

std::pair<ScapegoatTree::Node *, size_t> ScapegoatTree::find(Node * node, int value, Node * parent, size_t size) const
{
    if (node == nullptr) {
        return std::make_pair(parent, size);
    }
    if (node->key == value) {
        return std::make_pair(node, size + 1);
    }
    return value < node->key ? find(node->left, value, node, size + 1) : find(node->right, value, node, size + 1);
}

void ScapegoatTree::adoptionSon(Node * parent, Node * son, int value)
{
    if (parent == nullptr) {
        return;
    }
    if (parent->left != nullptr && parent->left->key == value) {
        parent->left = son;
    }
    else {
        parent->right = son;
    }
    if (son != nullptr) {
        son->parent = parent;
    }
}
