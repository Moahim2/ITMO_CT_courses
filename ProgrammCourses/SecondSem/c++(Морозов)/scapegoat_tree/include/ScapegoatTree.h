#pragma once

#include <vector>

class ScapegoatTree
{
    struct Node
    {
        int key;
        Node * left = nullptr;
        Node * right = nullptr;
        Node * parent = nullptr;

        Node(int val, Node * parent);
    };

    Node * root = nullptr;
    std::size_t sizeTree = 0;
    double alpha = 0.75;

    std::pair<Node *, size_t> find(Node * node, int value, Node * parent = nullptr, size_t size = 0) const;

    void adoptionSon(Node * parent, Node * son, int value);

    int buildVectorValue(Node * node, size_t p, std::vector<int> & ans, Node * par = nullptr) const;

    Node * findScapegoat(Node * node) const;

    void cleanup(Node * n);

    size_t siblingSize(Node * node) const;
    size_t nodeSize(Node * node) const;

    Node * flattenTree(Node * newRoot, Node * head = nullptr);
    void checkRebuildTree(Node * scapegoat);
    Node * rebuildScapegoatTree(Node * scapegoat);
    Node * buildTreeOnFlattenTree(size_t size, Node * node);

public:
    ScapegoatTree() = default;
    ScapegoatTree(double alpha);

    bool contains(int value) const;
    bool insert(int value);
    bool remove(int value);

    size_t size() const;
    bool empty() const;

    std::vector<int> values() const;

    ~ScapegoatTree();
};
