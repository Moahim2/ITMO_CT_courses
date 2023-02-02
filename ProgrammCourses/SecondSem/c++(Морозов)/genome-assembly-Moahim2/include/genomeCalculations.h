#pragma once

#include <unordered_map>

struct GenomeCalculations
{
private:
    struct Node
    {
        std::string_view fragment;
        long long deg = 0;
        std::vector<std::size_t> children;

        Node(const std::string & fragment);
    };

private:
    std::unordered_map<std::string, std::size_t> m_encoding_fragments;
    std::vector<Node> m_graph;

private:
    std::size_t make_node_and_return_index(const std::string & fragment);
    void make_genome_graph_struct(std::size_t len_fragment, const std::vector<std::string> & fragments);
    std::vector<std::size_t> find_Eulerian_path();

public:
    std::string get_assembling_genome();

    GenomeCalculations(std::size_t k, const std::vector<std::string> & reads);
};
