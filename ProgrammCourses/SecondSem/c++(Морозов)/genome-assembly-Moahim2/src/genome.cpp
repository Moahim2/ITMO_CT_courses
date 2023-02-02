#include "genome.h"

#include "genomeCalculations.h"

#include <algorithm>

namespace genome {

std::string assembly(const size_t k, const std::vector<std::string> & reads)
{
    if (reads.empty() || k == 0) {
        return "";
    }
    GenomeCalculations genomeCalculations(k, reads);
    return genomeCalculations.get_assembling_genome();
}

} // namespace genome

GenomeCalculations::Node::Node(const std::string & fragment)
    : fragment(fragment)
{
}

GenomeCalculations::GenomeCalculations(const std::size_t k, const std::vector<std::string> & reads)
{
    make_genome_graph_struct(k, reads);
}

std::size_t GenomeCalculations::make_node_and_return_index(const std::string & fragment)
{
    const auto [element, result] = m_encoding_fragments.try_emplace(fragment, m_graph.size());
    if (result) {
        m_graph.emplace_back(element->first);
    }
    return element->second;
}

void GenomeCalculations::make_genome_graph_struct(const std::size_t len_fragment, const std::vector<std::string> & fragments)
{
    const std::size_t count_parts_in_fragment = fragments.front().size() - len_fragment;

    std::size_t parent_i;
    std::size_t child_i;
    for (auto const & fragment : fragments) {
        parent_i = make_node_and_return_index(fragment.substr(0, len_fragment));
        for (std::size_t j = 1; j < count_parts_in_fragment + 1; ++j) {
            child_i = make_node_and_return_index(fragment.substr(j, len_fragment));
            Node & parent = m_graph[parent_i];
            parent.children.push_back(child_i);
            --parent.deg;
            ++m_graph[child_i].deg;
            parent_i = child_i;
        }
    }
}

std::vector<std::size_t> GenomeCalculations::find_Eulerian_path()
{
    const std::size_t start_Eulerian_path = std::find_if(m_graph.begin(), m_graph.end(), [](const Node & node) { return node.deg == -1; }) - m_graph.begin();
    std::vector<std::size_t> stack{start_Eulerian_path};
    std::vector<std::size_t> path;

    stack.reserve(m_graph.size());
    path.reserve(m_graph.size());

    std::size_t current_node_i;
    while (!stack.empty()) {
        current_node_i = stack.back();
        Node & current_node = m_graph[current_node_i];
        if (current_node.children.empty()) {
            path.push_back(current_node_i);
            stack.pop_back();
        }
        else {
            stack.push_back(current_node.children.back());
            current_node.children.pop_back();
        }
    }
    return path;
}

std::string GenomeCalculations::get_assembling_genome()
{
    auto path = find_Eulerian_path();

    std::string genome = m_graph[path.back()].fragment.data();
    genome.reserve(m_graph.front().fragment.size() + path.size() - 1);
    for (auto it = --path.end(); it-- != path.begin();) {
        genome.push_back(m_graph[*it].fragment.back());
    }
    return genome;
}
