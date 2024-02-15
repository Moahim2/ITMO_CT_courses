#include "PercolationStats.h"

#include "Percolation.h"

#include <functional>
#include <random>

std::mt19937 m_rand_engine(std::random_device{}()); // решил пока в итоге оставить просто так

PercolationStats::PercolationStats(size_t dimension, size_t trials)
    : m_dimension(dimension)
    , m_results(trials)
{
    execute();
}

double PercolationStats::get_mean() const
{
    double ans = 0.0;
    for (auto const result : m_results) {
        ans += result;
    }
    return ans / m_results.size();
}

double PercolationStats::get_standard_deviation() const
{
    double average_percolation_threshold = get_mean();
    double ans = 0.0;
    for (auto const result : m_results) {
        ans += std::pow(result - average_percolation_threshold, 2);
    }
    return std::sqrt(ans / (m_results.size() - 1));
}

double PercolationStats::get_confidence_low() const
{
    return get_mean() - ((1.96 * get_standard_deviation()) / std::sqrt(m_results.size()));
}

double PercolationStats::get_confidence_high() const
{
    return get_mean() + ((1.96 * get_standard_deviation()) / std::sqrt(m_results.size()));
}

std::vector<std::size_t> PercolationStats::gen_random_permutation() const
{
    std::vector<std::size_t> permutation(m_dimension * m_dimension);
    std::iota(permutation.begin(), permutation.end(), 0);
    std::shuffle(permutation.begin(), permutation.end(), m_rand_engine);
    return permutation;
}

double PercolationStats::make_experiment()
{
    std::vector<std::size_t> random_input_data = gen_random_permutation();
    Percolation percolation = Percolation(m_dimension);
    for (std::size_t i = 0; !percolation.has_percolation(); ++i) {
        percolation.open(random_input_data[i] / m_dimension, random_input_data[i] % m_dimension);
    }
    return percolation.get_numbet_of_open_cells() / static_cast<double>(m_dimension * m_dimension);
}

void PercolationStats::execute()
{
    for (auto & result : m_results) {
        result = make_experiment();
    }
}
