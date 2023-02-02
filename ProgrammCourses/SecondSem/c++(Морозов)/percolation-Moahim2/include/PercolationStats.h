#pragma once

#include <cstddef>
#include <vector>

struct PercolationStats
{
private:
    const std::size_t m_dimension;
    std::vector<double> m_results;

public:
    /**
     * Construct a new Percolation Stats object
     * @param dimension dimension of percolation grid
     * @param trials amount of experiments
     */
    PercolationStats(size_t dimension, size_t trials);

    /**
     * Returns mean of percolation threshold (x¯ from description)
     */
    double get_mean() const;

    /**
     * Returns standard deviation of percolation threshold (s from description)
     */
    double get_standard_deviation() const;

    /**
     * Returns log edge of condidence interval
     */
    double get_confidence_low() const;

    /**
     * Returns high edge of confidence interval
     */
    double get_confidence_high() const;

private:
    std::vector<std::size_t> gen_random_permutation() const;
    double make_experiment();

    void execute();
};
