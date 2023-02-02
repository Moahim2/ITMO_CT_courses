#include "Percolation.h"
#include "PercolationStats.h"

#include <iostream>

int main()
{
    const size_t dimension = 5;
    const size_t trials = 100;
    PercolationStats percolation_stats(dimension, trials);
    std::cout << "mean = " << percolation_stats.get_mean() << std::endl;

    return 0;
}
