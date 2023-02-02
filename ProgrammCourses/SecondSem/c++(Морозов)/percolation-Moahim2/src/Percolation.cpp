#include "Percolation.h"

Percolation::Cell::Cell(std::size_t row, std::size_t column, std::size_t dimension)
    : number(row * dimension + column)
    , max_row(row)
    , min_row(row)
{
}

Percolation::Percolation(size_t dimension)
    : m_dimension(dimension)
{
    m_parent.reserve(dimension * dimension);
    for (std::size_t i = 0; i < dimension; ++i) {
        for (std::size_t j = 0; j < dimension; ++j) {
            m_parent.emplace_back(i, j, dimension);
        }
    }
}

void Percolation::open(size_t row, size_t column)
{
    if (row < m_dimension && column < m_dimension && !m_parent[row * m_dimension + column].is_open) {
        make_set(row * m_dimension + column);
    }
}

bool Percolation::is_open(size_t row, size_t column) const
{
    return (row < m_dimension && column < m_dimension) && m_parent[row * m_dimension + column].is_open;
}

bool Percolation::is_full(size_t row, size_t column)
{
    return is_open(row, column) && find_set(row * m_dimension + column).min_row == 0;
}

size_t Percolation::get_numbet_of_open_cells() const
{
    return m_number_of_open_cells;
}

bool Percolation::has_percolation() const
{
    return m_has_percolation;
}

Percolation::Cell Percolation::find_set(const std::size_t x)
{
    if (m_parent[x].number != x) {
        m_parent[x] = find_set(m_parent[x].number);
    }
    return m_parent[x];
}

void Percolation::hang_cell(const std::size_t x, const std::size_t y)
{
    m_parent[x].number = y;
    m_parent[y].min_row = std::min(m_parent[x].min_row, m_parent[y].min_row);
    m_parent[y].max_row = std::max(m_parent[x].max_row, m_parent[y].max_row);
}

void Percolation::merge_sets(std::size_t x, std::size_t y)
{
    x = find_set(x).number;
    y = find_set(y).number;

    if (x == y) {
        return;
    }
    if (m_parent[x].rg == m_parent[y].rg) {
        m_parent[x].rg++;
    }

    if (m_parent[x].rg < m_parent[y].rg) {
        hang_cell(x, y);
    }
    else {
        hang_cell(y, x);
    }
}

void Percolation::merge_with_neighbor(const bool is_correct_neighbor, const std::size_t number_cell, const std::size_t number_neighbor)
{
    if (is_correct_neighbor && m_parent[number_neighbor].is_open) {
        merge_sets(number_cell, number_neighbor);
    }
}

void Percolation::make_set(const std::size_t x)
{
    ++m_number_of_open_cells;
    m_parent[x].is_open = true;

    merge_with_neighbor(x / m_dimension + 1 < m_dimension, x, x + m_dimension);
    merge_with_neighbor(x / m_dimension >= 1, x, x - m_dimension);
    merge_with_neighbor(x % m_dimension + 1 < m_dimension, x, x + 1);
    merge_with_neighbor(x % m_dimension >= 1, x, x - 1);

    if (find_set(x).max_row == m_dimension - 1 && find_set(x).min_row == 0) {
        m_has_percolation = true;
    }
}
