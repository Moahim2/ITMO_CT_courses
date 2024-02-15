#pragma once

#include <cstddef>
#include <vector>

struct Percolation
{
private:
    struct Cell
    {
        bool is_open = false;
        std::size_t rg = 1;
        std::size_t number;
        std::size_t max_row;
        std::size_t min_row;

        Cell(std::size_t row, std::size_t column, std::size_t dimension);
    };

private:
    const std::size_t m_dimension;
    std::vector<Cell> m_parent;

    bool m_has_percolation = false;
    std::size_t m_number_of_open_cells = 0;

private:
    Cell find_set(const std::size_t x);
    void hang_cell(const std::size_t x, const std::size_t y);
    void merge_sets(std::size_t x, std::size_t y);
    void merge_with_neighbor(const bool is_correct_neighbor, const std::size_t number_cell, const std::size_t number_neighbor);
    void make_set(const std::size_t x);

public:
    /**
     * Construct a new Percolation object
     * @param dimension dimension of percolation table
     */
    Percolation(size_t dimension);

    /**
     * Opens the cell[row, column] if it's not opened already
     * @param row row index
     * @param column column index
     */
    void open(size_t row, size_t column);

    /**
     * Checks if cell[row, column] is open
     * @param row row index
     * @param column column index
     * @return true cell is open
     * @return false cell is blocked
     */
    bool is_open(size_t row, size_t column) const;

    /**
     * Checks if cell[row, column] is full
     * @param row row index
     * @param column column index
     * @return true cell is full
     * @return false cell is not full
     */
    bool is_full(size_t row, size_t column);

    /**
     * Gets number of open cells in table
     * @return number of open cells
     */
    size_t get_numbet_of_open_cells() const;

    /**
     * Checks if system percolates
     * @return true system percolates
     * @return false system doesn't percolate
     */
    bool has_percolation() const;
};
