#include "hash_map.h"

#include <iostream>

namespace {

struct Marker
{
    std::size_t number;

    Marker(const std::size_t n)
        : number(n)
    {
    }

    Marker & operator=(const std::size_t n)
    {
        number = n;
    }
};

} // anonymous namespace

int main()
{
    HashMap<int, Marker> map;
    std::size_t i = 0;
    while (std::cin) {
        int x;
        std::cin >> x;
        if (map.contains(x)) {
            std::cout << "*\n";
        }
        else {
            std::cout << "-\n";
        }
        map.insert_or_assign(x, i++);
    }
}
