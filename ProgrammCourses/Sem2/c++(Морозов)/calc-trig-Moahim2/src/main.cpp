#include "calc.h"

#include <iostream>
#include <string>

int main()
{
    double current = 0;
    bool rad_on = false;
    for (std::string line; std::getline(std::cin, line);) {
        current = process_line(current, rad_on, line);
        std::cout << current << std::endl;
    }
}
