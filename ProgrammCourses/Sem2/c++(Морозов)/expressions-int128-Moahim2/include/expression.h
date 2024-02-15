#pragma once

#include "int128.h"

#include <map>
#include <ostream>
#include <string>

struct Expression
{
    virtual Int128 eval(std::map<std::string, Int128> const & values = {}) const = 0;

    virtual Expression * clone() const = 0;

    friend std::ostream & operator<<(std::ostream & out, const Expression & expression);
};
