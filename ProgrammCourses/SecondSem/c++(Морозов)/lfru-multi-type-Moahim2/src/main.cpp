#include "allocator.h"
#include "cache.h"

#include <iostream>
#include <string>

namespace {

struct String
{
    std::string data;
    bool marked = false;

    String(const std::string & key)
        : data(key)
    {
    }

    bool operator==(const std::string & other) const
    {
        return data == other;
    }

    [[maybe_unused]] friend std::ostream & operator<<(std::ostream & strm, const String & str)
    {
        //ф-ция вроде не участвует в тестах, а что она должна делать не очень понял)
        return strm << str.data;
    }
};

using TestCache = Cache<std::string, String, AllocatorWithPool>;

} // anonymous namespace

int main()
{
    TestCache cache(9, 18 * sizeof(String), std::initializer_list<std::size_t>{sizeof(String)});
    std::string line;
    while (std::getline(std::cin, line)) {
        auto & s = cache.get<String>(line);
        if (s.marked) {
            std::cout << "known" << std::endl;
        }
        s.marked = true;
    }
    std::cout << "\n"
              << cache << std::endl;
}
