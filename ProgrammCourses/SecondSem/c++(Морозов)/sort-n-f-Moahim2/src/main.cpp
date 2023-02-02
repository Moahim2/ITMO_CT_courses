#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>

namespace {

struct Capabilities
{
    Capabilities()
        : has_ignore_case()
        , has_numerical_sort()
    {
    }
    bool has_ignore_case : 1;
    bool has_numerical_sort : 1;
} options;

std::string_view get_input_file_name_and_processing_com_line_args(std::vector<std::string_view> com_lines)
{
    std::string_view input_file_name;

    for (std::size_t i = 1; i < com_lines.size(); ++i) {
        if (com_lines[i] == "-f" || com_lines[i] == "--ignore-case") {
            options.has_ignore_case = true;
        }
        else if (com_lines[i] == "-n" || com_lines[i] == "--numeric-sort" || com_lines[i] == "-nf" || com_lines[i] == "-fn") {
            options.has_numerical_sort = true;
        }
        else if (com_lines[i] != "-") {
            input_file_name = com_lines[i];
        }
    }
    return input_file_name;
}

auto const compare_with_ignore_case_option = [](std::string & a, std::string & b) {
    std::string copy_a(a);
    std::string copy_b(b);
    std::transform(copy_a.begin(), copy_a.end(), copy_a.begin(), tolower);
    std::transform(copy_b.begin(), copy_b.end(), copy_b.begin(), tolower);
    return copy_a < copy_b;
};

std::string::iterator skip_whitespace_and_minus(std::string & str, bool & minus, bool & is_num, std::size_t & count_n)
{
    auto it = str.begin();
    while (*it == ' ') {
        ++it;
    }
    if (it != str.end() && *it == '-') {
        ++it;
        minus = true;
    }
    if (it != str.end() && (std::isdigit(static_cast<unsigned char>(*it)) != 0)) {
        if (*it == '0') {
            is_num = false;
            ++count_n;
            return it;
        }
        is_num = true;
        auto copy_it = it;
        while (std::isdigit(static_cast<unsigned char>(*copy_it)) != 0) {
            ++copy_it;
            ++count_n;
        }
    }
    return it;
}

auto const compare_with_numerical_sort_option = [](std::string & a, std::string & b) {
    bool minus_a = false;
    bool minus_b = false;
    bool is_num_a = false;
    bool is_num_b = false;
    std::size_t count_n_a = 0;
    std::size_t count_n_b = 0;
    auto it_a = skip_whitespace_and_minus(a, minus_a, is_num_a, count_n_a);
    auto it_b = skip_whitespace_and_minus(b, minus_b, is_num_b, count_n_b);

    if (is_num_a != is_num_b || minus_a != minus_b) {
        return is_num_a ? minus_a : !minus_b;
    }

    if (!is_num_a) {
        return a < b;
    }
    if (count_n_a != count_n_b) {
        return minus_a ? count_n_a > count_n_b : count_n_a < count_n_b;
    }
    while (count_n_a != 0) {
        if (*it_a != *it_b) {
            return minus_a ? *it_a > *it_b : *it_a < *it_b;
        }
        ++it_a;
        ++it_b;
        --count_n_a;
    }

    if (*it_a++ == '.' && *it_b++ == '.') {
        const bool is_correct_dot_a = std::isdigit(static_cast<unsigned char>(*it_a)) != 0;
        const bool is_correct_dot_b = std::isdigit(static_cast<unsigned char>(*it_b)) != 0;
        if (is_correct_dot_a && is_correct_dot_b) {
            while (it_a != a.end() && it_b != b.end() && std::isdigit(static_cast<unsigned char>(*it_a)) != 0 && std::isdigit(static_cast<unsigned char>(*it_b)) != 0) {
                if (*it_a++ != *it_b++) {
                    return minus_a ? *(--it_a) > *(--it_b) : *(--it_a) < *(--it_b);
                }
            }
            if (std::isdigit(static_cast<unsigned char>(*it_a)) != 0 || std::isdigit(static_cast<unsigned char>(*it_b)) != 0) {
                return std::isdigit(static_cast<unsigned char>(*it_a)) != 0 ? minus_a : !minus_b;
            }
        }
        else if (is_correct_dot_a || is_correct_dot_b) {
            return is_correct_dot_a ? minus_a : !minus_b;
        }
    }
    return a < b;
};

std::vector<std::string> read_input_file(std::istream && input)
{
    std::vector<std::string> source;
    std::string tmp_str;
    while (std::getline(input, tmp_str)) {
        source.push_back(std::move(tmp_str));
    }
    return source;
}

void out_sorted_file(std::vector<std::string> & sorted_source)
{
    for (const auto & str : sorted_source) {
        std::cout << str << std::endl;
    }
}

void sort_and_out_input_file(std::string_view & input_file_name)
{
    std::vector<std::string> source = input_file_name.empty() ? read_input_file(std::move(std::cin))
                                                              : read_input_file(std::ifstream(input_file_name.data()));

    auto compare = options.has_numerical_sort ? compare_with_numerical_sort_option
            : options.has_ignore_case         ? compare_with_ignore_case_option
                                              : [](std::string & a, std::string & b) { return a < b; };
    std::sort(source.begin(), source.end(), compare);
    out_sorted_file(source);
}

} //anonymous namespace

int main(int argc, char ** argv)
{
    std::vector<std::string_view> com_arguments;
    com_arguments.reserve(argc);
    for (int i = 0; i < argc; ++i) {
        com_arguments.emplace_back(argv[i]);
    }
    std::string_view input_file_name = get_input_file_name_and_processing_com_line_args(com_arguments);
    sort_and_out_input_file(input_file_name);
}
