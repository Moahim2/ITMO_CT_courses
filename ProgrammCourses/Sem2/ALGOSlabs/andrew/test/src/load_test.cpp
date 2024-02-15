#include "searcher.h"

#include <gtest/gtest.h>

#include <algorithm>
#include <cctype>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <list>
#include <map>
#include <mutex>
#include <random>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

namespace {

constexpr std::size_t ControlN = 2;

template <class It>
void advance_with_limit(std::size_t num, It & it, const It & end)
{
    while (num != 0 && it != end) {
        ++it;
        --num;
    }
}

template <class... Args>
constexpr std::size_t count_args(Args &&... args)
{
    return sizeof...(args);
}

template <class It>
class SeqPrinter
{
    It m_begin;
    It m_end;
    std::string_view m_sep;
public:
    SeqPrinter(const It begin, const It end, const std::string_view sep = ", ")
        : m_begin(begin)
        , m_end(end)
        , m_sep(sep)
    {
    }

    std::ostream & print(std::ostream & strm) const
    {
        bool first = true;
        for (auto it = m_begin; it != m_end; ++it) {
            if (!first) {
                strm << m_sep;
            }
            else {
                first = false;
            }
            strm << *it;
        }
        return strm;
    }

    friend std::ostream & operator << (std::ostream & strm, const SeqPrinter & sp)
    {
        return sp.print(strm);
    }
};

template <class It>
SeqPrinter<It> sequence_printer(It begin, It end)
{
    return {begin, end};
}

template <class T>
auto split_tasks(const std::size_t n, const std::vector<T> & items)
{
    std::vector<std::pair<std::size_t, std::size_t>> tasks;
    const std::size_t part = items.size() / n;
    tasks.reserve(n);
    {
        std::size_t start = 0;
        for (std::size_t i = 0; i < n-1; ++i) {
            tasks.emplace_back(start, start + part);
            start += part;
        }
        tasks.emplace_back(start, items.size());
    }
    return tasks;
}

auto read_queries(const std::string & filename)
{
    std::vector<std::pair<std::string, std::size_t>> queries;
    std::ifstream f(filename);
    for (std::string line; std::getline(f, line); ) {
        const auto pos = line.find("\t");
        if (pos != line.npos && (pos + 1) < line.size()) {
            queries.emplace_back(line.substr(0, pos), std::stoul(line.substr(pos+1)));
        }
        else {
            std::cerr << "Bad queries file content: " << line << "\n";
        }
    }
    return queries;
}

auto read_doc_list(const std::string & filename, const std::filesystem::path & prefix)
{
    std::vector<std::filesystem::path> docs;
    std::ifstream f(filename);
    for (std::string line; std::getline(f, line); ) {
        docs.emplace_back(prefix / line);
    }
    return docs;
}

class Document
{
    Searcher::Filename m_filename;
    std::string_view m_name;
public:
    Document() = default;

    Document(Searcher::Filename && filename, std::string_view && name)
        : m_filename(std::move(filename))
        , m_name(name)
    { }

    std::ostream & print(std::ostream & strm) const
    {
        return strm << m_name;
    }

    friend std::ostream & operator << (std::ostream & strm, const Document & doc)
    {
        return doc.print(strm);
    }

    friend bool operator == (const Searcher::Filename & lhs, const Document & rhs)
    {
        return lhs == rhs.m_filename;
    }

    friend bool operator == (const Document & lhs, const Searcher::Filename & rhs)
    {
        return lhs.m_filename == rhs;
    }

    friend Searcher & operator += (Searcher & s, const Document & doc)
    {
        if (!doc.m_filename.empty()) {
            std::ifstream f(doc.m_filename);
            s.add_document(doc.m_filename, f);
        }
        return s;
    }

    friend Searcher & operator -= (Searcher & s, const Document & doc)
    {
        if (!doc.m_filename.empty()) {
            s.remove_document(doc.m_filename);
        }
        return s;
    }
};

template <class... Ds>
void load_docs(Searcher & s, Ds &&... ds)
{
    (s += ... += ds);
}

template <class... Ds>
void del_docs(Searcher & s, Ds &&... ds)
{
    (s -= ... -= ds);
}

#define DOC(x) const Document x{"test/etc/" #x ".txt", #x};
#include "list.inl"
#undef DOC

struct InvertedIndexSmallTest : ::testing::Test
{
    Searcher s;

    InvertedIndexSmallTest()
    {
        load_docs(s, Document{}
#define DOC(x) , x
#include "list.inl"
#undef DOC
                );
    }

    template <class... Ds>
    void remove(Ds &&... ds)
    {
        del_docs(s, std::forward<Ds>(ds)...);
    }
};

struct InvertedIndexLoadTest : ::testing::Test
{
    inline static Searcher s;
    inline static std::vector<std::pair<std::string, std::size_t>> queries;

    static void SetUpTestSuite()
    {
        queries = read_queries("test/etc/many_queries.txt");
        for (const auto & file : read_doc_list("test/etc/all_docs.txt", "test/etc")) {
            std::ifstream f(file);
            s.add_document(file.lexically_normal(), f);
        }
    }
};

} // anonymous namespace

TEST_F(InvertedIndexSmallTest, simple)
{
#define CHECK(query, ...) \
    do { \
        const auto [begin, end] = s.search(query); \
        for (const auto & doc : { __VA_ARGS__ }) { \
            EXPECT_EQ(1, std::count(begin, end, doc)) << doc; \
        } \
        EXPECT_EQ(count_args( __VA_ARGS__ ), std::distance(begin, end)) << "Found in " << sequence_printer(begin, end); \
    } while (false)
#define NOT_FOUND(query) \
    do { \
        const auto [begin, end] = s.search(query); \
        EXPECT_EQ(begin, end) << "Found in " << sequence_printer(begin, end); \
    } while (false)
    CHECK("theirs",
              A_Midsummer_Night_s_Dream
            , A_Princess_of_Mars
            , A_Study_In_Scarlet
            , A_Tale_of_Two_Cities
            , Alices_Adventures_in_Wonderland
            , Anne_Of_The_Island
            , Around_the_World_in_80_Days
            , David_Copperfield
            , Frankenstein
            , Heart_of_Darkness
            , House_of_Mirth
            , Ivanhoe
            , Leviathan
            , Macbeth
            , Memoirs_of_Fanny_Hill
            , Middlemarch
            , Moby_Dick
            , Our_Mutual_Friend
            , Paradise_Lost
            , Pride_and_Prejudice
            , Siddhartha
            , The_Adventures_of_Sherlock_Holmes
            , The_Age_of_Innocence
            , The_Call_of_the_Wild
            , The_Devils_Dictionary
            , The_Forsyte_Saga
            , The_Great_Gatsby
            , The_King_in_Yellow
            , The_Lady_of_the_Lake
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Lost_World
            , The_Mysteries_of_Udolpho
            , The_Old_Way
            , The_Picture_of_Dorian_Gray
            , The_Time_Machine
            , The_Voyage_of_the_Beagle
            , The_War_of_the_Worlds
            , The_Wind_in_the_Willows
            , The_Woman_in_White
            , This_Side_of_Paradise
            , Ulysses
            , Utilitarianism
            , Wuthering_Heights);
    CHECK("lantern",
              A_Gentleman_of_Leisure
            , A_Midsummer_Night_s_Dream
            , A_Study_In_Scarlet
            , A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , Anne_Of_The_Island
            , David_Copperfield
            , Dracula
            , Frankenstein
            , Great_Expectations
            , House_of_Mirth
            , Middlemarch
            , Moby_Dick
            , Our_Mutual_Friend
            , The_Adventures_of_Sherlock_Holmes
            , The_Canterville_Ghost
            , The_King_in_Yellow
            , The_Lair_of_the_White_Worm
            , The_Picture_of_Dorian_Gray
            , The_War_of_the_Worlds
            , The_Wind_in_the_Willows
            , The_Woman_in_White
            , Treasure_Island
            , Ulysses
            , Wuthering_Heights);
    CHECK("wight",
              Ivanhoe
            , Moby_Dick
            , Our_Mutual_Friend
            , Paradise_Lost
            , The_Adventures_of_Sherlock_Holmes
            , The_Devils_Dictionary
            , The_Lady_of_the_Lake
            , The_Legend_of_Sleepy_Hollow
            , The_Mysteries_of_Udolpho
            , Ulysses);
    CHECK("Prejudice",
              A_Gentleman_of_Leisure
            , Anne_Of_The_Island
            , David_Copperfield
            , Dracula
            , Frankenstein
            , Great_Expectations
            , Heart_of_Darkness
            , House_of_Mirth
            , Ivanhoe
            , Leviathan
            , Memoirs_of_Fanny_Hill
            , Metamorphosis
            , Middlemarch
            , Pride_and_Prejudice
            , The_Adventures_of_Sherlock_Holmes
            , The_Devils_Dictionary
            , The_Forsyte_Saga
            , The_Great_Gatsby
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Mysteries_of_Udolpho
            , The_Time_Machine
            , The_Voyage_of_the_Beagle
            , The_Woman_in_White
            , This_Side_of_Paradise
            , Ulysses
            , Utilitarianism
            , Wuthering_Heights);
    CHECK("Frankenstein", Frankenstein, The_Vampyre_A_Tale);
    CHECK("\"the horses\"",
              A_Gentleman_of_Leisure
            , A_Study_In_Scarlet
            , A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , Arms_and_the_Man
            , David_Copperfield
            , Dracula
            , Frankenstein
            , Great_Expectations
            , House_of_Mirth
            , Ivanhoe
            , Memoirs_of_Fanny_Hill
            , Middlemarch
            , Our_Mutual_Friend
            , Pride_and_Prejudice
            , The_Call_of_the_Wild
            , The_Forsyte_Saga
            , The_Great_Gatsby
            , The_King_in_Yellow
            , The_Lair_of_the_White_Worm
            , The_Legend_of_Sleepy_Hollow
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Mysteries_of_Udolpho
            , The_Voyage_of_the_Beagle
            , The_War_of_the_Worlds
            , The_Woman_in_White
            , Treasure_Island
            , Ulysses
            , Wuthering_Heights);
    CHECK("\"three months\"",
              A_Princess_of_Mars
            , A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , Around_the_World_in_80_Days
            , David_Copperfield
            , Frankenstein
            , Great_Expectations
            , Heart_of_Darkness
            , House_of_Mirth
            , Memoirs_of_Fanny_Hill
            , Middlemarch
            , Moby_Dick
            , Our_Mutual_Friend
            , Pride_and_Prejudice
            , The_Age_of_Innocence
            , The_Forsyte_Saga
            , The_Great_Gatsby
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Mysterious_Affair_at_Styles
            , The_Picture_of_Dorian_Gray
            , The_Voyage_of_the_Beagle
            , The_Woman_in_White
            , This_Side_of_Paradise
            , Ulysses
            , Wuthering_Heights);
    CHECK("\"the horses\" three months",
              A_Gentleman_of_Leisure
            , A_Study_In_Scarlet
            , A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , Arms_and_the_Man
            , David_Copperfield
            , Dracula
            , Frankenstein
            , Great_Expectations
            , House_of_Mirth
            , Ivanhoe
            , Memoirs_of_Fanny_Hill
            , Middlemarch
            , Our_Mutual_Friend
            , Pride_and_Prejudice
            , The_Call_of_the_Wild
            , The_Forsyte_Saga
            , The_Great_Gatsby
            , The_King_in_Yellow
            , The_Lair_of_the_White_Worm
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Mysteries_of_Udolpho
            , The_Voyage_of_the_Beagle
            , The_War_of_the_Worlds
            , The_Woman_in_White
            , Treasure_Island
            , Ulysses
            , Wuthering_Heights);
    CHECK("\"the horses\" \"three months\"",
              A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , David_Copperfield
            , Frankenstein
            , Great_Expectations
            , House_of_Mirth
            , Memoirs_of_Fanny_Hill
            , Middlemarch
            , Our_Mutual_Friend
            , Pride_and_Prejudice
            , The_Forsyte_Saga
            , The_Great_Gatsby
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Voyage_of_the_Beagle
            , The_Woman_in_White
            , Ulysses
            , Wuthering_Heights);
    CHECK("\"the horses\" \"three months\" to know what he had to",
              A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , David_Copperfield
            , Frankenstein
            , Great_Expectations
            , House_of_Mirth
            , Memoirs_of_Fanny_Hill
            , Middlemarch
            , Our_Mutual_Friend
            , Pride_and_Prejudice
            , The_Forsyte_Saga
            , The_Great_Gatsby
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Voyage_of_the_Beagle
            , The_Woman_in_White
            , Ulysses
            , Wuthering_Heights);
    CHECK("\"the horses\" \"three months\" \"to know what he had to\"", Pride_and_Prejudice);
    CHECK("\"to order the horses\"", Frankenstein);
    CHECK("\"a subject for\"",
              A_Tale_of_Two_Cities
            , David_Copperfield
            , Frankenstein
            , Leviathan
            , Memoirs_of_Fanny_Hill
            , Middlemarch
            , The_Forsyte_Saga);
    CHECK("\"my brother No one\"", Frankenstein);
    CHECK("a to the and an of the I in that when was his whom you this where what which he she it",
              _2_B_R_0_2_B
            , A_Gentleman_of_Leisure
            , A_Midsummer_Night_s_Dream
            , A_Modest_Proposal
            , A_Princess_of_Mars
            , A_Study_In_Scarlet
            , A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , Alices_Adventures_in_Wonderland
            , Anne_Of_The_Island
            , Arms_and_the_Man
            , Around_the_World_in_80_Days
            , David_Copperfield
            , Dracula
            , Frankenstein
            , Great_Expectations
            , Heart_of_Darkness
            , Homecoming_Horde
            , House_of_Mirth
            , Ivanhoe
            , Lamia
            , Leviathan
            , Macbeth
            , Memoirs_of_Fanny_Hill
            , Menace_From_Vega
            , Metamorphosis
            , Middlemarch
            , Moby_Dick
            , Our_Mutual_Friend
            , Paradise_Lost
            , Pride_and_Prejudice
            , Salome
            , Second_Variety
            , Siddhartha
            , The_Adventures_of_Sherlock_Holmes
            , The_Age_of_Innocence
            , The_Big_Trip_Up_Yonder
            , The_Call_of_the_Wild
            , The_Canterville_Ghost
            , The_Devils_Dictionary
            , The_Eyes_Have_It
            , The_Forsyte_Saga
            , The_Gift_of_the_Magi
            , The_Great_Gatsby
            , The_King_in_Yellow
            , The_Lady_of_the_Lake
            , The_Lair_of_the_White_Worm
            , The_Legend_of_Sleepy_Hollow
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Lost_World
            , The_Mysteries_of_Udolpho
            , The_Mysterious_Affair_at_Styles
            , The_Old_Way
            , The_Picture_of_Dorian_Gray
            , The_Strange_Case_Of_Dr_Jekyll_And_Mr_Hyde
            , The_Tale_Of_Benjamin_Bunny
            , The_Tale_of_Peter_Rabbit
            , The_Time_Machine
            , The_Vampyre_A_Tale
            , The_Voyage_of_the_Beagle
            , The_War_of_the_Worlds
            , The_Waste_Land
            , The_Wind_in_the_Willows
            , The_Woman_in_White
            , This_Side_of_Paradise
            , Treasure_Island
            , Ulysses
            , Voyage_to_Procyon
            , Wuthering_Heights);
    CHECK("a to the and an of the I in that when was his whom you this where won what which he she it",
              A_Gentleman_of_Leisure
            , A_Midsummer_Night_s_Dream
            , A_Princess_of_Mars
            , A_Study_In_Scarlet
            , A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , Alices_Adventures_in_Wonderland
            , Anne_Of_The_Island
            , Arms_and_the_Man
            , Around_the_World_in_80_Days
            , David_Copperfield
            , Dracula
            , Frankenstein
            , Great_Expectations
            , Homecoming_Horde
            , House_of_Mirth
            , Ivanhoe
            , Lamia
            , Leviathan
            , Macbeth
            , Memoirs_of_Fanny_Hill
            , Metamorphosis
            , Middlemarch
            , Our_Mutual_Friend
            , Paradise_Lost
            , Pride_and_Prejudice
            , Second_Variety
            , Siddhartha
            , The_Adventures_of_Sherlock_Holmes
            , The_Age_of_Innocence
            , The_Call_of_the_Wild
            , The_Forsyte_Saga
            , The_King_in_Yellow
            , The_Lady_of_the_Lake
            , The_Lair_of_the_White_Worm
            , The_Legend_of_Sleepy_Hollow
            , The_Lost_World
            , The_Mysteries_of_Udolpho
            , The_Mysterious_Affair_at_Styles
            , The_Old_Way
            , The_Vampyre_A_Tale
            , The_Voyage_of_the_Beagle
            , The_Wind_in_the_Willows
            , The_Woman_in_White
            , This_Side_of_Paradise
            , Ulysses);
    CHECK("\"Volunteers and financial support to provide volunteers with the assistance they need, are critical to reaching Project Gutenberg-tm's\"",
              A_Gentleman_of_Leisure
            , A_Midsummer_Night_s_Dream
            , A_Modest_Proposal
            , A_Tale_of_Two_Cities
            , Adventures_of_Huckleberry_Finn
            , Alices_Adventures_in_Wonderland
            , Arms_and_the_Man
            , Come_Into_My_Brain
            , David_Copperfield
            , Dracula
            , Great_Expectations
            , Heart_of_Darkness
            , Homecoming_Horde
            , Leviathan
            , Macbeth
            , Memoirs_of_Fanny_Hill
            , Menace_From_Vega
            , Metamorphosis
            , Middlemarch
            , Moby_Dick
            , Never_Trust_a_Thief
            , Pride_and_Prejudice
            , Salome
            , Second_Variety
            , The_Adventures_of_Sherlock_Holmes
            , The_Big_Trip_Up_Yonder
            , The_Call_of_the_Wild
            , The_Eyes_Have_It
            , The_Gift_of_the_Magi
            , The_Great_Gatsby
            , The_King_in_Yellow
            , The_Lady_of_the_Lake
            , The_Life_and_Adventures_of_Robinson_Crusoe
            , The_Mysteries_of_Udolpho
            , The_Old_Way
            , The_Picture_of_Dorian_Gray
            , The_Time_Machine
            , The_Vampyre_A_Tale
            , The_Voyage_of_the_Beagle
            , The_War_of_the_Worlds
            , The_Waste_Land
            , The_Wind_in_the_Willows
            , Treasure_Island
            , Ulysses
            , Voyage_to_Procyon
            , Wuthering_Heights);

    remove(Frankenstein, Leviathan, Memoirs_of_Fanny_Hill, The_Forsyte_Saga);
    NOT_FOUND("ksfhiwefhliwehapoheioi");
    NOT_FOUND("\"to order the horses\"");
    NOT_FOUND("\"my brother No one\"");
    CHECK("Frankenstein", The_Vampyre_A_Tale);
    CHECK("\"a subject for\"",
              A_Tale_of_Two_Cities
            , David_Copperfield
            , Middlemarch);
}

TEST_F(InvertedIndexSmallTest, many)
{
    const auto expected = read_queries("test/etc/queries.txt");
    for (const auto & [query, expected_number] : expected) {
        const auto [begin, end] = s.search(std::string{query});
        EXPECT_EQ(expected_number, std::distance(begin, end)) << query;
    }
}

TEST_F(InvertedIndexSmallTest, parallel_light)
{
    using S = std::string_view;
    std::map<std::string_view, std::size_t> expected = {
        {S{"a"}, 79},
        {S{"the"}, 79},
        {S{"whom"}, 73},
        {S{"theirs"}, 43},
        {S{"person"}, 73},
        {S{"castle"}, 32},
        {S{"car"}, 26},
        {S{"evidence"}, 51},
        {S{"vampyre"}, 1},
        {S{"vampire"}, 7},
        {S{"ghoul"}, 3},
        {S{"vampire ghoul"}, 2},
        {S{"un-dead"}, 1},
        {S{"ghost"}, 39},
        {S{"wight"}, 10},
        {S{"lantern"}, 25},
        {S{"\"to me\""}, 64},
        {S{"\"for you\""}, 60},
        {S{"\"where to\""}, 36},
        {S{"to me"}, 74},
        {S{"for you"}, 76},
        {S{"where to"}, 76},
        {S{"order the horses"}, 45},
        {S{"\"order the horses\""}, 1},
        {S{"all"}, 76},
        {S{"pack"}, 40},
        {S{"most effective"}, 18},
        {S{"\"most effective\""}, 5},
        {S{"that Hendricks"}, 1},
        {S{"that Hendriks"}, 0},
        {S{"them"}, 73},
        {S{"begin"}, 54},
        {S{"lid down"}, 28},
        {S{"\"lid down\""}, 1},
        {S{"but we"}, 73},
        {S{"\"but we\""}, 73},
        {S{"don't know"}, 61},
        {S{"\"don't know\""}, 44},
        {S{"Klaus stood"}, 1},
        {S{"Kaus stood"}, 0},
        {S{"\"Klaus stood\""}, 1},
        {S{"Stop"}, 57},
        {S{"was"}, 73},
        {S{"relays"}, 3},
        {S{"be a"}, 73},
        {S{"\"be a\""}, 62},
        {S{"forth now"}, 72},
        {S{"\"forth now\""}, 3},
        {S{"deductible to"}, 73},
        {S{"\"deductible to\""}, 73},
    };
    const std::size_t N = 6;
    const std::size_t part = expected.size() / N;
    std::vector<std::vector<std::string_view>> tasks(N);
    {
        auto it = expected.begin();
        const auto end = expected.end();
        for (std::size_t i = 0; i < N; ++i) {
            auto & current = tasks[i];
            for (std::size_t j = 0; j < part && it != end; ++j, ++it) {
                current.push_back(it->first);
            }
        }
        while (it != end) {
            tasks.back().push_back(it->first);
            ++it;
        }
    }
    std::map<std::string_view, std::size_t> results;
    std::mutex mutex;
    std::vector<std::thread> threads;
    threads.reserve(N);
    for (std::size_t i = 0; i < N; ++i) {
        threads.emplace_back([&mutex, &results, &task = tasks[i], &searcher = s] () {
                std::map<std::string_view, std::size_t> result;
                for (const auto & query : task) {
                    const auto [begin, end] = searcher.search(std::string{query});
                    result[query] = std::distance(begin, end);
                }
                std::lock_guard g(mutex);
                results.merge(result);
            });
    }
    for (auto & t : threads) {
        t.join();
    }
    for (const auto & [query, expected_num] : expected) {
        EXPECT_EQ(expected_num, results[query]) << query;
    }
}

TEST_F(InvertedIndexSmallTest, parallel_heavy)
{
    using S = std::string_view;
    std::map<std::string_view, std::size_t> expected = {
        {S{"little and the blood was \"There lay the\" \"seeing so\" \"seek him\""}, 1},
        {S{"to endure, \"little and the blood was\" :seeing so may not think"}, 1},
        {S{"\"caught the boat to\" white there lay the \"to endure\" \"here an opening\" \"further when\" \"all the\""}, 1},
        {S{"to endure in seeing so \"little and the\" blood was \"she grew\" \"time - was\" give me!"}, 1},
        {S{"\"and therefore\" selfish lips to rub. \"to endure in seeing so\" \"blood was\""}, 1},
        {S{"may not think it \"grew whiter and\", \"something like\" \"If so\" \"seek him\" \"and there are\""}, 1},
        {S{"\"long leather one, something like\" blood was respectful and polite \"seek him further when\" \"selfish and therefore\" dark"}, 0},
        {S{"\"If so, time was\" nigh little and begone \"may not think\" \"which the\" tempest"}, 0},
        {S{"\"rub her lips as so\" the blood was warm glistening \"we seek\" \"all the\" \"use it\" beauty"}, 0},
        {S{"\"we seek him further when\" came towards us now to tell where hung a door \"Give me\""}, 1},
        {S{"\"selfish and therefore\" \"the shoe down\" \"pledge him\" that we continued thus"}, 0},
        {S{"\"she grew whiter and\" \"there lay\" \"time was\" \"may not\" \"think it\" \"had drunk\" \"here an\" \"in seeing so\" \"give me\" \"her beauty up\" \"for I was\""}, 0},
        {S{"\"the bliss!\" and him was great"}, 4},
        {S{"\"Give me\" the hand of reason \"for your\""}, 47},
        {S{"\"had drunk her beauty up\" \"give me\""}, 1},
        {S{"\"portal door, where hung a\""}, 1},
        {S{"\"may not think it\" further pleasure and the bliss, had drunk her beauty up \"something like\" \"said he\""}, 1},
        {S{"\"And pledge him\" pleasure and beauty"}, 1},
        {S{"\"things you can do with\" one another are many \"therefore\""}, 51},
        {S{"\"saw them for\" what they are and more"}, 3},
        {S{"\"for I was\" healthy and young "}, 18},
        {S{"\"white foam which the tempest\" boat to long opening \"as so\" \"saw here\""}, 1},
        {S{"\"will use it well\""}, 1},
        {S{"long leather one, something like \"the Count\" \"command all the\" \"If so, time was\" \"rub her lips as\" \"and the blood was\" to endure \"white foam\""}, 1},
        {S{"\"that we continued thus\""}, 1},
        {S{"\"came towards us\" with fear and respect \"and there are\""}, 2},
        {S{"\"wear a more respectful\""}, 1},
        {S{"\"saw here an opening\" \"little and the\" \"time was\" \"if so\" \"a more\" respectful and polite \"and therefore\""}, 0},
        {S{"\"With pleasure said he\""}, 1},
        {S{"\"and there are\" \"for your\" \"give me\""}, 21},
        {S{"\"the shoe down\""}, 1},
        {S{"\"she grew whiter and\" her beauty came towards us with pleasure had unposted and there was the \"white foam\""}, 0},
        {S{"\"there was an inner meaning\""}, 1},
        {S{"rub her lips as so the blood was warm glistening \"and therefore\" selfish whiter which hung a pledge"}, 1},
        {S{"\"a good sort of fellow\""}, 1},
        {S{"\"It almost seemed to\" show little \"and for\""}, 2},
        {S{"\"end would be none\""}, 1},
        {S{"caught the boat to \"little and the\" where may: something like, \"and therefore\"!"}, 3},
        {S{"\"I have now to tell\""}, 1},
        {S{"\"There lay the\" all the things \"may not\" \"in seeing\""}, 1},
        {S{"\"had unposted his pen\""}, 1},
        {S{"\"command all the\" meaner things \"the blood was\" \"her lips\" there lay caught in the darkness fall upon us"}, 1},
        {S{"\"and there was the\""}, 14},
    };
    const std::size_t N = 6;
    const std::size_t part = expected.size() / N;
    std::vector<std::vector<std::string_view>> tasks(N);
    {
        auto it = expected.begin();
        const auto end = expected.end();
        for (std::size_t i = 0; i < N; ++i) {
            auto & current = tasks[i];
            for (std::size_t j = 0; j < part && it != end; ++j, ++it) {
                current.push_back(it->first);
            }
        }
        while (it != end) {
            tasks.back().push_back(it->first);
            ++it;
        }
    }
    std::map<std::string_view, std::size_t> results;
    std::mutex mutex;
    std::vector<std::thread> threads;
    threads.reserve(N);
    for (std::size_t i = 0; i < N; ++i) {
        threads.emplace_back([&mutex, &results, &task = tasks[i], &searcher = s] () {
                std::map<std::string_view, std::size_t> result;
                for (const auto & query : task) {
                    const auto [begin, end] = searcher.search(std::string{query});
                    result[query] = std::distance(begin, end);
                }
                std::lock_guard g(mutex);
                results.merge(result);
            });
    }
    for (auto & t : threads) {
        t.join();
    }
    for (const auto & [query, expected_num] : expected) {
        EXPECT_EQ(expected_num, results[query]) << query;
    }
}

TEST_F(InvertedIndexSmallTest, parallel_many)
{
    const std::size_t N = 4, K = 30, M = 2;
    const auto queries = read_queries("test/etc/queries.txt");
    std::mt19937_64 rnd(0); // always the same sequence
    std::vector<std::vector<std::size_t>> tasks;
    tasks.reserve(N);
    for (std::size_t i = 0; i < N; ++i) {
        auto & indices = tasks.emplace_back(K * queries.size());
        for (std::size_t j = 0; j < K; ++j) {
            std::iota(indices.begin() + j * queries.size(), indices.begin() + (j + 1) * queries.size(), 0);
        }
        std::shuffle(indices.begin(), indices.end(), rnd);
    }
    std::vector<std::thread> threads;
    threads.reserve(N);
    int acc = 0;
    for (const auto & task : tasks) {
        threads.emplace_back([&acc, &task, &searcher = s, &queries = queries] () {
                for (const auto i : task) {
                    auto [begin, end] = searcher.search(queries[i].first);
                    advance_with_limit(M, begin, end);
                    if (begin != end && begin->size() > 0) {
                        ++acc;
                    }
                }
            });
    }
    for (auto & t : threads) {
        t.join();
    }
    EXPECT_LT(0, acc);
}

TEST_F(InvertedIndexLoadTest, many)
{
    const std::size_t N = 4;
    const auto tasks = split_tasks(N, queries);
    struct Misstep
    {
        std::string_view query;
        std::size_t expected;

        Misstep(const std::string & query_, const std::size_t expected_)
            : query(query_)
            , expected(expected_)
        { }
    };
    std::list<std::vector<Misstep>> missteps;
    std::mutex mutex;
    std::vector<std::thread> threads;
    threads.reserve(N);
    for (const auto [from, to] : tasks) {
        threads.emplace_back([&mutex, &all_missteps = missteps, &searcher = s, from = from, to = to] () mutable {
                std::vector<Misstep> missteps;
                missteps.reserve(to - from);
                while (from != to) {
                    const auto & [query, expected] = queries[from];
                    const auto [begin, end] = searcher.search(query);
                    if (begin != end) {
                        if (expected == 0) {
                            missteps.emplace_back(query, expected);
                        }
                    }
                    else {
                        if (expected != 0) {
                            missteps.emplace_back(query, expected);
                        }
                    }
                    ++from;
                }
                if (!missteps.empty()) {
                    std::lock_guard g(mutex);
                    all_missteps.emplace_back(std::move(missteps));
                }
            });
    }
    for (auto & t : threads) {
        t.join();
    }
    for (const auto & ms : missteps) {
        for (const auto & m : ms) {
            if (m.expected != 0) {
//                EXPECT_EQ(m.expected, 0) << m.query;
            }
            else {
//                ADD_FAILURE() << "Expected empty results for /" << m.query << "/";
            }
        }
    }
}

TEST_F(InvertedIndexLoadTest, timing)
{
    const std::size_t N = 4, K = 4;
    std::vector<std::vector<std::size_t>> tasks;
    tasks.reserve(N);
    std::mt19937_64 rnd(0); // always the same sequence
    for (std::size_t i = 0; i < N; ++i) {
        auto & indices = tasks.emplace_back(K * queries.size());
        for (std::size_t j = 0, start = 0; j < K; ++j, start += queries.size()) {
            std::iota(indices.begin() + start, indices.begin() + start + queries.size(), 0);
        }
        std::shuffle(indices.begin(), indices.end(), rnd);
    }
    std::vector<std::thread> threads;
    threads.reserve(N);
    int acc = 0;
    const auto t1 = std::chrono::high_resolution_clock::now();
    for (const auto & task : tasks) {
        threads.emplace_back([&acc, &task, &searcher = s] () {
                for (const auto i : task) {
                    auto [begin, end] = searcher.search(queries[i].first);
                    advance_with_limit(ControlN, begin, end);
                    if (begin != end) {
                        ++acc;
                    }
                }
            });
    }
    for (auto & t : threads) {
        t.join();
    }
    const auto t2 = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> diff = t2 - t1;
    EXPECT_LT(0, acc);
    EXPECT_GT(40, diff.count()) << "Search took too long";
}
