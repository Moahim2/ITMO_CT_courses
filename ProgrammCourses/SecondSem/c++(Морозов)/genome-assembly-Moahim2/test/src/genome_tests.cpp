#include "genome.h"

#include "gtest/gtest.h"

#include <fstream>
#include <iostream>
#include <string>

namespace genome
{

TEST(GenomeTest, it_works_when_k_0)
{
    // NB! if k == 0 -> always return empty string
    EXPECT_EQ("", assembly(0, {}));
    EXPECT_EQ("", assembly(0, {"AATCT", "ACGAA", "GCTAC"}));
}

TEST(GenomeTest, it_works_when_n_0)
{
    // NB! if no fragments -> always return empty string
    EXPECT_EQ("", assembly(2, {}));
    EXPECT_EQ("", assembly(10, {}));
}

TEST(GenomeTest, it_works_when_k_1_d_2)
{
    EXPECT_EQ("TTG", assembly(1, {"TT", "TG"}));
    EXPECT_EQ("ACG", assembly(1, {"CG", "AC"}));
    EXPECT_EQ("GAT", assembly(1, {"GA", "AT"}));
    EXPECT_EQ("CAT", assembly(1, {"AT", "CA"}));
}

TEST(GenomeTest, it_works_when_n_1)
{
    EXPECT_EQ("TTG", assembly(1, {"TTG"}));
    EXPECT_EQ("CAT", assembly(1, {"CAT"}));
    EXPECT_EQ("AATGTCC", assembly(2, {"AATGTCC"}));
    EXPECT_EQ("AGCGCTACTGGACTACCCC", assembly(3, {"AGCGCTACTGGACTACCCC"}));
    EXPECT_EQ("AGCGCTACTGGACTACCCC", assembly(4, {"AGCGCTACTGGACTACCCC"}));
    EXPECT_EQ("AGCGCTACTGGACTACCCC", assembly(5, {"AGCGCTACTGGACTACCCC"}));
}

TEST(GenomeTest, it_works_when_k_overlaps_half_d)
{
    EXPECT_EQ("TAGAACT", assembly(3, {"TAGAA", "GAACT"}));
    EXPECT_EQ("TTTCTGG", assembly(3, {"TTTCT", "TCTGG"}));
    EXPECT_EQ("TTATTTTAAA", assembly(4, {"TTATTTT", "TTTTAAA"}));
    EXPECT_EQ("CTTATTACTT", assembly(4, {"TTACTT", "CTTATT", "TATTAC"}));
    EXPECT_EQ("TGCCAAGGGGGCT", assembly(5, {"AAGGGGG", "TGCCAAG", "GGGGGCT", "CCAAGGG"}));
}

TEST(GenomeTest, it_works_when_fragments_are_big)
{
    EXPECT_EQ(
        "CGTCTATGCAGGGTAACCCCTTAGGATACGAATGGCTGTCCACGTGGACAACGCGCCCTGGAGTGGTTGCCTACTTGAACTATAT",
        assembly(5, {
                        "CACGTGGACAACGCGCCCTGGAGTG",
                        "CGTCTATGCAGGGTAACCCCTTAGG",
                        "GAGTGGTTGCCTACTTGAACTATAT",
                        "TTAGGATACGAATGGCTGTCCACGT",
                    }));
    EXPECT_EQ(
        "CAGGAGTAGAGTCAAGCGCGGGCCACAGGCCGGGATCCAAAGCTAATACAGTACTCTGGGAAAACGAAGAGTAAGCCGGCATCTA"
        "CCCCCCGGCCTGTACGTTTAGGTCGTGCTCTTGAACTTCACATAAGTCGAGGTAAGAGCG",
        assembly(7, {
                        "CACAGGCCGGGATCCAAAGCTAATACAGTA",
                        "TTGAACTTCACATAAGTCGAGGTAAGAGCG",
                        "TACAGTACTCTGGGAAAACGAAGAGTAAGC",
                        "CAGGAGTAGAGTCAAGCGCGGGCCACAGGC",
                        "GCCTGTACGTTTAGGTCGTGCTCTTGAACT",
                        "AGTAAGCCGGCATCTACCCCCCGGCCTGTA",
                    }));
}

TEST(GenomeTest, it_works_when_k_1_d_3)
{
    EXPECT_EQ("AATCC", assembly(1, {"AAT", "TCC"}));
    EXPECT_EQ("AATGTCC", assembly(1, {"AAT", "TCC", "TGT"}));
    EXPECT_EQ("CCAAGTG", assembly(1, {"GTG", "CCA", "AAG"}));
}

TEST(GenomeTest, it_works_when_k_2_d_5_n_3)
{
    EXPECT_EQ("GCTACGAATCT", assembly(2, {"AATCT", "ACGAA", "GCTAC"}));
    EXPECT_EQ("CGGTTACTCGG", assembly(2, {"CTCGG", "CGGTT", "TTACT"}));
}

TEST(GenomeTest, it_works_when_k_3_d_7_n_4)
{
    EXPECT_EQ("AGCGCTACTGGACTACCCC", assembly(3, {"AGCGCTA", "CTACCCC", "CTACTGG", "TGGACTA"}));
    EXPECT_EQ("GTGCCTGAGCGGCGTTAAG", assembly(3, {"GCGGCGT", "GTGCCTG", "CGTTAAG", "CTGAGCG"}));
    EXPECT_EQ("CGATGTAAGCTATGCAGTT", assembly(3, {"GCTATGC", "CGATGTA", "TGCAGTT", "GTAAGCT"}));
    EXPECT_EQ("CATACGGCGATCGGTTGCT", assembly(3, {"GATCGGT", "CATACGG", "GGTTGCT", "CGGCGAT"}));
}

TEST(GenomeTest, it_works_when_k_5_d_9_n_17)
{
    EXPECT_EQ(
        "ACTGGTGCAAGAGCGTCCCACGTCTGTAATATTCAGAGGCTGGGCCGAAAATCTCCTAACTGACGATACTTAA",
        assembly(5, {"AAATCTCCT",
                     "AAGAGCGTC",
                     "AGGCTGGGC",
                     "TGGGCCGAA",
                     "CGTCTGTAA",
                     "TCAGAGGCT",
                     "TGTAATATT",
                     "ATATTCAGA",
                     "GTGCAAGAG",
                     "TGACGATAC",
                     "TAACTGACG",
                     "CCCACGTCT",
                     "CCGAAAATC",
                     "GCGTCCCAC",
                     "GATACTTAA",
                     "CTCCTAACT",
                     "ACTGGTGCA"}));
}

namespace
{
std::string genome_from_file(const char *name)
{
    std::string genom;
    std::ifstream file;
    file.open(name);
    std::getline(file, genom);
    file.close();
    return genom;
}

std::vector<std::string> reads_from_file(const char *name)
{
    std::vector<std::string> reads;
    std::ifstream file;
    file.open(name);
    for (std::string line; std::getline(file, line);)
    {
        reads.emplace_back(std::move(line));
    }
    file.close();
    return reads;
}
} // namespace

TEST(GenomeTest, it_works_when_input_is_big)
{
    EXPECT_EQ(
        genome_from_file("test/etc/big_genome.txt"),
        assembly(10, reads_from_file("test/etc/big_reads.txt")));
}

TEST(GenomeTest, it_works_when_input_is_bigger)
{
    EXPECT_EQ(
        genome_from_file("test/etc/bigger_genome.txt"),
        assembly(25, reads_from_file("test/etc/bigger_reads.txt")));
}

} // namespace genome

int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
