#include "ScapegoatTree.h"

#include <gtest/gtest.h>

/// Modification specific tests

TEST(ScapegoatTreeTest, scapegoat_tree_specific_alpha)
{
    double alpha = 0.75;
    ScapegoatTree tree(alpha);
}

TEST(ScapegoatTreeTest, scapegoat_tree_invalid_alpha)
{
    ASSERT_THROW(ScapegoatTree(0), std::invalid_argument);
}
