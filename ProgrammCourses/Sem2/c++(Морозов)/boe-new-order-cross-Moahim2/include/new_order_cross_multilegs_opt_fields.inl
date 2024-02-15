#ifndef FIELD
#  error You need to define FIELD macro
#else
FIELD(leg_position_effects, 3, 16)
FIELD(algorithmic_indicator, 1, 64)
#undef FIELD
#endif