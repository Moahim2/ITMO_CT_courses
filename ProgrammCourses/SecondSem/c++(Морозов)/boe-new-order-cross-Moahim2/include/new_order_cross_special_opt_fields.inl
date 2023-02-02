#ifndef FIELD
#  error You need to define FIELD macro
#else
FIELD(symbol, 1, 1)
#undef FIELD
#endif