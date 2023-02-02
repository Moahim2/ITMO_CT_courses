#ifndef FIELD
#  error You need to define FIELD macro
#else
FIELD(price, 1, 4)
FIELD(ord_type, 1, 16)
FIELD(time_in_force, 1, 32)
FIELD(max_floor, 1, 128)
FIELD(symbol, 2, 1)
FIELD(capacity, 2, 64)
FIELD(account, 3, 1)
#undef FIELD
#endif
